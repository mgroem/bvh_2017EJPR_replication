############################################################
## Replication: Birch & Van Ham (EJPR, 2017), 
## Table 1 (2), and Table 2 (Models 1, 2, and 3)
## 
############################################################

# !diagnostics off

rm(list=ls())
options(scipen=999)

# override table so that it always uses "NA"
table <- function(..., useNA = "always") {
  base::table(..., useNA = useNA)
}


options(scipen=999)
options(java.parameters = "-Xmx4g")

library(vdemdata)
library(rqog)
library(WDI)
library(tidyverse)
library(janitor)
library(countrycode)
library(plm)
library(sandwich)
library(lmtest)
library(modelsummary)
library(readr)
library(readxl)
library(curl)
library(sandwich)
library(plm)
library(officer)
library(flextable)





# READ DATA ---------------------------------------------------------------

# DEM
dem <- read.csv("data/demi_long_v1.csv")

## V-Dem
vdem <- vdem
vdem$ISO <- vdem$country_text_id
vdem$country_text_id <- NULL
vdem <- vdem 

## QoG 
qog <- read_qog(which_data = "standard", data_type = "time-series")
qog$ISO <- qog$ccodealp

## NELDA 
nelda_url  <- "https://www.dropbox.com/scl/fi/f9tqjn2d46ehekrphjvq8/NELDA.xls?rlkey=ceu5l116m0c3fed63pgl3600i&dl=1"
nelda_path <- file.path(tempdir(), "NELDA.xls")
curl::curl_download(url = nelda_url, destfile = nelda_path, quiet = FALSE)
nelda <- readxl::read_xls(nelda_path)

## WDI
wdi_indicators <- c(
  gdp_pc     = "NY.GDP.PCAP.CD",     # GDP per capita (current US$)
  gdp_growth = "NY.GDP.PCAP.KD.ZG"   # GDP per capita growth (%)
)
wdi <- WDI(
  country   = "all",
  indicator = wdi_indicators,
  start     = 1990,
  end       = 2012,
  extra     = FALSE
)


## IDEA 2006 electoral management model (governmental/mixed/independent)
idea <- readxl::read_xlsx(
  "data/idea_export_electoral_management_design_database_region.xlsx"
)


## CLEAN AND HARMONISE -----------------------------------------------------

# vdem
vdem <- vdem %>%
  mutate(
    ISO  = as.character(ISO),
    year = as.integer(year)
  )

# qog
qog <- qog %>%
  mutate(
    ISO  = as.character(ISO),
    year = as.integer(year)
  )

# nelda
nelda <- nelda %>%
  mutate(
    ISO = countrycode(
      sourcevar = ccode,
      origin    = "cown",
      destination = "iso3c"
    ),
    year = as.integer(year)
  )

nelda <- nelda %>%
  mutate(
    ISO = case_when(
      # Germany: drop pre-unification composite
      ccode == 260 ~ "DEU",
      
      # GDR: drop
      ccode == 265 ~ NA_character_,
      
      # Czechoslovakia: drop (pre-1993 only)
      ccode == 315 ~ NA_character_,
      
      # China (PRC)
      ccode %in% c(340, 345) ~ "CHN",
      
      # Taiwan (BVH exclude)
      ccode == 347 ~ NA_character_,
      
      # Zanzibar (exclude)
      ccode == 396 ~ NA_character_,
      
      # Tanzania (use ISO)
      ccode == 397 ~ "TZA",
      
      # Yemen (post-1990 unified)
      ccode %in% c(678, 680) ~ "YEM",
      
      # Soviet Union (exclude)
      ccode == 817 ~ NA_character_,
      
      # Serbia & Montenegro → Serbia
      ccode %in% c(971, 972) ~ "SRB",
      
      # Montenegro (post-2006 only, BVH exclude)
      ccode == 973 ~ NA_character_,
      
      # Everything else: use countrycode result
      TRUE ~ ISO
    )
  )

nelda <- nelda %>%
  filter(!is.na(ISO))






# wdi
wdi <- wdi %>%
  mutate(
    ISO  = as.character(iso3c),
    year = as.integer(year)
  ) %>%
  select(ISO, year, gdp_pc, gdp_growth)

# idea
idea <- idea %>%
  mutate(
    ISO = as.character(ISO3)
  ) %>%
  select(
    ISO,
    `Electoral Management Design Database - Model of electoral management`
  )

# Check ISO format
lapply(list(vdem, qog, nelda, wdi, idea), function(x) table(nchar(x$ISO)))

# Check year ranges
range(vdem$year, na.rm = TRUE)
range(qog$year,  na.rm = TRUE)
range(nelda$year, na.rm = TRUE)
range(wdi$year,  na.rm = TRUE)





# SUBSET AND CREATE VARIABLES --------------------------------------------------------

# nelda    (keep elections as unit, following B & vH)
nelda_elec <- nelda %>%
  
  # Restrict to analysis period
  filter(year >= 1990, year <= 2012) %>%
  
  # Keep only needed variables
  select(
    ISO,
    year,
    electionid,
    types,
    nelda45
  ) %>%
  
  # Drop Constituent Assembly elections (BVH scope)
  filter(types != "Constituent Assembly") %>%
  
  # Election-level recodes
  mutate(
    # International observers (election-level)
    intobs = case_when(
      nelda45 == "yes" ~ 1,
      nelda45 == "no"  ~ 0,
      TRUE             ~ NA_real_
    ),
    
    # Election type flags (election-level)
    leg  = ifelse(types == "Legislative/Parliamentary", 1, 0),
    exec = ifelse(types == "Executive", 1, 0)
  )

nelda_year_flags <- nelda_elec %>%    # identify concurrent elections
  group_by(ISO, year) %>%
  summarise(
    has_leg  = max(leg,  na.rm = TRUE),
    has_exec = max(exec, na.rm = TRUE),
    .groups = "drop"
  )

nelda_elec <- nelda_elec %>%     # merge back and retain election-year as unit of analysis
  left_join(
    nelda_year_flags,
    by = c("ISO", "year")
  ) %>%
  
  # Construct BVH election-type coding
  mutate(
    election_type = case_when(
      has_leg == 1 & has_exec == 1 ~ "Concurrent",
      exec == 1 & has_leg == 0     ~ "Executive",
      leg  == 1 & has_exec == 0    ~ "Legislative",
      TRUE                         ~ NA_character_
    ),
    election_type = factor(
      election_type,
      levels = c("Legislative", "Executive", "Concurrent")
    ),
    
    # BVH dummies (Legislative is baseline)
    type_exec = ifelse(election_type == "Executive", 1, 0),
    type_conc = ifelse(election_type == "Concurrent", 1, 0)
  )

# Election-type distribution
table(nelda_elec$election_type, useNA = "always")




# vdem    (keep election-years  [but unit is country-year combination, not el;ection event])

# ------------------------------------------------------------
# V-Dem: minimal preprocessing for merger with NELDA elections
# ------------------------------------------------------------

vdem <- vdem %>%
  
  # Restrict to analysis period
  filter(year >= 1990, year <= 2012) %>%
  
  # Keep only required variables
  select(
    ISO,
    year,
    v2x_regime,
    
    # Election integrity components (ordinal)
    v2elvotbuy_ord,
    v2elintim_ord,
    v2elpeace_ord,
    v2elfrfair_ord,
    
    # EMB characteristics (ordinal)
    v2elembaut_ord,
    v2elembcap_ord,
    
    # Domestic observers
    v2eldommon
  ) %>%
  
  # Construct election integrity DV
  # (will be NA outside election years automatically)
  mutate(
    electionintegrity = rowMeans(
      cbind(
        v2elvotbuy_ord,
        v2elintim_ord,
        v2elpeace_ord,
        v2elfrfair_ord
      ),
      na.rm = FALSE
    )
  )






# wdi
wdi <- wdi %>%
  filter(year >= 1990, year <= 2012) %>%
  mutate(
    gdp_pc_log = log(gdp_pc)
  ) %>%
  select(
    ISO,
    year,
    gdp_pc_log,
    gdp_growth
  )

wdi <- wdi %>%   # Drop rows with missing or empty ISO codes:
  filter(!is.na(ISO), ISO != "")


# idea
years <- 1990:2012
idea <- idea %>%    # expand to country–year format
  tidyr::expand_grid(year = years)

idea <- idea %>%
  mutate(
    emb_ind_idea = case_when(
      `Electoral Management Design Database - Model of electoral management` == "Independent" ~ 1,
      `Electoral Management Design Database - Model of electoral management` %in% c("Governmental", "Mixed") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    ISO,
    year,
    emb_ind_idea
  )

idea <- idea %>%
  group_by(ISO) %>%
  mutate(
    emb_ind_idea = ifelse(
      all(is.na(emb_ind_idea)),
      NA_real_,
      as.numeric(names(sort(table(emb_ind_idea), decreasing = TRUE)[1]))
    )
  ) %>%
  ungroup()

idea <- idea %>%   # drop missing ISOs
  filter(!is.na(ISO))



# qog
qog <- qog %>%      # region variable is now ten regions, was 7 regions at time of Birch & van Ham paper
  mutate(
    ht_region7 = case_when(
      ht_region == 1 ~ 1,  # Western Europe & North America
      ht_region == 2 ~ 2,  # Eastern Europe & Central Asia
      ht_region == 3 ~ 3,  # Latin America
      ht_region == 4 ~ 4,  # Sub-Saharan Africa
      ht_region == 5 ~ 5,  # Middle East & North Africa
      ht_region == 6 ~ 6,  # South Asia
      ht_region %in% c(7, 8, 9, 10) ~ 7,  # East & Southeast Asia + Pacific
      TRUE ~ NA_real_
    )
  )

qog <- qog %>%      # recode majoritarian electoral system (yes/no)
  mutate(
    majoritarian = case_when(
      iaep_es %in% c(1, 2) ~ 1,
      iaep_es %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_
    )
  )

qog <- qog %>%
  mutate(
    ciri_assn = ifelse(ciri_assn %in% c(0, 1, 2), ciri_assn, NA_real_),
    ciri_injud = ifelse(ciri_injud %in% c(0, 1, 2), ciri_injud, NA_real_),
    ciri_speech = ifelse(ciri_speech %in% c(0, 1, 2), ciri_speech, NA_real_)   # bich & van ham call this ' independence of media'
  )

qog <- qog %>%
  filter(year >= 1990, year <= 2012)



# MERGE -------------------------------------------------------------------

############################################################
## MERGE DATASETS: START FROM NELDA ELECTION EVENTS
############################################################

df <- nelda_elec %>%
  
  ##########################################################
## 1. Merge V-Dem (election-year attributes)
##########################################################
left_join(
  vdem %>%
    select(
      ISO,
      year,
      v2x_regime,
      electionintegrity,
      v2elembaut_ord,
      v2elembcap_ord,
      v2eldommon
    ),
  by = c("ISO", "year")
) %>%
  
  ##########################################################
## 3. Restrict to multiparty regimes (BVH sample)
##########################################################
filter(v2x_regime > 0) %>%
  
  ##########################################################
## 4. Merge WDI (economic controls)
##########################################################
left_join(
  wdi,
  by = c("ISO", "year")
) %>%
  
  ##########################################################
## 5. Merge IDEA (de jure EMB independence)
##########################################################
left_join(
  idea,
  by = c("ISO", "year")
) %>%
  
  ##########################################################
## 6. Merge QoG (institutions, regions, electoral system)
##########################################################
left_join(
  qog %>%
    select(
      ISO = ccodealp,
      year,
      ht_region7,
      majoritarian,
      ciri_injud,
      ciri_assn,
      ciri_speech
    ),
  by = c("ISO", "year")
)

##########################################################
## 7. Merge DEMI (democracy / observer indicators)
##########################################################
df <- df %>%
  left_join(
    dem %>%
      mutate(
        ISO  = as.character(ISO),
        year = as.integer(year)
      ),
    by = c("ISO", "year")
  )



# Final structure check
nrow(df)
length(unique(df$ISO))
table(df$election_type)

# Key BVH comparison numbers
summary(df$electionintegrity)
summary(df$v2elembaut_ord)
summary(df$emb_ind_idea)

# Panel structure
table(table(df$ISO))





############################################################
## ANALYSIS UNIT + MODEL-SPECIFIC SAMPLE
## Birch & Van Ham (2017): one country-election-year observation
## used consistently for all FE models (Table 1 model 2; Table 2 models 1–3)
############################################################

library(dplyr)
library(forcats)

# Helper: first non-missing value within a country-year
first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

# ------------------------------------------------------------------
# 1) CONSTRUCT THE ANALYSIS UNIT EXACTLY ONCE
# ------------------------------------------------------------------
# Start from the merged election-event data `df`.
# Collapse to one observation per ISO-year without using an ad hoc priority rule.
# Election type is defined from the set of election events observed that year:
#   - Concurrent: both legislative and executive elections in the same year
#   - Executive: executive only
#   - Legislative: legislative only
#
# All substantive covariates used in the article are year-level, so for duplicated
# rows within ISO-year they should be identical after the merges; we keep the first
# non-missing value and check consistency below.

analysis_df <- df %>%
  mutate(
    exec_event = ifelse(types == "Executive", 1L, 0L),
    leg_event  = ifelse(types == "Legislative/Parliamentary", 1L, 0L)
  ) %>%
  group_by(ISO, year) %>%
  summarise(
    # diagnostics on the raw event structure
    n_event_rows = n(),
    any_exec = max(exec_event, na.rm = TRUE),
    any_leg  = max(leg_event,  na.rm = TRUE),
    
    # election-type coding used in the paper (Legislative reference)
    election_type = case_when(
      any_exec == 1L & any_leg == 1L ~ "Concurrent",
      any_exec == 1L & any_leg == 0L ~ "Executive",
      any_exec == 0L & any_leg == 1L ~ "Legislative",
      TRUE                           ~ NA_character_
    ),
    
    type_exec = ifelse(election_type == "Executive", 1, 0),
    type_conc = ifelse(election_type == "Concurrent", 1, 0),
    
    # dependent variable and regressors: keep one country-year value
    electionintegrity = first_nonmissing(electionintegrity),
    v2elembaut_ord    = first_nonmissing(v2elembaut_ord),
    v2elembcap_ord    = first_nonmissing(v2elembcap_ord),
    v2eldommon        = first_nonmissing(v2eldommon),
    intobs            = first_nonmissing(intobs),
    gdp_growth        = first_nonmissing(gdp_growth),
    gdp_pc_log        = first_nonmissing(gdp_pc_log),
    ciri_injud        = first_nonmissing(ciri_injud),
    ciri_speech       = first_nonmissing(ciri_speech),
    ciri_assn         = first_nonmissing(ciri_assn),
    v2x_regime        = first_nonmissing(v2x_regime),
    
    # optional fields retained for diagnostics / later models
    emb_ind_idea      = first_nonmissing(emb_ind_idea),
    majoritarian      = first_nonmissing(majoritarian),
    ht_region7        = first_nonmissing(ht_region7),
    
    .groups = "drop"
  ) %>%
  mutate(
    election_type = factor(
      election_type,
      levels = c("Legislative", "Executive", "Concurrent")
    )
  )

# Basic construction diagnostics
cat("\n====================\nANALYSIS UNIT DIAGNOSTICS\n====================\n")
cat("Rows in raw merged election-event data: ", nrow(df), "\n", sep = "")
cat("Rows in analysis_df (ISO-year):         ", nrow(analysis_df), "\n", sep = "")
cat("Countries in analysis_df:               ", dplyr::n_distinct(analysis_df$ISO), "\n", sep = "")
cat("\nElection type distribution:\n")
print(table(analysis_df$election_type, useNA = "always"))

cat("\nCountry-years with multiple election-event rows before collapse:\n")
print(sum(analysis_df$n_event_rows > 1, na.rm = TRUE))

# ------------------------------------------------------------------
# Optional but useful: consistency checks
# ------------------------------------------------------------------
# These variables should ordinarily be constant within ISO-year after merges.
# If they are not, flag it before estimating anything.

consistency_check <- df %>%
  group_by(ISO, year) %>%
  summarise(
    n_electionintegrity = n_distinct(electionintegrity, na.rm = TRUE),
    n_v2elembaut_ord    = n_distinct(v2elembaut_ord,    na.rm = TRUE),
    n_v2elembcap_ord    = n_distinct(v2elembcap_ord,    na.rm = TRUE),
    n_v2eldommon        = n_distinct(v2eldommon,        na.rm = TRUE),
    n_intobs            = n_distinct(intobs,            na.rm = TRUE),
    n_gdp_growth        = n_distinct(gdp_growth,        na.rm = TRUE),
    n_gdp_pc_log        = n_distinct(gdp_pc_log,        na.rm = TRUE),
    n_ciri_injud        = n_distinct(ciri_injud,        na.rm = TRUE),
    n_ciri_speech       = n_distinct(ciri_speech,       na.rm = TRUE),
    n_ciri_assn         = n_distinct(ciri_assn,         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    n_electionintegrity > 1 | n_v2elembaut_ord > 1 | n_v2elembcap_ord > 1 |
      n_v2eldommon > 1 | n_intobs > 1 | n_gdp_growth > 1 | n_gdp_pc_log > 1 |
      n_ciri_injud > 1 | n_ciri_speech > 1 | n_ciri_assn > 1
  )

if (nrow(consistency_check) > 0) {
  cat("\nWARNING: Some ISO-years have inconsistent values across duplicated event rows.\n")
  print(head(consistency_check, 20))
} else {
  cat("\nNo within-ISO-year inconsistencies detected in model variables.\n")
}

# ------------------------------------------------------------------
# 2) APPLY MODEL-SPECIFIC LISTWISE DELETION TO THAT SAME DATASET
# ------------------------------------------------------------------
# Published Table 1 model 2 and Table 2 use FE, so de jure EMB independence is excluded.
# The displayed FE model includes:
#   de facto EMB independence
#   judiciary independence
#   media independence
#   civil society freedom
#   EMB capacity
#   international observers
#   domestic observers
#   GDP per capita growth
#   log GDP per capita
#   election type
#
# Region is only noted for model 1, and de jure EMB independence is dropped in FE.
# Majoritarian is not shown in Table 1 model 2 or Table 2, so it is not used here.

fe_vars <- c(
  "electionintegrity",
  "v2elembaut_ord",
  "ciri_injud",
  "ciri_speech",
  "ciri_assn",
  "v2elembcap_ord",
  "intobs",
  "v2eldommon",
  "gdp_growth",
  "gdp_pc_log",
  "election_type"
)

analysis_fe <- analysis_df %>%
  filter(
    year >= 1990, year <= 2012,
    v2x_regime > 0
  ) %>%
  filter(if_all(all_of(fe_vars), ~ !is.na(.))) %>%
  droplevels()

cat("\n====================\nFE ESTIMATION SAMPLE\n====================\n")
cat("N (elections / ISO-years): ", nrow(analysis_fe), "\n", sep = "")
cat("N (countries):             ", dplyr::n_distinct(analysis_fe$ISO), "\n", sep = "")
cat("\nElection type distribution in FE sample:\n")
print(table(analysis_fe$election_type, useNA = "always"))

# ------------------------------------------------------------------
# 3) VERIFY AGAINST THE PUBLISHED TARGET
# ------------------------------------------------------------------
target_n_elections <- 847L
target_n_countries <- 146L

actual_n_elections <- nrow(analysis_fe)
actual_n_countries <- dplyr::n_distinct(analysis_fe$ISO)

cat("\n====================\nTARGET CHECK\n====================\n")
cat("Published target: 847 elections, 146 countries\n")
cat("Your sample:      ", actual_n_elections, " elections, ", actual_n_countries, " countries\n", sep = "")

if (actual_n_elections == target_n_elections && actual_n_countries == target_n_countries) {
  cat("MATCH: sample size matches Birch & Van Ham (2017).\n")
} else {
  cat("NO MATCH: sample size does not match Birch & Van Ham (2017).\n")
  cat("Difference: ",
      actual_n_elections - target_n_elections, " elections; ",
      actual_n_countries - target_n_countries, " countries\n", sep = "")
}

# This is the only dataset you should pass to pdata.frame / plm for Table 1 model 2 and Table 2.
pdata_fe <- pdata.frame(analysis_fe, index = c("ISO", "year"))





# SOME MORE SAMPLE FORENSICS ----------------------------------------------

# Countries and # of elections per country in the FE sample
analysis_fe %>%
  count(ISO, sort = TRUE) %>%
  arrange(ISO) %>%
  print(n = Inf)


country_counts <- analysis_fe %>%
  count(ISO, name = "n_elections")

table(country_counts$n_elections)

country_counts %>%
  filter(n_elections == 1) %>%
  arrange(ISO)

fe_keys <- analysis_fe %>%
  distinct(ISO, year) %>%
  arrange(ISO, year)

print(fe_keys, n = Inf)


vars_to_check <- c(
  "electionintegrity",
  "v2elembaut_ord",
  "ciri_injud",
  "ciri_speech",
  "ciri_assn",
  "v2elembcap_ord",
  "intobs",
  "v2eldommon",
  "gdp_growth",
  "gdp_pc_log",
  "election_type"
)

loss_table <- lapply(vars_to_check, function(v) {
  data.frame(
    variable = v,
    n_missing = sum(is.na(analysis_df[[v]])),
    n_nonmissing = sum(!is.na(analysis_df[[v]]))
  )
}) %>%
  bind_rows()

loss_table


tmp <- analysis_df %>%
  filter(year >= 1990, year <= 2012, v2x_regime > 0)

track <- data.frame(
  step = "start: regime-filtered",
  n_elections = nrow(tmp),
  n_countries = n_distinct(tmp$ISO)
)

for (v in fe_vars) {
  tmp <- tmp %>% filter(!is.na(.data[[v]]))
  track <- bind_rows(track, data.frame(
    step = paste("drop missing", v),
    n_elections = nrow(tmp),
    n_countries = n_distinct(tmp$ISO)
  ))
}

track


nelda %>%
  count(nelda45, sort = TRUE)


analysis_df_alt <- df %>%
  mutate(
    exec_event = ifelse(types == "Executive", 1L, 0L),
    leg_event  = ifelse(types == "Legislative/Parliamentary", 1L, 0L)
  ) %>%
  group_by(ISO, year) %>%
  summarise(
    n_event_rows = n(),
    any_exec = max(exec_event, na.rm = TRUE),
    any_leg  = max(leg_event,  na.rm = TRUE),
    election_type = case_when(
      any_exec == 1L & any_leg == 1L ~ "Concurrent",
      any_exec == 1L & any_leg == 0L ~ "Executive",
      any_exec == 0L & any_leg == 1L ~ "Legislative",
      TRUE                           ~ NA_character_
    ),
    electionintegrity = first_nonmissing(electionintegrity),
    v2elembaut_ord    = first_nonmissing(v2elembaut_ord),
    v2elembcap_ord    = first_nonmissing(v2elembcap_ord),
    
    # changed rule here
    intobs     = ifelse(all(is.na(intobs)), NA, max(intobs, na.rm = TRUE)),
    v2eldommon = ifelse(all(is.na(v2eldommon)), NA, max(v2eldommon, na.rm = TRUE)),
    
    gdp_growth  = first_nonmissing(gdp_growth),
    gdp_pc_log  = first_nonmissing(gdp_pc_log),
    ciri_injud  = first_nonmissing(ciri_injud),
    ciri_speech = first_nonmissing(ciri_speech),
    ciri_assn   = first_nonmissing(ciri_assn),
    v2x_regime  = first_nonmissing(v2x_regime),
    .groups = "drop"
  ) %>%
  mutate(
    election_type = factor(election_type, levels = c("Legislative", "Executive", "Concurrent"))
  )

analysis_fe_alt <- analysis_df_alt %>%
  filter(year >= 1990, year <= 2012, v2x_regime > 0) %>%
  filter(if_all(all_of(fe_vars), ~ !is.na(.)))

c(
  n_elections = nrow(analysis_fe_alt),
  n_countries = n_distinct(analysis_fe_alt$ISO)
)



audit_df <- analysis_df %>%
  mutate(
    keep_regime = v2x_regime > 0,
    miss_electionintegrity = is.na(electionintegrity),
    miss_v2elembaut_ord    = is.na(v2elembaut_ord),
    miss_ciri_injud        = is.na(ciri_injud),
    miss_ciri_speech       = is.na(ciri_speech),
    miss_ciri_assn         = is.na(ciri_assn),
    miss_v2elembcap_ord    = is.na(v2elembcap_ord),
    miss_intobs            = is.na(intobs),
    miss_v2eldommon        = is.na(v2eldommon),
    miss_gdp_growth        = is.na(gdp_growth),
    miss_gdp_pc_log        = is.na(gdp_pc_log),
    miss_election_type     = is.na(election_type)
  ) %>%
  mutate(
    dropped_fe = !(
      keep_regime &
        !miss_electionintegrity &
        !miss_v2elembaut_ord &
        !miss_ciri_injud &
        !miss_ciri_speech &
        !miss_ciri_assn &
        !miss_v2elembcap_ord &
        !miss_intobs &
        !miss_v2eldommon &
        !miss_gdp_growth &
        !miss_gdp_pc_log &
        !miss_election_type
    )
  )

audit_df %>%
  filter(dropped_fe) %>%
  arrange(ISO, year) %>%
  print(n = Inf)


analysis_fe_nomne <- analysis_fe %>%
  filter(!ISO %in% c("BTN", "TLS", "MNE"))

c(
  n_elections = nrow(analysis_fe_nomne),
  n_countries = dplyr::n_distinct(analysis_fe_nomne$ISO)
)


analysis_fe %>%
  filter(ISO %in% c("BTN", "TLS", "MNE")) %>%
  arrange(ISO, year)

analysis_fe %>%
  count(ISO, sort = TRUE) %>%
  filter(n >= 9) %>%
  arrange(desc(n), ISO) %>%
  print(n = Inf)

dup_obs <- df %>%
  group_by(ISO, year) %>%
  summarise(
    n_rows = n(),
    n_intobs = dplyr::n_distinct(intobs, na.rm = TRUE),
    n_domobs = dplyr::n_distinct(v2eldommon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_rows > 1 & (n_intobs > 1 | n_domobs > 1))

dup_obs %>% arrange(ISO, year)

# -> Decision to exlcude three suspicious countries BTN, TLS, and MNE

analysis_fe_final <- analysis_fe %>%
  filter(!ISO %in% c("BTN", "TLS", "MNE"))



# COMPARE DESCRIPTIVES TO BIRCH & VAN HAM APPENDIX TABLE 1.1 -------------
# Recommended comparison sample: analysis_df (collapsed country-year sample)
# Not analysis_fe_final, because Appendix Table 1.1 is for the broader descriptive sample.

library(dplyr)
library(tibble)
library(flextable)
library(officer)

# Choose comparison dataset:
#   analysis_df       = best match to Appendix Table 1.1
#   analysis_fe_final = FE estimation sample after exclusions
desc_data <- analysis_df

# Helper
desc_stats <- function(x) {
  x_nonmiss <- x[!is.na(x)]
  c(
    N    = length(x_nonmiss),
    Mean = if (length(x_nonmiss) == 0) NA_real_ else mean(x_nonmiss),
    Min  = if (length(x_nonmiss) == 0) NA_real_ else min(x_nonmiss),
    Max  = if (length(x_nonmiss) == 0) NA_real_ else max(x_nonmiss)
  )
}

# Recode type of election to match appendix coding:
# Legislative = 1, Executive = 2, Concurrent = 3
type_num <- dplyr::case_when(
  desc_data$election_type == "Legislative" ~ 1,
  desc_data$election_type == "Executive"   ~ 2,
  desc_data$election_type == "Concurrent"  ~ 3,
  TRUE ~ NA_real_
)

# Your descriptives
my_descriptives <- rbind(
  "Election integrity"                        = desc_stats(desc_data$electionintegrity),
  "De jure EMB independence"                  = desc_stats(desc_data$emb_ind_idea),
  "De facto EMB independence"                 = desc_stats(desc_data$v2elembaut_ord),
  "Independence of judiciary"                 = desc_stats(desc_data$ciri_injud),
  "Independence of media"                     = desc_stats(desc_data$ciri_speech),
  "Civil society freedom"                     = desc_stats(desc_data$ciri_assn),
  "EMB capacity"                              = desc_stats(desc_data$v2elembcap_ord),
  "International observers present?"          = desc_stats(desc_data$intobs),
  "Domestic observers present?"               = desc_stats(desc_data$v2eldommon),
  "GDP per capita annual growth %"            = desc_stats(desc_data$gdp_growth),
  "Real GDP per capita log (current USD)"     = desc_stats(desc_data$gdp_pc_log),
  "Legislative electoral system majoritarian" = desc_stats(desc_data$majoritarian),
  "Type of election"                          = desc_stats(type_num),
  "Region"                                    = desc_stats(desc_data$ht_region7)
) %>%
  as.data.frame() %>%
  rownames_to_column("Variable")

# Birch & Van Ham Appendix Table 1.1
bvh_descriptives <- tibble(
  Variable = c(
    "Election integrity",
    "De jure EMB independence",
    "De facto EMB independence",
    "Independence of judiciary",
    "Independence of media",
    "Civil society freedom",
    "EMB capacity",
    "International observers present?",
    "Domestic observers present?",
    "GDP per capita annual growth %",
    "Real GDP per capita log (current USD)",
    "Legislative electoral system majoritarian",
    "Type of election",
    "Region"
  ),
  BVH_N    = c(1045, 1033, 1045, 949, 913, 912, 1045, 1045, 1007, 994, 1012, 1045, 1045, 1045),
  BVH_Mean = c(2.61, 0.66, 2.46, 1.13, 1.11, 1.33, 2.77, 0.52, 0.91, 2.02, 7.88, 0.43, 1.68, 3.42),
  BVH_Min  = c(0.39, 0, 0.07, 0, 0, 0, 0.24, 0, 0, -45.33, 4.80, 0, 1, 1),
  BVH_Max  = c(3.97, 1, 3.97, 2, 2, 2, 4.00, 1, 1, 104.66, 11.39, 1, 3, 7)
)

# Merge and compute differences
comparison_df <- bvh_descriptives %>%
  left_join(my_descriptives, by = "Variable") %>%
  rename(
    Your_N    = N,
    Your_Mean = Mean,
    Your_Min  = Min,
    Your_Max  = Max
  ) %>%
  mutate(
    Diff_N    = Your_N    - BVH_N,
    Diff_Mean = Your_Mean - BVH_Mean,
    Diff_Min  = Your_Min  - BVH_Min,
    Diff_Max  = Your_Max  - BVH_Max
  )

# Print rounded comparison to console
comparison_print <- comparison_df %>%
  mutate(across(-Variable, ~ round(., 2)))

print(comparison_print, n = Inf)

# Export to Word
ft <- flextable(comparison_print) |>
  autofit()

save_as_docx(
  "Descriptives vs Birch & Van Ham Appendix Table 1.1" = ft,
  path = "results/descriptives_bvh_appendix_table1_1.docx"
)

# My descriptives track Birch and Van Ham’s Appendix Table 1.1 closely for the main institutional covariates and election type, 
# but differ more noticeably for election integrity, EMB capacity, international observers, and majoritarian electoral system, 
# likely reflecting differences in data vintage and some remaining harmonisation differences in observer and electoral-system coding.






############################################################
## TABLE 1, MODEL 2 — BIRCH & VAN HAM (2017)
## Fixed effects replication
############################################################

library(plm)
library(lmtest)
library(sandwich)
library(modelsummary)
library(dplyr)
library(tibble)

# 0) Use the final FE replication sample

tab1_m2_data <- analysis_fe_final %>%
  mutate(
    election_type = factor(
      election_type,
      levels = c("Legislative", "Executive", "Concurrent")
    )
  )

cat("Replication sample:\n")
cat("N elections:", nrow(tab1_m2_data), "\n")
cat("N countries:", dplyr::n_distinct(tab1_m2_data$ISO), "\n")

# 1) Declare panel structure

pdata_tab1_m2 <- pdata.frame(tab1_m2_data, index = c("ISO", "year"))

# 2) Estimate Table 1, Model 2

m2_fe <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembcap_ord +
    intobs +
    v2eldommon +
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata_tab1_m2,
  model  = "within",
  effect = "individual"
)

summary(m2_fe)

# 3) Standard errors

m2_fe_default <- coeftest(m2_fe)

vcov_m2_cluster <- vcovHC(
  m2_fe,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

m2_fe_cluster <- coeftest(m2_fe, vcov. = vcov_m2_cluster)

print(m2_fe_default)
print(m2_fe_cluster)

# 4) Hausman check against RE

m2_re <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembcap_ord +
    intobs +
    v2eldommon +
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata_tab1_m2,
  model  = "random",
  effect = "individual"
)

hausman_m2 <- phtest(m2_fe, m2_re)
print(hausman_m2)






# 5) Export side-by-side table: published vs replication
# Put coefficient, stars, and SE in the same cell

library(dplyr)
library(tibble)
library(lmtest)
library(flextable)
library(officer)

bvh_tab1_m2 <- tribble(
  ~term, ~`B&VH 2017`,
  "De facto EMB independence (0–4)",          "0.319*** (0.025)",
  "Independence of judiciary (0–2)",          "0.041* (0.020)",
  "Independence of media (0–2)",              "0.066*** (0.019)",
  "Civil society freedom (0–2)",              "0.037* (0.019)",
  "EMB capacity (0–4)",                       "0.132*** (0.035)",
  "International election observers (0–1)",   "0.007 (0.023)",
  "Domestic election observers (0–1)",        "0.084+ (0.043)",
  "GDP per capita annual growth %",           "-0.006*** (0.001)",
  "Real GDP per capita log (current USD)",    "0.003 (0.019)",
  "Executive",                                "-0.022 (0.022)",
  "Concurrent",                               "-0.011 (0.027)",
  "R-squared",                                "0.686",
  "N (elections)",                            "847",
  "N (countries)",                            "146"
)

coef_map_tab1_m2 <- c(
  "v2elembaut_ord"          = "De facto EMB independence (0–4)",
  "ciri_injud"              = "Independence of judiciary (0–2)",
  "ciri_speech"             = "Independence of media (0–2)",
  "ciri_assn"               = "Civil society freedom (0–2)",
  "v2elembcap_ord"          = "EMB capacity (0–4)",
  "intobs"                  = "International election observers (0–1)",
  "v2eldommon"              = "Domestic election observers (0–1)",
  "gdp_growth"              = "GDP per capita annual growth %",
  "gdp_pc_log"              = "Real GDP per capita log (current USD)",
  "election_typeExecutive"  = "Executive",
  "election_typeConcurrent" = "Concurrent"
)

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.10)  return("+")
  return("")
}

ct <- coeftest(m2_fe, vcov. = vcov_m2_cluster)

replication_table <- tibble(
  raw_term   = rownames(ct),
  estimate   = ct[, 1],
  std_error  = ct[, 2],
  p_value    = ct[, 4]
) %>%
  mutate(
    term = coef_map_tab1_m2[raw_term],
    `Replication FE` = paste0(
      sprintf("%.3f", estimate),
      vapply(p_value, star_code, character(1)),
      " (",
      sprintf("%.3f", std_error),
      ")"
    )
  ) %>%
  select(term, `Replication FE`)

replication_gof <- tibble(
  term = c("R-squared", "N (elections)", "N (countries)"),
  `Replication FE` = c(
    sprintf("%.3f", summary(m2_fe)$r.squared["rsq"]),
    as.character(nobs(m2_fe)),
    as.character(dplyr::n_distinct(tab1_m2_data$ISO))
  )
)

table_side_by_side <- bind_rows(
  bvh_tab1_m2 %>% filter(term %in% replication_table$term),
  bvh_tab1_m2 %>% filter(term %in% replication_gof$term)
) %>%
  left_join(bind_rows(replication_table, replication_gof), by = "term")

term_order <- c(
  "De facto EMB independence (0–4)",
  "Independence of judiciary (0–2)",
  "Independence of media (0–2)",
  "Civil society freedom (0–2)",
  "EMB capacity (0–4)",
  "International election observers (0–1)",
  "Domestic election observers (0–1)",
  "GDP per capita annual growth %",
  "Real GDP per capita log (current USD)",
  "Executive",
  "Concurrent",
  "R-squared",
  "N (elections)",
  "N (countries)"
)

table_side_by_side <- table_side_by_side %>%
  mutate(term = factor(term, levels = term_order)) %>%
  arrange(term) %>%
  mutate(term = as.character(term))

ft <- flextable(table_side_by_side) |>
  autofit()

save_as_docx(
  "Table 1, Model 2. Birch & Van Ham (2017) vs. Replication" = ft,
  path = "results/table1_model2_replication.docx"
)

# The Table 1, model 2 fixed-effects specification was implemented following Birch and Van Ham, and the Hausman test in the replication sample also favored fixed effects, but the substantive results do not replicate closely: although de facto EMB independence remains positive and significant, its estimated effect is substantially smaller than in the published article, and several other coefficients differ materially in magnitude, significance, and in some cases sign from the reported estimates.





############################################################
## TABLE 2 — BIRCH & VAN HAM (2017)
## Fixed effects interaction models
############################################################

library(plm)
library(lmtest)
library(sandwich)
library(dplyr)
library(tibble)
library(flextable)
library(officer)

# 0) Use the final FE replication sample

tab2_data <- analysis_fe_final %>%
  mutate(
    election_type = factor(
      election_type,
      levels = c("Legislative", "Executive", "Concurrent")
    )
  )

cat("Replication sample:\n")
cat("N elections:", nrow(tab2_data), "\n")
cat("N countries:", dplyr::n_distinct(tab2_data$ISO), "\n")

# 1) Declare panel structure

pdata_tab2 <- pdata.frame(tab2_data, index = c("ISO", "year"))

# 2) Estimate Table 2 models

m1_tab2 <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembaut_ord:ciri_injud +
    v2elembcap_ord +
    intobs +
    v2eldommon +
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata_tab2,
  model  = "within",
  effect = "individual"
)

m2_tab2 <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembaut_ord:ciri_speech +
    v2elembcap_ord +
    intobs +
    v2eldommon +
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata_tab2,
  model  = "within",
  effect = "individual"
)

m3_tab2 <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembaut_ord:ciri_assn +
    v2elembcap_ord +
    intobs +
    v2eldommon +
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata_tab2,
  model  = "within",
  effect = "individual"
)

summary(m1_tab2)
summary(m2_tab2)
summary(m3_tab2)

# 3) Clustered standard errors

vcov_m1_tab2 <- vcovHC(
  m1_tab2,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

vcov_m2_tab2 <- vcovHC(
  m2_tab2,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

vcov_m3_tab2 <- vcovHC(
  m3_tab2,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

ct_m1_tab2 <- coeftest(m1_tab2, vcov. = vcov_m1_tab2)
ct_m2_tab2 <- coeftest(m2_tab2, vcov. = vcov_m2_tab2)
ct_m3_tab2 <- coeftest(m3_tab2, vcov. = vcov_m3_tab2)

print(ct_m1_tab2)
print(ct_m2_tab2)
print(ct_m3_tab2)

# 4) Helper to format coef + stars + SE in one cell

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.10)  return("+")
  return("")
}

format_ct <- function(ct, coef_map, model_name, rsq, n_elections, n_countries) {
  coef_tbl <- tibble(
    raw_term   = rownames(ct),
    estimate   = ct[, 1],
    std_error  = ct[, 2],
    p_value    = ct[, 4]
  ) %>%
    mutate(
      term = coef_map[raw_term],
      value = paste0(
        sprintf("%.3f", estimate),
        vapply(p_value, star_code, character(1)),
        " (",
        sprintf("%.3f", std_error),
        ")"
      )
    ) %>%
    select(term, value)
  
  gof_tbl <- tibble(
    term = c("R-squared", "N (elections)", "N (countries)"),
    value = c(
      sprintf("%.3f", rsq),
      as.character(n_elections),
      as.character(n_countries)
    )
  )
  
  bind_rows(coef_tbl, gof_tbl) %>%
    rename(!!model_name := value)
}

# 5) Published Table 2 values from Birch & Van Ham (2017)

bvh_tab2 <- tribble(
  ~term, ~`B&VH 2017 M1`, ~`B&VH 2017 M2`, ~`B&VH 2017 M3`,
  "De facto EMB independence (0–4)",          "0.370*** (0.028)", "0.362*** (0.028)", "0.387*** (0.031)",
  "Independence of judiciary (0–2)",          "0.183*** (0.043)", "0.034+ (0.020)",   "0.042* (0.020)",
  "Independence of media (0–2)",              "0.060** (0.019)",  "0.193*** (0.042)", "0.065*** (0.019)",
  "Civil society freedom (0–2)",              "0.037* (0.018)",   "0.037* (0.018)",   "0.159*** (0.038)",
  "EMB independence * judiciary",             "-0.064*** (0.017)", "",                  "",
  "EMB independence * media",                 "",                  "-0.053*** (0.016)", "",
  "EMB independence * civil society",         "",                  "",                  "-0.058*** (0.016)",
  "EMB capacity (0–4)",                       "0.130*** (0.034)", "0.134*** (0.034)", "0.123*** (0.035)",
  "International election observers (0–1)",   "0.007 (0.023)",    "0.003 (0.023)",    "0.006 (0.023)",
  "Domestic election observers (0–1)",        "0.076+ (0.043)",   "0.093* (0.043)",   "0.077+ (0.043)",
  "GDP per capita annual growth %",           "-0.005*** (0.001)","-0.006*** (0.001)","-0.006*** (0.001)",
  "Real GDP per capita log (current USD)",    "0.003 (0.019)",    "0.002 (0.019)",    "0.002 (0.019)",
  "Executive",                                "-0.023 (0.022)",   "-0.021 (0.022)",   "-0.019 (0.022)",
  "Concurrent",                               "-0.013 (0.027)",   "-0.008 (0.027)",   "-0.007 (0.027)",
  "R-squared",                                "0.367",            "0.364",            "0.366",
  "N (elections)",                            "847",              "847",              "847",
  "N (countries)",                            "146",              "146",              "146"
)

# 6) Term maps for each model

coef_map_m1_tab2 <- c(
  "v2elembaut_ord"            = "De facto EMB independence (0–4)",
  "ciri_injud"                = "Independence of judiciary (0–2)",
  "ciri_speech"               = "Independence of media (0–2)",
  "ciri_assn"                 = "Civil society freedom (0–2)",
  "v2elembaut_ord:ciri_injud" = "EMB independence * judiciary",
  "v2elembcap_ord"            = "EMB capacity (0–4)",
  "intobs"                    = "International election observers (0–1)",
  "v2eldommon"                = "Domestic election observers (0–1)",
  "gdp_growth"                = "GDP per capita annual growth %",
  "gdp_pc_log"                = "Real GDP per capita log (current USD)",
  "election_typeExecutive"    = "Executive",
  "election_typeConcurrent"   = "Concurrent"
)

coef_map_m2_tab2 <- c(
  "v2elembaut_ord"             = "De facto EMB independence (0–4)",
  "ciri_injud"                 = "Independence of judiciary (0–2)",
  "ciri_speech"                = "Independence of media (0–2)",
  "ciri_assn"                  = "Civil society freedom (0–2)",
  "v2elembaut_ord:ciri_speech" = "EMB independence * media",
  "v2elembcap_ord"             = "EMB capacity (0–4)",
  "intobs"                     = "International election observers (0–1)",
  "v2eldommon"                 = "Domestic election observers (0–1)",
  "gdp_growth"                 = "GDP per capita annual growth %",
  "gdp_pc_log"                 = "Real GDP per capita log (current USD)",
  "election_typeExecutive"     = "Executive",
  "election_typeConcurrent"    = "Concurrent"
)

coef_map_m3_tab2 <- c(
  "v2elembaut_ord"           = "De facto EMB independence (0–4)",
  "ciri_injud"               = "Independence of judiciary (0–2)",
  "ciri_speech"              = "Independence of media (0–2)",
  "ciri_assn"                = "Civil society freedom (0–2)",
  "v2elembaut_ord:ciri_assn" = "EMB independence * civil society",
  "v2elembcap_ord"           = "EMB capacity (0–4)",
  "intobs"                   = "International election observers (0–1)",
  "v2eldommon"               = "Domestic election observers (0–1)",
  "gdp_growth"               = "GDP per capita annual growth %",
  "gdp_pc_log"               = "Real GDP per capita log (current USD)",
  "election_typeExecutive"   = "Executive",
  "election_typeConcurrent"  = "Concurrent"
)

# 7) Build replication columns

rep_m1_tab2 <- format_ct(
  ct = ct_m1_tab2,
  coef_map = coef_map_m1_tab2,
  model_name = "Replication M1",
  rsq = summary(m1_tab2)$r.squared["rsq"],
  n_elections = nobs(m1_tab2),
  n_countries = dplyr::n_distinct(tab2_data$ISO)
)

rep_m2_tab2 <- format_ct(
  ct = ct_m2_tab2,
  coef_map = coef_map_m2_tab2,
  model_name = "Replication M2",
  rsq = summary(m2_tab2)$r.squared["rsq"],
  n_elections = nobs(m2_tab2),
  n_countries = dplyr::n_distinct(tab2_data$ISO)
)

rep_m3_tab2 <- format_ct(
  ct = ct_m3_tab2,
  coef_map = coef_map_m3_tab2,
  model_name = "Replication M3",
  rsq = summary(m3_tab2)$r.squared["rsq"],
  n_elections = nobs(m3_tab2),
  n_countries = dplyr::n_distinct(tab2_data$ISO)
)

# 8) Merge into one side-by-side table

term_order <- c(
  "De facto EMB independence (0–4)",
  "Independence of judiciary (0–2)",
  "Independence of media (0–2)",
  "Civil society freedom (0–2)",
  "EMB independence * judiciary",
  "EMB independence * media",
  "EMB independence * civil society",
  "EMB capacity (0–4)",
  "International election observers (0–1)",
  "Domestic election observers (0–1)",
  "GDP per capita annual growth %",
  "Real GDP per capita log (current USD)",
  "Executive",
  "Concurrent",
  "R-squared",
  "N (elections)",
  "N (countries)"
)

table2_side_by_side <- bvh_tab2 %>%
  left_join(rep_m1_tab2, by = "term") %>%
  left_join(rep_m2_tab2, by = "term") %>%
  left_join(rep_m3_tab2, by = "term") %>%
  mutate(term = factor(term, levels = term_order)) %>%
  arrange(term) %>%
  mutate(term = as.character(term))

print(table2_side_by_side, n = Inf)

# 9) Export to Word

ft_tab2 <- flextable(table2_side_by_side) |>
  autofit()

save_as_docx(
  "Table 2. Birch & Van Ham (2017) vs. Replication" = ft_tab2,
  path = "results/table2_replication.docx"
)


# The Table 2 fixed-effects interaction specifications were implemented as in Birch and Van Ham, but the substantive results do not replicate in the harmonised dataset used here: all three interaction terms remain negative but lose statistical significance, and several core coefficients differ materially in magnitude and in some cases sign from the published estimates.

