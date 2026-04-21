############################################################
## Replication: Birch & Van Ham (EJPR, 2017), Table 1 (Models 1–2)
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
qog <- read_qog("standard", "time-series")
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
idea <- read_xlsx("data/idea_export_electoral_management_design_database_region.xlsx") 



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
## COLLAPSE RULE (BEST MATCH TO BIRCH & VAN HAM)
## Goal: Convert country–election–year (NELDA event-level) -> country–year
## while mimicking B&vH’s implicit “one election per country-year” logic.
##
## Key idea:
## 1) Prefer a CONCURRENT national election if one occurred in that country-year
##    (because it “contains” both executive + legislative contests).
## 2) If no concurrent election: prefer EXECUTIVE over LEGISLATIVE
##    (executive elections are typically more salient in cross-national integrity work,
##     and this rule also prevents double-counting when both occur separately).
## 3) If multiple elections remain of the same chosen type in a country-year,
##    keep the first (deterministic tie-breaker) but flag the tie for inspection.
##
## IMPORTANT: This assumes your current df is country–election–year and contains:
## ISO, year, election_type (factor or character: Legislative/Executive/Concurrent),
## electionintegrity, v2elembaut_ord, v2elembcap_ord, v2eldommon,
## intobs, emb_ind_idea, majoritarian, ht_region7, ciri_* , gdp_*.
############################################################

library(dplyr)

# --- 0) quick pre-check: df should be election-event data
stopifnot(all(c("ISO","year","election_type") %in% names(df)))

# --- 1) rank election types within country-year (B&vH-matching collapse priority)
# Priority: Concurrent (1) > Executive (2) > Legislative (3) > otherwise (99)
df_cy <- df %>%
  mutate(
    election_type_chr = as.character(election_type),
    type_priority = case_when(
      election_type_chr == "Concurrent"  ~ 1L,
      election_type_chr == "Executive"   ~ 2L,
      election_type_chr == "Legislative" ~ 3L,
      TRUE                               ~ 99L
    )
  ) %>%
  group_by(ISO, year) %>%
  # --- 2) keep the "best" election in that country-year
  arrange(type_priority, election_type_chr, .by_group = TRUE) %>%
  mutate(
    n_elections_cy = n(),
    n_besttype_cy  = sum(type_priority == min(type_priority, na.rm = TRUE), na.rm = TRUE),
    kept_row       = row_number() == 1L
  ) %>%
  ungroup() %>%
  filter(kept_row) %>%
  select(-kept_row)

# --- 3) diagnostics to confirm collapse behavior
cat("\n====================\nB&vH-STYLE COLLAPSE DIAGNOSTICS\n====================\n")
cat("Rows before (country–election–year): ", nrow(df), "\n", sep = "")
cat("Rows after  (country–year):         ", nrow(df_cy), "\n", sep = "")
cat("Countries after collapse:           ", length(unique(df_cy$ISO)), "\n\n", sep = "")

cat("Election type distribution AFTER collapse:\n")
print(table(df_cy$election_type, useNA = "always"))

cat("\nHow many country–years had >1 election event originally?\n")
print(sum(df_cy$n_elections_cy > 1, na.rm = TRUE))

cat("\nHow many country–years had multiple events of the chosen 'best' type?\n")
# These are the ambiguous cases where tie-breaking (keeping the first) matters.
print(sum(df_cy$n_besttype_cy > 1, na.rm = TRUE))

cat("\nTop 20 ambiguous country–years (multiple elections of same retained type):\n")
print(
  df_cy %>%
    filter(n_besttype_cy > 1) %>%
    arrange(desc(n_besttype_cy), desc(n_elections_cy)) %>%
    select(ISO, year, election_type, n_elections_cy, n_besttype_cy) %>%
    head(20)
)

# --- 4) OPTIONAL: Apply B&vH’s main inclusion restrictions AFTER collapsing
# (this is usually what moves you from ~867 down toward ~847)
df_bvh <- df_cy %>%
  # multiparty elections only
  filter(v2x_regime > 0) %>%
  # must observe the de facto EMB independence variable used in Table 1
  filter(!is.na(v2elembaut_ord)) %>%
  # listwise deletion for Model 2 regressors (fixed effects model)
  filter(
    !is.na(ciri_injud),
    !is.na(ciri_speech),
    !is.na(ciri_assn),
    !is.na(v2elembcap_ord),
    !is.na(intobs),
    !is.na(v2eldommon),
    !is.na(gdp_growth),
    !is.na(gdp_pc_log),
    !is.na(type_exec),
    !is.na(type_conc),
    !is.na(ht_region7)
  )

cat("\n====================\nAFTER B&vH-STYLE MODEL-2 FILTERS\n====================\n")
cat("N (country–years / 'elections'): ", nrow(df_bvh), "\n", sep = "")
cat("N (countries):                  ", length(unique(df_bvh$ISO)), "\n", sep = "")























# SUBSET TO B & VH's INCLUSION CRITERIA -----------------------------------

df <- df %>%       # listwise deletion across all regressors
  filter(
    !is.na(v2elembcap_ord),   # EMB capacity
    !is.na(type_exec),        # executive election dummy
    !is.na(type_conc),        # concurrent election dummy
    !is.na(majoritarian),     # electoral system
    !is.na(ciri_injud),       # judicial independence
    !is.na(ciri_assn),        # civil society freedom
    !is.na(ciri_speech),      # media independence
    !is.na(ht_region7)        # region
  )


# Birch & van Ham (2017), Model 1
# 847 elections
# 146 countries

# Replication
# 1292 elections
# 141 countries

# explanation for discrepancies
# newer V-Dem vintages, some elections have been added retrospectively,
# reclassification of a small number of borderline elections, some borderline cases moved between v2x_regime = 0 and 1
# slightly different handling of missingness in IDEA / QoG,  B & vH used IDEA 2006,


# COMPARE DESCRIPTIVES TO B&VH (2017) -------------------------------------

## Helper function
desc_stats <- function(x) {
  c(
    N    = sum(!is.na(x)),
    Mean = mean(x, na.rm = TRUE),
    Min  = min(x, na.rm = TRUE),
    Max  = max(x, na.rm = TRUE)
  )
}

## Your descriptives (using existing variables)
my_descriptives <- rbind(
  "Election integrity"                       = desc_stats(df$electionintegrity),
  "De jure EMB independence"                 = desc_stats(df$emb_ind_idea),
  "De facto EMB independence"                = desc_stats(df$v2elembaut_ord),
  "Independence of judiciary"                = desc_stats(df$ciri_injud),
  "Independence of media"                    = desc_stats(df$ciri_speech),
  "Civil society freedom"                    = desc_stats(df$ciri_assn),
  "EMB capacity"                             = desc_stats(df$v2elembcap_ord),
  "International observers present?"         = desc_stats(df$intobs),
  "Domestic observers present?"              = desc_stats(df$v2eldommon),
  "GDP per capita annual growth %"            = desc_stats(df$gdp_growth),
  "Real GDP per capita log (current USD)"    = desc_stats(df$gdp_pc_log),
  "Legislative electoral system majoritarian"= desc_stats(df$majoritarian),
  "Type of election"                         = desc_stats(
    ifelse(df$election_type == "Legislative", 1,
           ifelse(df$election_type == "Executive", 2,
                  ifelse(df$election_type == "Concurrent", 3, NA)))
  ),
  "Region"                                   = desc_stats(df$ht_region7)
) %>% as.data.frame()

## Birch & Van Ham (2017) reported descriptives
bvh_descriptives <- data.frame(
  N    = c(1045, 1033, 1045, 949, 913, 912, 1045, 1045, 1007, 994, 1012, 1045, 1045, 1045),
  Mean = c(2.61, 0.66, 2.46, 1.13, 1.11, 1.33, 2.77, 0.52, 0.91, 2.02, 7.88, 0.43, 1.68, 3.42),
  Min  = c(0.39, 0, 0.07, 0, 0, 0, 0.24, 0, 0, -45.33, 4.80, 0, 1, 1),
  Max  = c(3.97, 1, 3.97, 2, 2, 2, 4.00, 1, 1, 104.66, 11.39, 1, 3, 7),
  row.names = rownames(my_descriptives)
)

## Side-by-side comparison
comparison <- cbind(
  BVH_Reported = bvh_descriptives,
  Yours = my_descriptives
)

round(comparison, 2)


comparison_round <- round(comparison, 2)
comparison_df <- as.data.frame(comparison_round)
comparison_df <- tibble::rownames_to_column(comparison_df, var = "Variable")
ft <- flextable(comparison_df) |>
  autofit()
save_as_docx(
  ft,
  path = "results/descriptives.docx"
)


# strangely, B & vH report on ~1,045 observations, although they include only 847 in their model.
# But distributions are very similar.
# Minor differences in descriptive statistics could reflect updated data releases 
# and stricter handling of missing election-level observer data
# e.g. NELDA 'unclear' seems to have been recoded as '0' by B&vH, but 'missing' in ours






############################################################
## TABLE 1 – MODEL 2 ONLY (FIXED EFFECTS)
## Birch & Van Ham (2017)
## OUTPUT: results/Tab1_exact.docx
############################################################


library(modelsummary)
library(tibble)
library(dplyr)

# ------------------------------------------------------------
# Birch & Van Ham (2017) – Table 1, Model 2 (Fixed Effects)
# ------------------------------------------------------------

bvh_model2 <- tribble(
  ~term, ~estimate,
  "De facto EMB independence (0–4)",            "0.319*** (0.025)",
  "Independence of judiciary (0–2)",             "0.041* (0.020)",
  "Independence of media (0–2)",                 "0.066*** (0.019)",
  "Civil society freedom (0–2)",                 "0.037* (0.019)",
  "EMB capacity (0–4)",                          "0.132*** (0.035)",
  "International election observers (0–1)",      "0.007 (0.023)",
  "Domestic election observers (0–1)",           "0.084+ (0.043)",
  "GDP per capita growth, %",                    "-0.006*** (0.001)",
  "Real GDP per capita, logged (current US$)",   "0.003 (0.019)",
  "Executive election (vs. Legislative)",        "-0.022 (0.022)",
  "Concurrent election (vs. Legislative)",       "-0.011 (0.027)"
)










library(plm)
library(lmtest)
library(sandwich)
library(modelsummary)
library(dplyr)

## ---------------------------------------------------------
## Declare panel structure
## ---------------------------------------------------------
pdata <- pdata.frame(df_cy, index = c("ISO", "year"))

## ---------------------------------------------------------
## MODEL 2: FIXED EFFECTS (within estimator)
## ---------------------------------------------------------
texreg::screenreg(m2_fe <- plm(
  electionintegrity ~
    v2elembaut_ord +        # De facto EMB independence
    ciri_injud +            # Independence of judiciary
    ciri_speech +           # Independence of media
    ciri_assn +             # Civil society freedom
    v2elembcap_ord +        # EMB capacity
    intobs +                # International observers
    v2eldommon +            # Domestic observers
    gdp_growth +            # GDP per capita growth
    gdp_pc_log +            # GDP per capita (log)
    election_type,          # Election type
  data   = pdata,
  model  = "within",
  effect = "individual"
))


## Country-clustered standard errors (Arellano HC1)
vcov_m2 <- vcovHC(
  m2_fe,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

## ---------------------------------------------------------
## Coefficient order & labels (match Table 1 exactly)
## ---------------------------------------------------------
coef_map <- c(
  v2elembaut_ord            = "De facto EMB independence (0–4)",
  ciri_injud                = "Independence of judiciary (0–2)",
  ciri_speech               = "Independence of media (0–2)",
  ciri_assn                 = "Civil society freedom (0–2)",
  v2elembcap_ord            = "EMB capacity (0–4)",
  intobs                    = "International election observers (0–1)",
  v2eldommon                = "Domestic election observers (0–1)",
  gdp_growth                = "GDP per capita growth, %",
  gdp_pc_log                = "Real GDP per capita, logged (current US$)",
  
  # Election type contrasts (baseline = Legislative)
  election_typeExecutive    = "Executive election (vs. Legislative)",
  election_typeConcurrent   = "Concurrent election (vs. Legislative)"
)


## ---------------------------------------------------------
## Add N (countries) 
## ---------------------------------------------------------
add_rows_rep <- tibble(
  term = "N (countries)",
  `Replication (FE)` = length(unique(df_cy$ISO))
)



library(officer)
library(flextable)

dir.create("results", showWarnings = FALSE)

# BVH table
bvh_ft <- flextable(bvh_model2) |>
  autofit()

save_as_docx(
  bvh_ft,
  path = "results/Tab1_bvh.docx"
)



modelsummary(
  list(
    "Replication (FE)" = m2_fe
  ),
  vcov      = list(vcov_m2),
  coef_map = coef_map,
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = c(
    "+"  = 0.10,
    "*"  = 0.05,
    "**" = 0.01,
    "***"= 0.001
  ),
  gof_map = tibble::tribble(
    ~raw,        ~clean,              ~fmt,
    "r.squared", "R² (overall)",       3,
    "nobs",      "N (elections)",      0
  ),
  add_rows = add_rows_rep,
  title = "Table 1. Election Management Bodies and Election Integrity\nReplication (Fixed Effects)",
  notes = c(
    "Country fixed-effects (within) estimator.",
    "Legislative elections are the reference category.",
    "Standard errors clustered by country (Arellano HC1)."
  ),
  output = "results/Tab1_replication.docx"
)



## ---------------------------------------------------------
## MODEL 2 variant: DEM binary observers
## ---------------------------------------------------------
texreg::screenreg(m2_dem_bin <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembcap_ord +
    intobs +
    obs_bin +              # DEM binary observers
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata,
  model  = "within",
  effect = "individual"
))

vcov_m2_dem_bin <- vcovHC(
  m2_dem_bin,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

modelsummary(
  list("Replication (FE, DEM bin)" = m2_dem_bin),
  vcov      = list(vcov_m2_dem_bin),
  coef_map = c(coef_map, obs_bin = "Domestic observers (DEM binary)"),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = c("+"=0.10,"*"=0.05,"**"=0.01,"***"=0.001),
  gof_map = tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "r.squared", "R² (overall)", 3,
    "nobs",      "N (elections)",0
  ),
  add_rows = add_rows_rep,
  title = "Table 1. Replication with DEM Binary Domestic Observers",
  notes = c(
    "Country fixed-effects (within) estimator.",
    "Legislative elections are the reference category.",
    "Standard errors clustered by country (Arellano HC1)."
  ),
  output = "results/Tab1_replication_dem_bin.docx"
)



## ---------------------------------------------------------
## MODEL 2 variant: DEM observer counts
## ---------------------------------------------------------
texreg::screenreg(m2_dem_bcount <- plm(
  electionintegrity ~
    v2elembaut_ord +
    ciri_injud +
    ciri_speech +
    ciri_assn +
    v2elembcap_ord +
    intobs +
    obs_count +           # DEM observer count
    gdp_growth +
    gdp_pc_log +
    election_type,
  data   = pdata,
  model  = "within",
  effect = "individual"
))

vcov_m2_dem_bcount <- vcovHC(
  m2_dem_bcount,
  method  = "arellano",
  type    = "HC1",
  cluster = "group"
)

modelsummary(
  list("Replication (FE, DEM count)" = m2_dem_bcount),
  vcov      = list(vcov_m2_dem_bcount),
  coef_map = c(coef_map, obs_count = "Domestic observers (DEM count)"),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = c("+"=0.10,"*"=0.05,"**"=0.01,"***"=0.001),
  gof_map = tibble::tribble(
    ~raw,        ~clean,         ~fmt,
    "r.squared", "R² (overall)", 3,
    "nobs",      "N (elections)",0
  ),
  add_rows = add_rows_rep,
  title = "Table 1. Replication with DEM Domestic Observer Counts",
  notes = c(
    "Country fixed-effects (within) estimator.",
    "Legislative elections are the reference category.",
    "Standard errors clustered by country (Arellano HC1)."
  ),
  output = "results/Tab1_replication_dem_count.docx"
)
