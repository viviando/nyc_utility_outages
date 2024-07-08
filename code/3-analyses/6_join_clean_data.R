# Code goals: 
# Clean data and join for a final table
# Vivian Do
# Updated 5/13/24

# Read in libraries and paths --------------------------------------------------------------------
# read in libraries
rm(list=ls(all=TRUE))
library(here)
source(here("code", "0-setup", "libraries.R"))


# Read in data ------------------------------------------------------------
# read in chisquare outputs to join with current prevalence results (chisquare results produced from calc_chisq.R)
chisq_ref <- read_csv(paste0(here(), "/data/processed/chisq_ref.csv")) %>% 
  rename(service_type = r_serv_type,
         characteristic = r_characteristic,
         characteristic_level = r_level,
         pvalue = V5)
dist_char_level <- read_csv(paste0(here(), "/output/results/dist_char_level.csv"))
pct_char_svy_overall <- read_csv(paste0(here(), "/output/results/pct_char_svy_overall.csv")) %>% 
  rename(characteristic = result_characteristic,
         characteristic_level = result_characteristic_level)


# Join data ---------------------------------------------------------------
joined_data <- left_join(dist_char_level, pct_char_svy_overall) %>% 
  left_join(., chisq_ref)

# Clean data --------------------------------------------------------------

# round to nearest thousand
round_to_thousand <- function(x) {
  round(x / 1000) * 1000
}

# select characteristics in study and clean the names
joined_data_selected_char <- joined_data %>%
  filter(
    characteristic %in% c(
      "nfloors_rup",
      "HHPOV200",
      "NEIGH_POV_collapsed",
      "RACEETH",
      "DIABETES",
      "COGNITIVE",
      "MENTAL",
      "HYPERTEN",
      "EME",
      "RENTAL_TYPE",
      "yrblt3",
      "HH_TYPE_MAR"
    )
  ) %>%
  mutate(
    category = case_when(
      characteristic %in% c("RENTAL_TYPE", "nfloors_rup", "yrblt3", "HH_TYPE_MAR") ~ "Building",
      characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH", "QBORO") ~ "Demographic",
      characteristic %in% c("DIABETES", "HYPERTEN", "COGNITIVE", "MENTAL", "EME") ~ "Health"
    ),
    characteristic = case_when(
      characteristic == "RENTAL_TYPE" ~ "Rental type",
      characteristic == "HHPOV200" ~ "Household poverty",
      characteristic == "NEIGH_POV_collapsed" ~ "Neighborhood poverty",
      characteristic == "RACEETH" ~ "Race/ethnicity",
      characteristic == "QBORO" ~ "Borough",
      characteristic == "DIABETES" ~ "Diabetes",
      characteristic == "HYPERTEN" ~ "Hypertension",
      characteristic == "COGNITIVE" ~ "Cognitive condition",
      characteristic == "MENTAL" ~ "Mental health condition",
      characteristic == "EME" ~ "Electricity-dependent medical equipment",
      characteristic == "nfloors_rup" ~ "Number of floors in building",
      characteristic == "yrblt3" ~ "Building year built",
      characteristic == "HH_TYPE_MAR" ~ "Owner/rental status"
    )
  ) %>%
  rename(
    char_count = level_total,
    char_pct = pct_char_level_in_svy_est,
    char_pct_lci = pct_char_level_in_svy_lci,
    char_pct_uci = pct_char_level_in_svy_uci,
    outage_char_level_count = prev_outage,
    outage_char_level_pct = pct_total,
    outage_char_level_pct_lci = pct_lci,
    outage_char_level_pct_uci = pct_uci
  ) %>%
  select(-c(se, lci, uci, rse)) %>%  # these are vars for the reliability indicators, which can be dropped
  mutate_at(vars(contains("count")), round_to_thousand) %>%
  mutate_at(vars(contains("pct")), ~round(., 0)) %>%
  group_by(characteristic) %>%
  fill(r_ref, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(
    ref = ifelse(as.character(r_ref) == characteristic_level, "(ref)", ""),
    chisq_stat_sig = ifelse(pvalue < 0.05, "!", NA),
    characteristic_level = paste0(characteristic_level, " ", ref),
    outage_char_level_pct = gsub("NA", "", outage_char_level_pct),
    outage_char_level_pct_95ci = paste0(
      outage_char_level_pct, 
      " (",
      outage_char_level_pct_lci,
      "-",
      outage_char_level_pct_uci,
      ")",
      reliability_asterik,
      reliability_suppress,
      chisq_stat_sig
    ),
    outage_char_level_pct_95ci = gsub("NA", "", outage_char_level_pct_95ci),
    char_pct_95ci = paste0(
      char_pct,
      " (",
      char_pct_lci,
      "-",
      char_pct_uci,
      ")"
    )
  ) %>%
  mutate_all( ~ ifelse(str_detect(., "\\^"), "", .)) %>% #if there is a ^, then suppress (aka make NA)
  select(-c(
    reliability_asterik,
    reliability_suppress,
    chisq_stat_sig,
    ref,
    r_ref,
    pvalue
  )) %>%
  select(
    category,
    service_type,
    characteristic,
    characteristic_level,
    char_count,
    char_pct_95ci,
    outage_char_level_count,
    outage_char_level_pct_95ci
  )


# pivot from long to wide by outage
joined_pivoted <- joined_data_selected_char %>%
  pivot_wider(
    names_from = service_type,
    values_from = c(
      outage_char_level_count,
      outage_char_level_pct_95ci
    )
  ) %>% 
  mutate(characteristic_level = gsub(">=", "≥", characteristic_level),
         characteristic_level_order = case_when(
           characteristic_level == "<1940 (ref)" ~ 1,
           characteristic_level == "1940-1969 " ~ 2,
           characteristic_level == "1970-1999 " ~ 3,
           characteristic_level == "≥2000 " ~ 4,
           characteristic_level == "White, Non-Latino (ref)" ~ 1,
           characteristic_level == "Black, Non-Latino " ~ 2,
           characteristic_level == "Latino " ~ 3,
           characteristic_level == "Asian/Pacific Islander " ~ 4,
           characteristic_level == "Other/Multi-Racial, Non-Latino " ~ 5,
           characteristic_level == "Low poverty (<20% below poverty) (ref)" ~ 1,
           characteristic_level == "High poverty (≥20% below poverty) " ~ 2,
           characteristic_level == "Low-rise (1-2 floors) (ref)" ~ 1,
           characteristic_level == "Mid-rise (3-5 floors) " ~ 2,
           characteristic_level == "High-rise (6+ floors) " ~ 3,
         )) %>% 
  arrange(category, characteristic, characteristic_level_order) %>% 
  select(category, characteristic, characteristic_level, char_count, char_pct_95ci,
         outage_char_level_count_INTER_ELEC, outage_char_level_pct_95ci_INTER_ELEC,
         outage_char_level_count_INTER_GAS, outage_char_level_pct_95ci_INTER_GAS,
         outage_char_level_count_INTER_HEAT, outage_char_level_pct_95ci_INTER_HEAT,
         outage_char_level_count_INTER_WATER, outage_char_level_pct_95ci_INTER_WATER,
         outage_char_level_count_INTER_ELEC_GAS, outage_char_level_pct_95ci_INTER_ELEC_GAS) %>% 
  select(-starts_with("outage_char_level_count_"))
  
  
# save
write_csv(joined_pivoted, paste0(here(), "/output/results/joined_pivoted.csv"))










