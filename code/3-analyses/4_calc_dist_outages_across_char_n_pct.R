# Code goals: 
# Calculate outage type distribution and prevalence within subpopulations 
#    calculate the n, pct, and their respective confidence intervals using svydesign/weights
# Vivian Do
# Updated 1/31/24


# Read in libraries and paths --------------------------------------------------------------------
# read in libraries
rm(list=ls(all=TRUE))
library(here)
source(here("code", "0-setup", "libraries.R"))

# read in data
outage_dta <- read_sas(paste0(here("data", "raw", "building_characteristics", "ei_analytic_team2.sas7bdat")))
names(outage_dta)
glimpse(outage_dta)


# Prep data ---------------------------------------------------------------
outage_dta <- transform(outage_dta, strata=as.character(strata))

# specify label names for each variable; easier to read table 1s
outage_dta <- outage_dta %>% 
  mutate(HHSIZE_gtet4 = ifelse(HHSIZE >= 4, ">= 4 hh size", "<4 hh size"),
         HH_CHILD17_dich = case_when(HH_CHILD17_dich == "1" ~ "Yes",
                                     TRUE ~ "No"),
         HH_CHILD4_dich = case_when(HH_CHILD4_dich == "1" ~ "Yes",
                                    TRUE ~ "No"),
         HH_ADULT60 = case_when(HH_ADULT60 >= 1 ~ "Yes",
                                TRUE ~ "No"),
         HH_TYPE_MAR = case_when(HH_TYPE_MAR == "1" ~ "Owner",
                                 HH_TYPE_MAR == "2" ~ "Renter",
                                 HH_TYPE_MAR == "3" ~ "Renter",
                                 TRUE ~ NA_character_),
         RENTAL_TYPE = case_when(RENTAL_TYPE == "1" ~ "Public housing (NYCHA)",
                                 RENTAL_TYPE == "2" ~ "Rental assistance program",
                                 RENTAL_TYPE == "3" ~ "Rent-controlled/stabilized",
                                 RENTAL_TYPE == "4" ~ "Market rate",
                                 TRUE ~ NA_character_),
         AGEGROUP6 = case_when(AGEGROUP6 == "1" ~ "18-24",
                               AGEGROUP6 == "2" ~ "25-29",
                               AGEGROUP6 == "3" ~ "30-44",
                               AGEGROUP6 == "4" ~ "45-64",
                               AGEGROUP6 == "5" ~ "65-74",
                               AGEGROUP6 == "6" ~ "75+",
                               TRUE ~ NA_character_),
         EDUCAT5 = case_when(EDUCAT5 == "1" ~ "Less than high school",
                             EDUCAT5 == "2" ~ "Grade 12/GED",
                             EDUCAT5 == "3" ~ "Technical school/associate's degree",
                             EDUCAT5 == "4" ~ "College graduate",
                             EDUCAT5 == "5" ~ "Graduate/professional degree",
                             TRUE ~ NA_character_),
         ENGLISH = case_when(ENGLISH == "1" ~ "English speaking-only",
                             ENGLISH == "2" ~ "Multi-lingual/non-English speaking",
                             TRUE ~ NA_character_),
         GENDER_CAT3 = case_when(GENDER_CAT3 == "1" ~ "Man",
                                 GENDER_CAT3 == "2" ~ "Women",
                                 GENDER_CAT3 == "3" ~ "Transgender",
                                 TRUE ~ NA_character_),
         HHPOV200 = case_when(HHPOV200 == "1" ~ "<200% FPL",
                              HHPOV200 == "2" ~ ">=200% FPL",
                              TRUE ~ NA_character_),
         NEIGH_POV = case_when(NEIGH_POV == "1" ~ "<10% below poverty",
                               NEIGH_POV == "2" ~ "10%-<20% below poverty",
                               NEIGH_POV == "3" ~ "20%-<30% below poverty",
                               NEIGH_POV == "4" ~ ">=30% below poverty",
                               TRUE ~ NA_character_),
         RACEETH = case_when(RACEETH == "1" ~ "White, Non-Latino",
                             RACEETH == "2" ~ "Black, Non-Latino",
                             RACEETH == "3" ~ "Latino",
                             RACEETH == "4" ~ "Asian/Pacific Islander",
                             RACEETH == "5" ~ "Other/Multi-Racial, Non-Latino", # join "Other" categories together
                             TRUE ~ NA_character_),
         QBORO = case_when(QBORO == "1" ~ "The Bronx",
                           QBORO == "2" ~ "Brooklyn",
                           QBORO == "3" ~ "Manhattan",
                           QBORO == "4" ~ "Queens",
                           QBORO == "5" ~ "Staten Island"),
         GENHEALTH = case_when(GENHEALTH == "1" ~ "Excellent",
                               GENHEALTH == "2" ~ "Very good",
                               GENHEALTH == "3" ~ "Good",
                               GENHEALTH == "4" ~ "Fair",
                               GENHEALTH == "5" ~ "Poor",
                               TRUE ~ NA_character_),
         DIABETES = case_when(DIABETES == "1" ~ "Yes",
                              DIABETES == "2" ~ "No",
                              TRUE ~ NA_character_),
         HEART = case_when(HEART == "1" ~ "Yes",
                           HEART == "2" ~ "No",
                           TRUE ~ NA_character_),
         HYPERTEN = case_when(HYPERTEN == "1" ~ "Yes",
                              HYPERTEN == "2" ~ "No",
                              TRUE ~ NA_character_),
         RESP = case_when(RESP == "1" ~ "Yes",
                          RESP == "2" ~ "No",
                          TRUE ~ NA_character_),
         KIDNEY = case_when(KIDNEY == "1" ~ "Yes",
                            KIDNEY == "2" ~ "No",
                            TRUE ~ NA_character_),
         COGNITIVE = case_when(COGNITIVE == "1" ~ "Yes",
                               COGNITIVE == "2" ~ "No",
                               TRUE ~ NA_character_),
         MENTAL = case_when(MENTAL == "1" ~ "Yes",
                            MENTAL == "2" ~ "No",
                            TRUE ~ NA_character_),
         EME = case_when(EME == "1" ~ "Yes",
                         EME == "2" ~ "No",
                         TRUE ~ NA_character_),
         OXYGEN = case_when(OXYGEN == "1" ~ "Yes",
                            OXYGEN == "2" ~ "No",
                            TRUE ~ NA_character_),
         DIALYSIS = case_when(DIALYSIS == "1" ~ "Yes",
                              DIALYSIS == "2" ~ "No",
                              TRUE ~ NA_character_),
         INFUSION = case_when(INFUSION == "1" ~ "Yes",
                              INFUSION == "2" ~ "No",
                              TRUE ~ NA_character_),
         CPAP = case_when(CPAP == "1" ~ "Yes",
                          CPAP == "2" ~ "No",
                          TRUE ~ NA_character_),
         BREASTPUMP = case_when(BREASTPUMP == "1" ~ "Yes",
                                BREASTPUMP == "2" ~ "No",
                                TRUE ~ NA_character_),
         ELEC_WHEEL = case_when(ELEC_WHEEL == "1" ~ "Yes",
                                ELEC_WHEEL == "2" ~ "No",
                                TRUE ~ NA_character_),
         NEBULIZER = case_when(NEBULIZER == "1" ~ "Yes",
                               NEBULIZER == "2" ~ "No",
                               TRUE ~ NA_character_),
         EME_OTHER = case_when(EME_OTHER == "1" ~ "Yes",
                               EME_OTHER == "2" ~ "No",
                               TRUE ~ NA_character_),
         AFFECT_SLEEP = case_when(AFFECT_SLEEP == "0" ~ "No",
                                  AFFECT_SLEEP == "1" ~ "Yes",
                                  TRUE ~ NA_character_),
         AFFECT_STRESS = case_when(AFFECT_STRESS == "0" ~ "No",
                                   AFFECT_STRESS == "1" ~ "Yes",
                                   TRUE ~ NA_character_),
         AFFECT_HEALTH = case_when(AFFECT_HEALTH == "0" ~ "No",
                                   AFFECT_HEALTH == "1" ~ "Yes",
                                   TRUE ~ NA_character_),
         AFFECT_FOOD = case_when(AFFECT_FOOD == "0" ~ "No",
                                 AFFECT_FOOD == "1" ~ "Yes",
                                 TRUE ~ NA_character_),
         AFFECT_MED = case_when(AFFECT_MED == "0" ~ "No",
                                AFFECT_MED == "1" ~ "Yes",
                                TRUE ~ NA_character_),
         nfloors_rup = case_when(nfloors_rup == "1" ~ "Low-rise (1-2 floors)",
                                 nfloors_rup == "2" ~ "Mid-rise (3-5 floors)",
                                 nfloors_rup == "3" ~ "High-rise (6 stories of higher)",
                                 TRUE ~ NA_character_),
         yrblt3 = case_when(yrblt3 == "1" ~ "<1940",
                            yrblt3 == "2" ~ "1940-1969",
                            yrblt3 == "3" ~ "1970-1999",
                            yrblt3 == "4" ~ "≥2000",
                            TRUE ~ NA_character_))

# aggregate neighborhood poverty to fewer categories
outage_dta <- outage_dta %>% 
  mutate(NEIGH_POV_collapsed = case_when(NEIGH_POV == ">=30% below poverty" ~ "High poverty (≥20% below poverty)",
                                         NEIGH_POV == "20%-<30% below poverty" ~ "High poverty (≥20% below poverty)",
                                         NEIGH_POV == "10%-<20% below poverty" ~ "Low poverty (<20% below poverty)",
                                         NEIGH_POV == "<10% below poverty" ~ "Low poverty (<20% below poverty)",
                                         TRUE ~ NA_character_))

# for looping: list of service outage types
service_outages <- c("INTER_ELEC", "INTER_GAS", "INTER_HEAT", "INTER_WATER", "INTER_ELEC_GAS")

# for looping:  list of categorized demographic vars
# commented out vars at the end of code lines are ones we do not use
hh_char <- c("HH_ADULT60", "HH_TYPE_MAR", "RENTAL_TYPE") #"HHSIZE_gtet4", "HH_CHILD17_dich", "HH_CHILD4_dich", 
demographic <- c("RACEETH", "HHPOV200", "NEIGH_POV", "NEIGH_POV_collapsed") #"AGEGROUP6", "EDUCAT5", "ENGLISH", "GENDER_CAT3, "QBORO"
health <- c("DIABETES", "HYPERTEN", "COGNITIVE", "MENTAL") #"GENHEALTH", "RESP", "KIDNEY", "HEART", 
medical_device <- c("EME") #"OXYGEN", "DIALYSIS", "INFUSION", "CPAP", "BREASTPUMP", "ELEC_WHEEL", "NEBULIZER", "EME_OTHER"
# vars_from_qual_analysis <- c("AFFECT_SLEEP", "AFFECT_STRESS", "AFFECT_HEALTH", "AFFECT_FOOD", "AFFECT_MED")
building_char <- c("nfloors_rup", "yrblt3")

# create new variables specifying 
#    (1) frequency of singular outage type 
#    (2) total number of reported outages per individual based on how responses were coded (e.g., 2 == "no")
outage_ext_factors <- outage_dta %>% 
  select(EnergyID, strata, final_wt, INTER_ELEC, INTER_GAS, INTER_WATER, INTER_HEAT,
         hh_char, demographic, health, medical_device, HHSIZE_gtet4, building_char) %>%
  mutate(outage_combo = case_when(INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 8 ~ "No outages",
                                  INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 7 & INTER_ELEC == 1 ~ "ELEC ONLY",
                                  INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 7 & INTER_GAS == 1 ~ "GAS ONLY",
                                  INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 7 & INTER_WATER == 1 ~ "WATER ONLY",
                                  INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 7 & INTER_HEAT == 1 ~ "HEAT ONLY",
                                  INTER_ELEC == 1 & INTER_GAS == 1 ~ "ELEC + GAS",
                                  TRUE ~ "Other combo"),
         n_outages = case_when(INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 8 ~ "0 outages",
                               INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 7 ~ "1 outage",
                               INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT == 6 ~ "2 outages",
                               INTER_ELEC + INTER_GAS + INTER_WATER + INTER_HEAT <= 5 ~ "3+ outages"),
         INTER_ELEC = case_when(INTER_ELEC == 1 ~ 1,
                                INTER_ELEC == 2 ~ 0,
                                TRUE ~ NA_real_),
         INTER_GAS = case_when(INTER_GAS == 1 ~ 1,
                               INTER_GAS == 2 ~ 0,
                               TRUE ~ NA_real_),
         INTER_WATER = case_when(INTER_WATER == 1 ~ 1,
                                 INTER_WATER == 2 ~ 0,
                                 TRUE ~ NA_real_),
         INTER_HEAT = case_when(INTER_HEAT == 1 ~ 1,
                                INTER_HEAT == 2 ~ 0,
                                TRUE ~ NA_real_),
         INTER_ELEC_GAS = case_when(INTER_ELEC == 1 & INTER_ELEC == 1 ~ 1,
                                INTER_ELEC == 1 & INTER_ELEC == 2 ~ 0,
                                INTER_ELEC == 2 & INTER_ELEC == 1 ~ 0,
                                INTER_ELEC == 2 & INTER_ELEC == 2 ~ 0,
                                TRUE ~ NA_real_))


# Apply svydesign ---------------------------------------------------------
# check if we need nest = true or nest = false
# "If your PSUs reuse the same identifiers across strata then set nest=TRUE" aka repeated identifiers
length(unique(outage_ext_factors$EnergyID)) == nrow(outage_ext_factors) # unique identifiers so set nest = FALSE (default)

# define the survey design 
glimpse(outage_ext_factors)
svy_outage_ext_factors <- svydesign(ids = ~EnergyID , strata = ~strata, weights= ~final_wt, data = outage_ext_factors, na.rm=TRUE)


# Calculate dist of each outage type across subpopulations from lists generated earlier ---------------------------------------------------
characteristic <- c(hh_char, demographic, health, medical_device, building_char)
serv_type <- c("INTER_ELEC", "INTER_GAS", "INTER_HEAT", "INTER_WATER", "INTER_ELEC_GAS")

# characteristic <- c("RENTAL_TYPE", "QBORO")
# serv_type <- c("INTER_ELEC")
dist_outages_across_char_n_pct <- data.frame()

for (c in 1:length(characteristic)){
  print(characteristic[c])
  for (s in 1:length(serv_type)){
    print(serv_type[s])
    
    # totals for each level of characteristic
    totals_per_level_in_characteristic <- as.data.frame(svytable(as.formula(paste("~", characteristic[c])), 
                                                                 design = svy_outage_ext_factors)) %>% 
      rename(level_total = Freq)
    
    # service outage == 1 across levels of characteristic (n)
    total_serv_type_across_characteristic <- as.data.frame(svyby(as.formula(paste("~", serv_type[s])), 
                                                                 by = as.formula(paste("~", characteristic[c])), 
                                                                 design = svy_outage_ext_factors, 
                                                                 FUN = svytotal, 
                                                                 na.rm = TRUE))
    total_serv_type_across_characteristic <- total_serv_type_across_characteristic %>%
      mutate(service_type = paste0(serv_type[s]),
             lci = total_serv_type_across_characteristic[[2]]-1.96*total_serv_type_across_characteristic[[3]],
             uci = total_serv_type_across_characteristic[[2]]+1.96*total_serv_type_across_characteristic[[3]]) %>% 
      rename_at(2, ~"prev_outage") %>% 
      select(service_type, everything())
    
    
    
    # service outage == 1 across levels of characteristic (%)
    pct_serv_type_across_characteristic <- full_join(totals_per_level_in_characteristic, total_serv_type_across_characteristic) %>% 
      mutate(pct_total = prev_outage/level_total*100,
             pct_lci = lci/level_total*100,
             pct_uci = uci/level_total*100) %>% 
      mutate(characteristic = paste(characteristic[c])) %>% 
      rename_at(1, ~"characteristic_level") %>% 
      select(service_type, characteristic, characteristic_level, everything()) %>% 
      mutate(across(where(is.numeric), ~round(., 1)))
    
    # reliability metrics; calculate rse manually 
    pct_serv_type_across_characteristic_w_rse <- pct_serv_type_across_characteristic %>% 
      mutate(rse = se/prev_outage*100,
             reliability_asterik = case_when((rse >= 30 & rse < 50) ~ "*",
                                             (prev_outage < 50) ~ "*",
                                             ((pct_uci - pct_lci) < 6 & rse >= 50) ~ "*",
                                             ((pct_uci - pct_lci)/2 > 10) ~ "*",
                                             TRUE ~ ""),
             reliability_suppress = case_when((rse >= 50 & (uci - lci) >= 6) ~ "^",
                                              (prev_outage < 50 & pct_total == 0) ~ "^",
                                              (prev_outage < 50 & pct_total == 100) ~ "^",
                                              (se == 0 & (uci - lci) == 0 & pct_total == 0) ~ "^",
                                              (se == 0 & (uci - lci) == 0 & pct_total == 100) ~ "^",  
                                              TRUE ~ "")
      )
    
    # save output to growing dataset of all outputs
    dist_outages_across_char_n_pct <- rbind(dist_outages_across_char_n_pct, pct_serv_type_across_characteristic_w_rse)
    
  }
}

# check out the results
dist_outages_across_char_n_pct

# save output
write_csv(dist_outages_across_char_n_pct, paste0(here(), "/data/processed/dist_outages_across_char_n_pct_w_rse.csv"))


# Format previous output by combining vars, cleaning up, etc -----------------------------------------------------------
# calculate the characteristic weighted percent using total individuals the survey estimates for (ie, 6 417 763)
dist_outages_across_char_n_pct <- read_csv(paste0(here(), "/data/processed/dist_outages_across_char_n_pct_w_rse.csv")) %>% 
  mutate(char_wght_pct = round((level_total/6417763)*100, 1))

# round to nearest thousand
round_to_thousand <- function(x) {
  round(x / 1000) * 1000
}

# read in chisquare outputs to join with current prevalence results (chisquare results produced from calc_chisq.R)
chisq_ref <- read_csv(paste0(here(), "/data/processed/chisq_ref.csv"))

# format output
dist_outages_across_char_n_pct2 <- dist_outages_across_char_n_pct %>% 
  left_join(., chisq_ref, by = c("service_type" = "r_serv_type",
                                 "characteristic" = "r_characteristic",
                                 "characteristic_level" = "r_level")) %>% 
  mutate(chisq_stat_sig = ifelse(V5 < 0.05, "!", NA),
         ref = ifelse(is.na(r_ref), "(ref)", ""),
         characteristic_level_n = paste0(characteristic_level, " (", round_to_thousand(level_total), ", ", char_wght_pct, "%) ", ref),
         prev_n_pct_ci = paste0((prev_outage), " (", (pct_total), "%, ", (pct_lci), "%-", (pct_uci), "%)", 
                                reliability_asterik, reliability_suppress, chisq_stat_sig),
         prev_n_pct_ci = gsub("NA", "", prev_n_pct_ci)) %>% 
  select(service_type, characteristic, characteristic_level_n, prev_n_pct_ci) %>% 
  pivot_wider(names_from = service_type, values_from = prev_n_pct_ci)

write_csv(dist_outages_across_char_n_pct2, paste0(here(), "/data/processed/dist_outages_across_char_n_pct_w_rse_wide.csv"))


# Final list of variables to keep -----------------------------------------
# making sure we are only keeping variables of interest for final output
dist_outages_across_char_n_pct3 <- dist_outages_across_char_n_pct2 %>% 
  filter(characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH",
                               "DIABETES", "COGNITIVE", "MENTAL", "HYPERTEN", "EME",
                               "RENTAL_TYPE", "yrblt3")) %>% 
  mutate_all(~ifelse(str_detect(., "\\^"), "", .)) 

# clean filtered data
dist_outages_across_char_n_pct3_cleaned <- dist_outages_across_char_n_pct3 %>% 
  mutate(category = case_when(characteristic %in% c("RENTAL_TYPE", "nfloors_rup", "yrblt3") ~ "Building",
                              characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH", "QBORO") ~ "Demographic",
                              characteristic %in% c("DIABETES", "HYPERTEN", "COGNITIVE", "MENTAL", "EME") ~ "Health"),
         characteristic = case_when(characteristic == "RENTAL_TYPE" ~ "Rental type",
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
                                    characteristic == "yrblt3" ~ "Building year built")) %>% 
  group_by(category, characteristic) %>% 
  arrange(category, characteristic, desc(str_detect(characteristic_level_n, "\\(ref\\)"))) %>% 
  ungroup() %>% 
  rename(Category = category,
         Characteristic = characteristic,
         Level = characteristic_level_n,
         "Electric outage" = INTER_ELEC,
         "Gas outage" = INTER_GAS,
         "Heat outage" = INTER_HEAT,
         "Water outage" = INTER_WATER,
         "Electric and gas outage" = INTER_ELEC_GAS) %>% 
  select(Category, everything()) 


# Format with more columns for clarity --------------------------------------------------
dist_outages_across_char_n_pct4 <- dist_outages_across_char_n_pct %>%
  filter(characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH",
                               "DIABETES", "COGNITIVE", "MENTAL", "HYPERTEN", "EME",
                               "RENTAL_TYPE", "nfloors_rup", "yrblt3")) %>% 
  mutate(service_type = ifelse(service_type == "INTER_ELEC_GAS", "INTER_COMBO", service_type)) %>% 
  left_join(., chisq_ref, by = c("service_type" = "r_serv_type",
                                 "characteristic" = "r_characteristic",
                                 "characteristic_level" = "r_level")) %>% 
  group_by(characteristic) %>%
  fill(r_ref, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(chisq_stat_sig = ifelse(V5 < 0.05, "!", NA),
         ref = ifelse(as.character(r_ref) == characteristic_level, "(ref)", ""),
         characteristic = characteristic,
         characteristic_level = paste0(characteristic_level, " ", ref),
         characteristic_level_n = round_to_thousand(level_total),
         characteristic_level_pct = char_wght_pct,
         outage_n = round_to_thousand(prev_outage),
         outage_pct = paste0(pct_total, " (", pct_lci, "-", pct_uci, ")", reliability_asterik, reliability_suppress, chisq_stat_sig), 
         outage_pct = gsub("NA", "", outage_pct)) %>% 
  select(service_type, characteristic, starts_with(c("characteristic", "outage"))) %>% 
  pivot_wider(names_from = service_type, values_from = c(outage_n, outage_pct)) %>% 
  mutate_all(~ifelse(str_detect(., "\\^"), "", .)) %>% 
  mutate(category = case_when(characteristic %in% c("RENTAL_TYPE", "nfloors_rup", "yrblt3") ~ "Building",
                              characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH", "QBORO") ~ "Demographic",
                              characteristic %in% c("DIABETES", "HYPERTEN", "COGNITIVE", "MENTAL", "EME") ~ "Health"),
         characteristic = case_when(characteristic == "RENTAL_TYPE" ~ "Rental type",
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
                                    characteristic == "yrblt3" ~ "Year built")) %>% 
  select(category, characteristic, characteristic_level, characteristic_level_n, characteristic_level_pct,
         matches("elec"), matches("gas"), matches("heat"), matches("water"), matches("combo"), everything()) %>% 
  group_by(category, characteristic) %>% 
  arrange(category, characteristic, desc(str_detect(characteristic_level_n, "\\(ref\\)"))) %>% 
  filter(!(characteristic == "Neighborhood poverty" & characteristic_level == "Other "),
         !(characteristic == "Household poverty" & characteristic_level == "Other "),
         !(characteristic == "Number of floors in building" & characteristic_level == "Unknown "),
         !(characteristic_level == "NA NA")) %>% 
  mutate(characteristic_level = str_replace_all(characteristic_level, " NA", ""))

  
write_csv(dist_outages_across_char_n_pct4, paste0(here(), "/data/processed/dist_outages_across_char_n_pct_w_rse_wide_select.csv"))  
  




write_csv(dist_outages_across_char_n_pct3_cleaned, paste0(here(), "/data/processed/dist_outages_across_char_n_pct_w_rse_wide_select.csv"))



# test --------------------------------------------------------------------

test <- dist_outages_across_char_n_pct %>%
  filter(characteristic %in% c("HHPOV200", "NEIGH_POV_collapsed", "RACEETH",
                               "DIABETES", "COGNITIVE", "MENTAL", "HYPERTEN", "EME")) %>% 
  mutate(service_type = ifelse(service_type == "INTER_ELEC_GAS", "INTER_COMBO", service_type)) %>% 
  left_join(., chisq_ref, by = c("service_type" = "r_serv_type",
                                 "characteristic" = "r_characteristic",
                                 "characteristic_level" = "r_level")) %>% 
  mutate(chisq_stat_sig = ifelse(V5 < 0.05, "!", NA),
         ref = ifelse(is.na(r_ref), "(ref)", ""),
         characteristic = characteristic,
         characteristic_level = paste0(characteristic_level, " ", ref),
         characteristic_level_n = round_to_thousand(level_total),
         characteristic_level_pct = char_wght_pct,
         outage_n = round_to_thousand(prev_outage),
         outage_pct = paste0(pct_total, " (", pct_lci, "-", pct_uci, ")", reliability_asterik, reliability_suppress, chisq_stat_sig), 
         outage_pct = gsub("NA", "", outage_pct)) %>% 
  select(service_type, characteristic, starts_with(c("characteristic", "outage"))) 

test2 <- test %>% 
  pivot_wider(names_from = service_type, values_from = c(outage_n, outage_pct))
  

