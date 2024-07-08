# Code goals: 
# Use svydesign to estimate the percent of subgroups experiencing outages at an overall level
# Vivian Do
# Updated 3/12/24


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
         INTER_ELEC_GAS = case_when(INTER_ELEC == 1 & INTER_GAS == 1 ~ 1,
                                    INTER_ELEC == 1 & INTER_GAS == 0 ~ 0,
                                    INTER_ELEC == 0 & INTER_GAS == 1 ~ 0,
                                    INTER_ELEC == 0 & INTER_GAS == 0 ~ 0,
                                    TRUE ~ NA_real_))


# Apply svydesign ---------------------------------------------------------
# check if we need nest = true or nest = false
# "If your PSUs reuse the same identifiers across strata then set nest=TRUE" aka repeated identifiers
length(unique(outage_ext_factors$EnergyID)) == nrow(outage_ext_factors) # unique identifiers so set nest = FALSE (default)

# define the survey design 
glimpse(outage_ext_factors)
svy_outage_ext_factors <- svydesign(ids = ~EnergyID , strata = ~strata, weights= ~final_wt, data = outage_ext_factors, na.rm=TRUE)


# Obtain overall prevalence -----------------------------------------------
pct_char_svy_overall <- data.frame()
characteristic <- c(hh_char, demographic, health, medical_device, building_char)

c <- 3
cl <- 4
for (c in 1:length(characteristic)){
  print(characteristic[c])
    
    # get the non-NA levels of the characteristic (e.g., MENTAL has levels, "No", "Yes")
    characteristic_levels <- unique(outage_ext_factors[[characteristic[c]]])
    characteristic_levels <- characteristic_levels[!is.na(characteristic_levels)]
    
    for (cl in 1:length(characteristic_levels)){
      print(characteristic_levels[cl])

      # percent of service type across characteristic levels
      formula_prop <- as.formula(paste0("~I(", characteristic[c], "==", "'", characteristic_levels[cl], "'", ")"))

      pct_char_level_in_svy <- svyciprop(formula_prop,
                                         svy_outage_ext_factors,
                                         method = "xlogit",
                                         na.rm = T) * 100

      ci <- attr(pct_char_level_in_svy, "ci")

      result_characteristic <- as.character(characteristic[c])
      result_characteristic_level <- as.character(paste0(characteristic_levels[cl]))
      pct_char_level_in_svy_est <- round(pct_char_level_in_svy[[1]])
      pct_char_level_in_svy_lci <- round(ci[1], 3)
      pct_char_level_in_svy_uci <- round(ci[2])

      result <- cbind(result_characteristic, result_characteristic_level, pct_char_level_in_svy_est, pct_char_level_in_svy_lci, pct_char_level_in_svy_uci)
      pct_char_svy_overall <- rbind(pct_char_svy_overall, result)

    }
}

# check out the results
pct_char_svy_overall

# save
write_csv(pct_char_svy_overall, paste0(here(), "/output/results/pct_char_svy_overall.csv"))


