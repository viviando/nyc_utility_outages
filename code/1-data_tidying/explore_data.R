# Code summary ------------------------------------------------------------
# Author: Vivian Do
# Date: 8/1/23
# Goal: Read in gas outage data from NYCDOH to understand how big of a problem it is

# Load data ---------------------------------------------------------------
# read libraries
library(here)
source(here("code", "0-setup", "libraries.R"))

# read in data
gas_outage <- read_sas(paste0(here("data", "raw", "ei_analytic_team.sas7bdat")))
names(gas_outage)

# keep variables
hh_char <- c("HHSIZE", "HH_CHILD17_dich", "HH_CHILD4_dich", "HH_ADULT60", "HH_TYPE_MAR", "RENTAL_TYPE")
energy <- c("UNABLEPAY_ELEC", "OWE_ELEC", "OWE_ELECGAS", "UNABLEPAY_GAS", "OWE_GAS", "TURNOFF_ELEC", "TURNOFF_GAS", "INTER_ELEC", "INTER_GAS")
assist_program <- c("HELP_UTIL", "HELP_FRIEND", "HELP_ORG")
demographic <- c("AGEGROUP6", "EDUCAT5", "ENGLISH", "GENDER_CAT3", "HHPOV200", "NEIGH_POV", "RACEETH", "QBORO")
health <- c("GENHEALTH", "DIABETES", "HEART", "HYPERTEN", "RESP", "KIDNEY", "COGNITIVE", "MENTAL", "EME")

gas_outage <- gas_outage %>% 
  select(hh_char, energy, assist_program, demographic, health)

# Evaluate gas outage prevalence/combos ----------------------------------------------------
table(gas_outage$TURNOFF_GAS) # focus on 1 (gas turned off)
table(gas_outage$UNABLEPAY_GAS) # focus on 2, 3, 4 (months with inability to pay gas bill)
table(gas_outage$OWE_GAS) # focus on >= 2 (money owed for gas)
table(gas_outage$INTER_GAS) # focus on 1 (yes interruption)

# compare to electricity out of curiosity
table(gas_outage$TURNOFF_ELEC) # focus on 1 (gas turned off)
table(gas_outage$UNABLEPAY_ELEC) # focus on 2, 3, 4 (months with inability to pay gas bill)
table(gas_outage$OWE_ELEC) # focus on >= 2 (money owed for gas)
table(gas_outage$INTER_ELEC) # focus on 1 (yes interruption)

# invest overlaps in gas outages
gas_outage_exp <- gas_outage %>% 
  select(hh_char, demographic, health, contains("_gas")) %>% 
  mutate(TURNOFF_GAS = ifelse(TURNOFF_GAS == 1, "TURNOFF_GAS", ""),
         TURNOFF_GAS = ifelse(is.na(TURNOFF_GAS), "", TURNOFF_GAS),
         UNABLEPAY_GAS = ifelse(UNABLEPAY_GAS >= 2, "UNABLEPAY_GAS", ""),
         UNABLEPAY_GAS = ifelse(is.na(UNABLEPAY_GAS), "", UNABLEPAY_GAS),
         OWE_GAS = ifelse(OWE_GAS >= 2, "OWE_GAS", ""),
         OWE_GAS = ifelse(is.na(OWE_GAS), "", OWE_GAS),
         INTER_GAS = ifelse(INTER_GAS == 1, "INTER_GAS", ""),
         INTER_GAS = ifelse(is.na(INTER_GAS), "", INTER_GAS))

gas_outage_exp <- gas_outage_exp %>% 
  rowwise() %>% 
  mutate(n_gas_problems = sum((c_across(contains("gas")) != ""))) %>% 
  ungroup() %>% 
  mutate(type_gas_issues = paste0(TURNOFF_GAS, " ", UNABLEPAY_GAS, " ", OWE_GAS, " ", INTER_GAS),
         type_gas_issues = trimws(type_gas_issues)) %>% 
  filter(type_gas_issues != "") %>% 
  mutate(across(!matches("type_gas_issues"), as.factor))

# Descriptive statistics table 1) --------------------------------------------------
# sometimes the stratification is too identifying bc there are only n = 1 for some combinations
# can use 3+ (and ask team)
table1(~ n_gas_problems, data = gas_outage_exp)
table1(~ QBORO + HH_CHILD17_dich + HH_CHILD4_dich + HH_ADULT60 + EDUCAT5 + HHPOV200 + NEIGH_POV + RACEETH | n_gas_problems, data = gas_outage_exp)
table1(~ GENHEALTH + DIABETES + HEART + HYPERTEN + RESP + KIDNEY + COGNITIVE + MENTAL + EME | n_gas_problems, data = gas_outage_exp) 

table1(~ type_gas_issues, data = gas_outage_exp)
table1(~ QBORO + HH_CHILD17_dich + HH_CHILD4_dich + HH_ADULT60 + EDUCAT5 + HHPOV200 + NEIGH_POV + RACEETH | type_gas_issues, data = gas_outage_exp)
table1(~ GENHEALTH + DIABETES + HEART + HYPERTEN + RESP + KIDNEY + COGNITIVE + MENTAL + EME | type_gas_issues, data = gas_outage_exp) 








