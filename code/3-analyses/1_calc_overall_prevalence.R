# try to use svydesign package to analyze survey data
# goal is to calculate overall prevalence of utility outages in nyc
# 12/4/23
# read more from here: https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/

# read in libraries
rm(list=ls(all=TRUE))
library(here)
source(here("code", "0-setup", "libraries.R"))

# read in data
# names(outage_dta)
outage_dta <- read_sas(paste0(here("data", "raw", "building_characteristics", "ei_analytic_team2.sas7bdat")))
names(outage_dta)
glimpse(outage_dta)


# Prep data ---------------------------------------------------------------

outage_dta <- transform(outage_dta, strata=as.character(strata))

# list of service outage types
service_outages <- c("INTER_ELEC", "INTER_GAS", "INTER_HEAT", "INTER_WATER", "INTER_ELEC_GAS")

outage_ext_factors <- outage_dta %>% 
  select(EnergyID, strata, final_wt, INTER_ELEC, INTER_GAS, INTER_WATER, INTER_HEAT) %>%
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


# count of service outages experienced ----------------------------------------------
# sum
svytotal(I(~n_outages=="0 outages"), design = svy_outage_ext_factors, na.rm=TRUE)
svytotal(I(~n_outages=="1 outage"), design = svy_outage_ext_factors, na.rm=TRUE)
svytotal(I(~n_outages=="2 outages"), design = svy_outage_ext_factors, na.rm=TRUE)
svytotal(I(~n_outages=="3+ outages"), design = svy_outage_ext_factors, na.rm=TRUE)


#pct
svyciprop(~I(n_outages=="0 outages"), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
svyciprop(~I(n_outages=="1 outage"), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
svyciprop(~I(n_outages=="2 outages"), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
svyciprop(~I(n_outages=="3+ outages"), svy_outage_ext_factors, method="xlogit", na.rm = T)*100


# outage type in nyc ------------------------------------------------------
# count
count_elec <- svytotal(I(~INTER_ELEC == 1), design = svy_outage_ext_factors, na.rm=TRUE)
count_gas <- svytotal(I(~INTER_GAS == 1), design = svy_outage_ext_factors, na.rm=TRUE)
count_heat <- svytotal(I(~INTER_HEAT == 1), design = svy_outage_ext_factors, na.rm=TRUE)
count_water <- svytotal(I(~INTER_WATER == 1), design = svy_outage_ext_factors, na.rm=TRUE)
count_elec_gas <- svytotal(I(~INTER_ELEC_GAS == 1), design = svy_outage_ext_factors, na.rm=TRUE)

#round to nearest 1,000
round(count_elec[[2]], -3)

#pct
pct_elec <- svyciprop(~I(INTER_ELEC == 1), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
pct_gas <- svyciprop(~I(INTER_GAS == 1), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
pct_heat <- svyciprop(~I(INTER_HEAT == 1), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
pct_water <- svyciprop(~I(INTER_WATER == 1), svy_outage_ext_factors, method="xlogit", na.rm = T)*100
pct_elec_gas <- svyciprop(~I(INTER_ELEC_GAS == 1), svy_outage_ext_factors, method="xlogit", na.rm = T)*100

#pct_95ci
pct_95ci_elec <- paste0(round(pct_elec[[1]], 5), " (", round(confint(pct_elec)[[1]]*100, 5), ", ", round(confint(pct_elec)[[2]]*100, 5), ")")
pct_95ci_gas <- paste0(round(pct_gas[[1]], 5), " (", round(confint(pct_gas)[[1]]*100, 5), ", ", round(confint(pct_gas)[[2]]*100, 5), ")")
pct_95ci_heat <- paste0(round(pct_heat[[1]], 5), " (", round(confint(pct_heat)[[1]]*100, 5), ", ", round(confint(pct_heat)[[2]]*100, 5), ")")
pct_95ci_water <- paste0(round(pct_water[[1]], 5), " (", round(confint(pct_water)[[1]]*100, 5), ", ", round(confint(pct_water)[[2]]*100, 5), ")")
pct_95ci_elec_gas <- paste0(round(pct_elec_gas[[1]], 5), " (", round(confint(pct_elec_gas)[[1]]*100, 5), ", ", round(confint(pct_elec_gas)[[2]]*100, 5), ")")

# combine data
outage_type_nyc <-
  as.data.frame(cbind(
    c("ELEC", "GAS", "HEAT", "WATER", "ELEC + GAS"),
    c(
      round(count_elec[[2]], -3),
      round(count_gas[[2]], -3),
      round(count_heat[[2]], -3),
      round(count_water[[2]], -3),
      round(count_elec_gas[[2]], -3)
    ),
    c(
      pct_95ci_elec,
      pct_95ci_gas,
      pct_95ci_heat,
      pct_95ci_water,
      pct_95ci_elec_gas
    )
  ))

write_csv(outage_type_nyc, paste0(here(), "/output/results/outage_type_nyc.csv"))

outage_type_nyc <- read_csv(paste0(here(), "/output/results/outage_type_nyc.csv"))

##### number of service outages - n, pct, ci, reliability -------------------------------------------------------
# eg 0 outages vs 2 outages

n_outages_n <- as.data.frame(svytotal(~n_outages, design = svy_outage_ext_factors, na.rm=TRUE))
n_study <- sum(outage_ext_factors$final_wt)
n_outages_n <- as.data.frame(n_outages_n) %>% 
  mutate(n_outages = case_when(row_number() == 1 ~ "0",
                               row_number() == 2 ~ "1",
                               row_number() == 3 ~ "2",
                               row_number() == 4 ~ "3+")) %>% 
  select(n_outages, everything()) %>% 
  mutate(pct_total = (total/n_study) * 100,
         lci = total - 1.96*SE,
         uci = total + 1.96*SE,
         pct_lci = lci/sum(n_outages_n$total) * 100,
         pct_uci = uci/sum(n_outages_n$total) * 100,
         rse = SE/total*100,
         reliability_asterik = case_when((rse >= 30 & rse < 50) ~ "*",
                                         (total < 50) ~ "*",
                                         ((pct_uci - pct_lci) < 6 & rse >= 50) ~ "*",
                                         ((pct_uci - pct_lci)/2 > 10) ~ "*",
                                         TRUE ~ ""),
         reliability_suppress = case_when((rse >= 50 & (uci - lci) >= 6) ~ "^",
                                          (total < 50 & pct_total == 0) ~ "^",
                                          (total < 50 & pct_total == 100) ~ "^",
                                          (SE == 0 & (uci - lci) == 0 & pct_total == 0) ~ "^",
                                          (SE == 0 & (uci - lci) == 0 & pct_total == 100) ~ "^",  
                                          TRUE ~ "")
  ) %>% 
  mutate(n_pct_ci = paste0(paste0(round(total), " (", round(pct_total, 0), "%, ", round(pct_lci, 0), "%-", round(pct_uci, 0), "%)", reliability_asterik, reliability_suppress))) #%>% 
  # select(n_outages, n_pct_ci)

n_outages_n

write_csv(n_outages_n, paste0(here(), "/output/results/n_outages_n.csv"))

##### types of service outages - n -------------------------------------------------------
# provides weighted N by level with standard error
weighted_n_type <- function(service_outage) {
  formula <- as.formula(paste("~", service_outage))
  condition <- as.formula(paste("~", service_outage, " == 1"))
  result <- as.data.frame(svytotal(formula, design = svy_outage_ext_factors, na.rm = TRUE, condition = condition))
  # return(result)
  total <- round(result[1, 1])
  se <- round(result[1, 2])
  
  output <- as.data.frame(cbind(service_outage, total, se))
  return(output)
}

output <- data.frame()
for (s in 1:length(service_outages)){
  print(service_outages[s])
  
  result <- weighted_n_type(service_outages[s])
  output <- rbind(output, result)
}

output2 <- output %>% 
  mutate(total = as.numeric(total),
         se = as.numeric(se),
         pct_total = (total/n_study) * 100,
         lci = total - 1.96*se,
         uci = total + 1.96*se,
         pct_lci = lci/n_study * 100,
         pct_uci = uci/n_study * 100,
         rse = se/total*100,
         reliability_asterik = case_when((rse >= 30 & rse < 50) ~ "*",
                                         (total < 50) ~ "*",
                                         ((pct_uci - pct_lci) < 6 & rse >= 50) ~ "*",
                                         ((pct_uci - pct_lci)/2 > 10) ~ "*",
                                         TRUE ~ ""),
         reliability_suppress = case_when((rse >= 50 & (uci - lci) >= 6) ~ "^",
                                          (total < 50 & pct_total == 0) ~ "^",
                                          (total < 50 & pct_total == 100) ~ "^",
                                          (se == 0 & (uci - lci) == 0 & pct_total == 0) ~ "^",
                                          (se == 0 & (uci - lci) == 0 & pct_total == 100) ~ "^",  
                                          TRUE ~ "")
  ) %>% 
  mutate(n = round(total, -3),
         pct_ci = paste0(round(pct_total), " (", round(pct_lci), "-", round(pct_uci), ")", reliability_asterik, reliability_suppress)) #%>% 
  # select(service_outage, n, pct_ci)

output2

# add epi services NYC/overall weighted counts (rounding to nearest 1,000)
# add new row for epi services
nyc_overall_weights <- paste0(round(sum(outage_ext_factors$final_wt), -3), " (100%)")

output2 <- output2 %>% 
  add_row(service_outage = "EPI_SERVICES", n = round(sum(outage_ext_factors$final_wt), -3), pct_ci = "(100%)")

write_csv(output2, paste0(here(), "/output/results/dist_serv_outages_n.csv"))

