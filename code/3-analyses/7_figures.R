# goal is to generate figures for the paper
# count of outages experienced
# count of outage types
# 7/8/24

# read in libraries
rm(list=ls(all=TRUE))
library(here)
source(here("code", "0-setup", "libraries.R"))
library(scales)

# prevalence of outage count experienced ----------------------------------
n_outages_n <- read_csv(paste0(here(), "/output/results/n_outages_n.csv"))

# create barplot using n_outages_n where
# n_outages is the x axis
# pct_total is the bar height
# pct_lci is the lower error bar
# pct_uci is the upper error bar
ggplot() +
  geom_bar(data = n_outages_n,
           aes(x = n_outages, y = pct_total),
           stat = "identity",
           fill = "lightgrey") +
  geom_errorbar(data = n_outages_n,
                aes(x = n_outages, ymin = pct_lci, ymax = pct_uci),
                width = 0.2) +
  geom_text(
    data = n_outages_n,
    aes(
      x = n_outages,
      y = pct_uci,
      label = paste0(
        round(pct_total, 0),
        " (",
        round(pct_lci, 0),
        "-",
        round(pct_uci, 0),
        ")"
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  labs(x = "Number of utility outage types",
       y = "Percent") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# save output 
ggsave(paste0(here(), "/output/figures/n_outages_n.png"), width = 10, height = 6)


# prevalence of outage type -----------------------------------------------
dist_serv_outages_n <- read_csv(paste0(here(), "/output/results/dist_serv_outages_n.csv"))

# create barplot using output2 where
# n_outages is the x axis
# pct_total is the bar height
# pct_lci is the lower error bar
# pct_uci is the upper error bar
dist_serv_outages_n <- dist_serv_outages_n %>% 
  filter(service_outage != "EPI_SERVICES",
         service_outage != "INTER_ELEC_GAS") %>% 
  mutate(service_outage = case_when(service_outage == "INTER_ELEC" ~ "Electricity",
                                    service_outage == "INTER_GAS" ~ "Gas",
                                    service_outage == "INTER_HEAT" ~ "Heat",
                                    service_outage == "INTER_WATER" ~ "Water"))


ggplot() +
  geom_bar(data = dist_serv_outages_n,
           aes(x = service_outage, y = pct_total),
           stat = "identity") +
  geom_errorbar(
    data = dist_serv_outages_n,
    aes(x = service_outage, ymin = pct_lci, ymax = pct_uci),
    width = 0.2
  ) +
  geom_text(
    data = dist_serv_outages_n,
    aes(
      x = service_outage,
      y = pct_uci,
      label = paste0(
        round(pct_total, 0),
        " (",
        round(pct_lci, 0),
        "-",
        round(pct_uci, 0),
        ")"
      )
    ),
    vjust = -0.5,
    size = 5
  ) +
  labs(x = "Service outage type",
       y = "Percent") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# save output
ggsave(paste0(here(), "/output/figures/dist_serv_outages_n.png"), width = 10, height = 6)
