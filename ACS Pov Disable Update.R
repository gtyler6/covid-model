library(cdcfluview)
library(tidyverse)

cdc_pi <- pi_mortality('region', years=2019)


vprofile18 <- load_variables(2018, "acs5/profile")

perc_pov <- load_variables(2018, "acs5/profile") %>%
  filter(name %in% c("DP03_0128", "DP03_0128P", "DP02_0071", "DP02_0071P", "DP03_0096", "DP03_0096P"
                     , "DP02_0061", "DP02_0061P"
                     , "DP03_0005P"
                      ,"DP05_0038P"
                     , "DP02_0112P")) %>%
  pull(name)

pov_test <- get_acs(geography = 'county', variables = perc_pov, year = 2018)

# Get poverty rate, disability rate, unemployed, language spoken at home

acs_aut <- acs %>%
  filter(GEOID == "01003")