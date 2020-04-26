library(tidyverse)
library(zoo)
library(sf)

# Import outcomes
outcomes <- read_csv("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/outcomes-2020-04-26.csv")

# Import ACS
acs <- read_csv("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/acs.csv")


# Check what's in outcomes that's not in ACS
anti_join(outcomes, acs, by = c("FIPS" = "GEOID")) %>%
  group_by(full_name) %>%
  summarize(count = n())

# Check what's in ACS that's not in outcomes by state/territory. Should only be Puerto Rico. 
anti_join(acs, outcomes, by = c("GEOID" = "FIPS")) %>%
  group_by(str_extract(county_full, '\\b[^,]+$')) %>%
  summarize(count = n())

county_data <- outcomes %>% 
  left_join(select(acs, -c(county_full, county_name)), by = c("FIPS" = "GEOID")) %>%
  rename(pop_acsavg = total_pop)

# Export to git directory
write_csv(county_data, paste0("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/county_data-",as.character(Sys.Date()),".csv"))

