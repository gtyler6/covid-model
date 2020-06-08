# Setup 
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

# Noting Census API Key. Should already be installed and cached from previous use. 
census_api_key("700bf5181c794dd3d94c328aa8ffeb4161137d17")

options(tigris_use_cache = T)

# Load census variable lists to view if needed. 
# ACS 5 variable list for 2014
vars_14 <- load_variables(2014, "acs5", cache = T)

# 2010 decennial census variable list
vars_10 <- load_variables(2010, "sf1", cache = T)

# Name ACS variables to include
acs_vars <- c(total_pop = "B01001_001")

# Name census variables to include
census_vars <- c(total_pop_2010 = "P001001")

# Set years for ACS 5. It only goes back to 2011 for zip code level. Will use 2010 census data for 2010 estimate. 
years_acs <- c(2011:2014)

# Set years for land area. It only goes back to 2014 for zip code. Will use the 2010 census for 2010 estimate. 
years_land <- c(2014:2014)

# Download ACS data by zip code for each year and variable in lists. 
acs_zip <- map_dfr(years_acs, 
                             ~ get_acs(
                               geography = "zcta",
                               variables = acs_vars,
                               year = .x,
                               output = "wide",
                               geometry = F) %>%
                               mutate(year_value = .x) %>%
                               select(year_value, zip = GEOID, total_popE, total_popM), 
                             .id = "year") %>%
  # Pivot data to wide format with one row per zip code and columns for each yearly estimate and margin of error. 
  pivot_wider(id_cols = zip, names_from = year_value, values_from = c(total_popE, total_popM)) %>%
  
  # Download 2010 decennial census data and join on zip code. 
  left_join(select(
    get_decennial(
      geography = "zcta",
      variables = census_vars,
      year = 2010,
      output = "wide",
      geometry = F), 
    zip = GEOID, total_pop_2010), 
    by = "zip"
    )

# Download land area for each zip code by year in list. 
acs_land <- map_dfr(years_land, 
                   ~ zctas(year = .x, 
                           cb = T, 
                           class = "sf",
                           progress_bar = F) %>%
                     st_drop_geometry() %>%
                     #Convert to square miles
                     mutate(area_land = ALAND10 / 2589988,
                            year_value = paste0("area_land_",.x)) %>%
                     select(year_value, zip = GEOID10, area_land), 
                   .id = "year") %>%
  # Pivot data to wide format with one row per zip code and columns for each yearly estimate 
  pivot_wider(id_cols = zip, names_from = year_value, values_from = area_land) %>%
  
  # Download 2010 decennial census area estimate and join on zip code. 
  left_join(zctas(year = 2010,
                  cb = T,
                  class = "sf",
                  progress_bar = F) %>%
              st_drop_geometry() %>%
              select(zip = ZCTA5, area_land_2010 = CENSUSAREA),
            by = "zip")

# Join variable estimates and area estimates
acs_zip <- acs_zip %>% left_join(acs_land, by = "zip")

write.csv(acs_zip, "/Users/grahamtyler/Desktop/Zip Code Population Estimates.csv", row.names = F)

