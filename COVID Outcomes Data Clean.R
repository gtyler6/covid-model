# Setup 
options(stringsAsFactors = F, digits = 3)

library(tidyverse)
library(zoo)
library(sf)

### Import data

# Download time series data from Corona Data Scraper. Need to unzip file to pull latest file directly from their site. 
temp <- tempfile()
download.file("https://coronadatascraper.com/timeseries-tidy.csv.zip", temp)
cds <- read_csv(unz(temp, "timeseries-tidy.csv"))
unlink(temp)

# Download current US counties dataset from NYT github. 
nyt <- read_csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))

# Download current US confirmed cases and US deaths datasets from Jonhs Hopkins github.
jh_confirmed_us <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"))

jh_deaths_us <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"))


### CDS Cleanup

# Widen dataframe to create separate columns for cases, deaths, tests, etc. 
cds <- cds %>% 
  pivot_wider(names_from = type, values_from = value)

# Filter to US only at the county level. 
cds <- cds %>%
  filter(country == "United States" & level == "county")

# Remove aggregated Dukes and Nantucket County. We will just use the separate entries for Dukes County and Nantucket County. 
cds <- cds %>% 
  filter(name != "Dukes County, Nantucket County, Massachusetts, United States")

# Drop the Economic Region data from Alaska. 
cds <- cds %>%
  filter(!grepl("Economic Region", name))


### JH Cleanup

# Generate character vector of dates corresponding to JH column names.
dates <- str_remove(str_replace_all(as.character(format(seq.Date(as.Date("2020/1/22"), as.Date(Sys.Date()-1), by = "day"), "%m/%d/%y")), "/0", "/"), "^0+")

# Lengthen JH datasets to have a row for each date for each county. 
jh_confirmed_us <- jh_confirmed_us %>%
  pivot_longer(cols = all_of(dates), names_to = "date", values_to = "cases")

jh_deaths_us <- jh_deaths_us %>%
  pivot_longer(cols = all_of(dates), names_to = "date", values_to = "deaths")

# Combine the two JH datasets.
jh <-jh_confirmed_us %>% left_join(select(jh_deaths_us, UID, date, Population, deaths), by = c("UID", "date"))

# Check to make sure nothing is lost in the join. 
anti_join(jh_confirmed_us, jh_deaths_us, by = c("UID", "date"))

# Hard code a FIPS of 29000 for KC (which I've confirmed does not belong to any other county). 
jh <- jh %>% 
  mutate(FIPS = ifelse(is.na(FIPS) & Admin2=="Kansas City" & Province_State=="Missouri", 29000, FIPS))

# Now remove all NA rows to trim dataset to established counties with FIPS + KC. 
jh <- jh %>% drop_na()

# Remove Unassigned  and "Out of" county designations. Note that as of 4/25, JH no longer has aggregated Dukes and Nantucket entries. 
jh <- jh %>%
  filter(Admin2 != "Unassigned") %>%
  filter(!grepl("Out of", Admin2))

# Add leading zero to all JH FIPS codes with only 4 digits and adjust date format to match NYT.
jh <- jh %>% mutate(FIPS = as.character(ifelse(nchar(FIPS) == 4, paste0("0", FIPS), FIPS))
                    , date = as.Date(date, format = "%m/%d/%y"))

# Trim down the JH dataset to only the relevant columns and rename them.
jh <- jh %>% select(UID, FIPS, full_name = Combined_Key, country = Country_Region, state = Province_State, county = Admin2, date
                    , cases, deaths, population.jh = Population, lat = Lat, long = Long_) 


### NYT Cleanup
# Hard code KC FIPS that we used for JH dataset and hard code NYC value. 
nyt <- nyt %>% 
  mutate(fips = ifelse(is.na(fips) & county=="New York City" & state=="New York", "36061", fips)) %>%
  mutate(fips = ifelse(is.na(fips) & county=="Kansas City" & state=="Missouri", "29000", fips))

# Drop Unknown cases for each state not assigned to a county.   
nyt <- nyt %>%
  filter(county != "Unknown")


### Joins
# Check what's in NYT, but missing from JH. Summarize by county. On 4/25: no data in NYC for 4/24 for JH. 
print(anti_join(nyt, jh, by = c("fips" = "FIPS" , "date")) %>%
        group_by(county, state) %>% 
        summarise(count = n(), cases = sum(cases), deaths = sum(deaths)) %>%
        arrange(desc(count))
      , n=Inf)

# Join NYT data on FIPS and date. 
df <- jh %>% left_join(select(nyt, fips, date, cases, deaths), by = c("FIPS" = "fips", "date"), suffix = c(".jh", ".nyt"))

# Clean up CDS county and state names to be able to join. 
county_header <- c(" County| Parish| Borough| Census Area| Municipality")

cds <- cds %>%
  mutate(county = ifelse(state == "Alaska" & county == "Juneau City and Borough", "Juneau", county)) %>%
  mutate(county = ifelse(state == "Alaska" & county == "Sitka City and Borough", "Sitka", county)) %>%
  mutate(county = ifelse(state == "Alaska" & county == "Wrangell City and Borough", "Wrangell", county)) %>%
  mutate(county = ifelse(state == "Alaska" & county == "Yakutat City and Borough", "Yakutat", county)) %>%
  
  mutate(county = ifelse(state == "Virginia" & county == "Charles City County", "Charles City", county)) %>%
  mutate(county = ifelse(state == "Virginia" & county == "James City County", "James City", county)) %>%
  
  mutate(county = ifelse(state == "New Mexico" & county == "Doña Ana County", "Dona Ana", county)) %>%
  
  mutate(state = ifelse(state == "Washington, D.C." & county == "District of Columbia", "District of Columbia", state)) %>%
  
  mutate(county = ifelse(name %in% c("Baltimore City, Maryland, United States", "Carson City, Nevada, United States", "Charles City County, Virginia, United States"
                                     , "Fairfax City, Virginia, United States", "Franklin City, Virginia, United States", "James City County, Virginia, United States"
                                     , "Kansas City, Missouri, United States", "Richmond City, Virginia, United States", "Roanoke City, Virginia, United States"
                                     , "St. Louis City, Missouri, United States"), county, str_remove_all(county, " City"))) %>%
  
  mutate(county = str_remove_all(county, county_header))

# Check what's in CDS, but missing from combined JH/NYT. Summarize by county: 
print(anti_join(cds, df, by = c("state", "county", "date")) %>%
        group_by(county, state) %>% 
        summarise(count = n(), tests = sum(tested)) %>%
        arrange(desc(count))
      , n=Inf)

# Check what's in JH/NYT, but not CDS. Only include entries with at least one case or death in the NYT data. Only go up to two days ago because these datasets get updated at different times. 
print(anti_join(df %>% filter(cases.nyt + deaths.nyt >0), cds, by = c("state", "county", "date")) %>%
        group_by(full_name) %>% 
        filter(date < as.Date(Sys.Date()-1)) %>%
        summarise(count = n()) %>%
        arrange(desc(count))
)

# Join CDS data to JH/NYT data. 
df <- df %>% left_join(select(cds, county, state, date, cases.cds = cases, deaths.cds = deaths, tested.cds = tested, hospitalized.cds = hospitalized, recovered.cds = recovered)
                       , by = c("state", "county", "date"), suffix = c("", ".cds"))

# Count total counties in dataset as a reference. Should be 3,143. 
n_county <- df %>% summarize(n_distinct(full_name))
n_county


### Correcting missing and incorrectly decreasing data. 

# Step by step process: 

# 1. Determine if value for given date > most recent total. If so, drop that value. If we have an incorrect low total for the most recent value, this will throw out previous estimates. However, the hope is that the most recent data is the most accurate and that this issue will be taken care of when we rerun this in the future after data is retroactively corrected. Might consider making this something like whether value is > than yesterday's total. Note that we do not throw out data if the current total is missing. 

# 2. Determine if value for given date > max value over next 5 days. If so, drop that value. Similar logic as 1, but this handles outlier high values that don't happen to be greater than the most recent total. There will be very few cases for which 1 is true and 2 is NOT true, but it is theoretically possible and the logic is that we should categorically throw out values that are greater than current total. 

# 3. After dropping values for above two situations, drop any value that is < the max at any point up to that date. This handles the majority of situations: the assumed reason for a decrease in value is that an incorrectly LOW value was entered for a single day or string of days. 

# 4. Linear interpolation of dropped values using the closest non-missing values. 

# 5. Fill missing values at the front end of the dataset with 0. 

# 6. Fill missing values at the tail end of the datset with the most recent value. 

# Steps 1 to 3 to identify which values to throw out as likely errors. Do this for NYT cases and deaths, and for CDS tests. 
df <- df %>%
  group_by(FIPS, full_name) %>%
  arrange(FIPS, full_name, date) %>%
  
  # Step 1: drop values > current total.
  
  # Set current values for each variable and a new _adj variable to save the new values for each, so   that we can preserve original data. 
  mutate(current_cases_total.nyt = cases.nyt[which(date == max(date))]
         , current_deaths_total.nyt = deaths.nyt[which(date == max(date))]
         , cases_adj.nyt = cases.nyt
         , deaths_adj.nyt = deaths.nyt
         
         , current_tested_total.cds = tested.cds[which(date == max(date))]
         , tested_adj.cds = tested.cds) %>%
  
  # Drop values meeting criteria. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > current_cases_total.nyt, NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > current_deaths_total.nyt, NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > current_tested_total.cds, NA, tested_adj.cds)) %>%
  # Step 2: drop values > max of next 5 days. 
  
  # Set max of next 5 days for each variable. 
  mutate(max_cases_next5.nyt = rollapply(cases_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         , max_deaths_next5.nyt = rollapply(deaths_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         
         , max_tested_next5.cds = rollapply(tested_adj.cds, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)) %>%
  
  # Drop variables meeting criteria. We will cycle through each variable 5 times so that if there are strings over incorrectly high entries for multiple days in a row, we make sure to throw all of them out. 
  # Pass 1
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > max_cases_next5.nyt & !is.na(max_cases_next5.nyt), NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > max_deaths_next5.nyt & !is.na(max_deaths_next5.nyt), NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > max_tested_next5.cds & !is.na(max_tested_next5.cds), NA, tested_adj.cds)) %>%
  
  # Pass 2 - recalculate max next 5. 
  mutate(max_cases_next5.nyt = rollapply(cases_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         , max_deaths_next5.nyt = rollapply(deaths_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         
         , max_tested_next5.cds = rollapply(tested_adj.cds, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)) %>%
  
  # Pass 2 - drop values. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > max_cases_next5.nyt & !is.na(max_cases_next5.nyt), NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > max_deaths_next5.nyt & !is.na(max_deaths_next5.nyt), NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > max_tested_next5.cds & !is.na(max_tested_next5.cds), NA, tested_adj.cds)) %>%
  
  # Pass 3 - recalculate max next 5. 
  mutate(max_cases_next5.nyt = rollapply(cases_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         , max_deaths_next5.nyt = rollapply(deaths_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         
         , max_tested_next5.cds = rollapply(tested_adj.cds, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)) %>%
  
  # Pass 3 - drop values. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > max_cases_next5.nyt & !is.na(max_cases_next5.nyt), NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > max_deaths_next5.nyt & !is.na(max_deaths_next5.nyt), NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > max_tested_next5.cds & !is.na(max_tested_next5.cds), NA, tested_adj.cds)) %>%
  
  # Pass 4 - recalculate max next 5. 
  mutate(max_cases_next5.nyt = rollapply(cases_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         , max_deaths_next5.nyt = rollapply(deaths_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         
         , max_tested_next5.cds = rollapply(tested_adj.cds, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)) %>%
  
  # Pass 4 - drop values. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > max_cases_next5.nyt & !is.na(max_cases_next5.nyt), NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > max_deaths_next5.nyt & !is.na(max_deaths_next5.nyt), NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > max_tested_next5.cds & !is.na(max_tested_next5.cds), NA, tested_adj.cds)) %>%
  
  # Pass 5 - recalculate max next 5. 
  mutate(max_cases_next5.nyt = rollapply(cases_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         , max_deaths_next5.nyt = rollapply(deaths_adj.nyt, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)
         
         , max_tested_next5.cds = rollapply(tested_adj.cds, list((1:5)), max, na.rm = TRUE, fill=NA, align="left", partial=TRUE)) %>%
  
  # Pass 5 - drop values. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt > max_cases_next5.nyt & !is.na(max_cases_next5.nyt), NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt > max_deaths_next5.nyt & !is.na(max_deaths_next5.nyt), NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds > max_tested_next5.cds & !is.na(max_tested_next5.cds), NA, tested_adj.cds)) %>%
  
  # Step 3: drop values < previous max to date
  # Set max to date. . 
  mutate(max_cases_to_date.nyt = cummax(ifelse(is.na(cases_adj.nyt), -Inf, cases_adj.nyt))
         , max_deaths_to_date.nyt = cummax(ifelse(is.na(deaths_adj.nyt), -Inf, deaths_adj.nyt))
         
         , max_tested_to_date.cds = cummax(ifelse(is.na(tested_adj.cds), -Inf, tested_adj.cds))) %>%
  
  # Drop values meeting criteria. 
  mutate(cases_adj.nyt = ifelse(cases_adj.nyt < max_cases_to_date.nyt, NA, cases_adj.nyt)
         , deaths_adj.nyt = ifelse(deaths_adj.nyt < max_deaths_to_date.nyt, NA, deaths_adj.nyt)
         
         , tested_adj.cds = ifelse(tested_adj.cds < max_tested_to_date.cds, NA, tested_adj.cds))

# Linearly interpolate missing values in between two known values. 
df <- df %>%
  group_by(FIPS, full_name) %>%
  arrange(FIPS, full_name, date) %>%
  mutate(cases_adj_int.nyt = floor(na.approx(cases_adj.nyt, na.rm = FALSE))
         , deaths_adj_int.nyt = floor(na.approx(deaths_adj.nyt, na.rm = FALSE))
         
         , tested_adj_int.cds = floor(na.approx(tested_adj.cds, na.rm = FALSE)))

# Fill down missing entries at the end of the data with the most recent count that we have in the data. 
df <- df %>%
  group_by(FIPS, full_name) %>%
  arrange(FIPS, full_name, date) %>%
  fill(c(cases_adj_int.nyt, deaths_adj_int.nyt, tested_adj_int.cds))

# Fill initial missing values up to first case, death, or test with 0. 
df <- df %>%
  mutate_at(vars(cases_adj_int.nyt, deaths_adj_int.nyt, tested_adj_int.cds), ~replace_na(., 0))

# Create new daily amounts 
df <- df %>% ungroup() %>%
  mutate(new_cases_adj_int.nyt = cases_adj_int.nyt - lag(ifelse(is.na(cases_adj_int.nyt), 0, cases_adj_int.nyt))
         , new_deaths_adj_int.nyt = deaths_adj_int.nyt - lag(ifelse(is.na(deaths_adj_int.nyt), 0, deaths_adj_int.nyt))
         , new_tests_adj_int.cds = tested_adj_int.cds - lag(ifelse(is.na(tested_adj_int.cds),0,tested_adj_int.cds))
         
         , new_cases_adj.nyt = cases_adj.nyt - lag(cases_adj.nyt)
         , new_deaths_adj.nyt = deaths_adj.nyt - lag(deaths_adj.nyt)
         , new_tests_adj.cds = tested_adj.cds - lag(tested_adj.cds)

         , new_cases.nyt = cases.nyt - lag(cases.nyt)
         , new_deaths.nyt = deaths.nyt - lag(deaths.nyt)
         , new_tests.cds = tested.cds - lag(tested.cds))

# Import 2019 population estimates from census 
pop_census <- read_csv("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/census_2019_pop_estimate.csv")

# Update FIPS to join data
pop_census <- pop_census %>%
  mutate(FIPS = paste0(as.character(ifelse(nchar(STATE) == 1, paste0("0", STATE), STATE)), 
                       as.character(ifelse(nchar(COUNTY) == 1, paste0("00", COUNTY)
                                           , ifelse(nchar(COUNTY) == 2, paste0("0", COUNTY), COUNTY)))))

# Trim pop_census to relevant variables for future use
pop_census <- pop_census %>%
  select(STNAME, CTYNAME, FIPS
         , POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016, POPESTIMATE2017, POPESTIMATE2018, POPESTIMATE2019
         , NPOPCHG_2019
         , BIRTHS2013, BIRTHS2014, BIRTHS2015, BIRTHS2016, BIRTHS2017, BIRTHS2018, BIRTHS2019
         , DEATHS2013, DEATHS2014, DEATHS2015, DEATHS2016, DEATHS2017, DEATHS2018, DEATHS2019
         , RDEATH2013, RDEATH2014, RDEATH2015, RDEATH2016, RDEATH2017, RDEATH2018, RDEATH2019
         , NETMIG2013, NETMIG2014, NETMIG2015, NETMIG2016, NETMIG2017, NETMIG2018, NETMIG2019
         , GQESTIMATES2019)

# Check for anything in df not in pop_census. 
anti_join(df, pop_census, by = "FIPS")

# Check for anything in pop_census not in df. Should only be the 50 rows for state totals. 
anti_join(pop_census, df, by = "FIPS")

# Join on FIPS to get population data
df <- df %>% left_join(select(pop_census, FIPS, POPESTIMATE2019), by = "FIPS")

# Calculate per capita numbers. 
df <- df %>%
  mutate(cases_r.nyt = cases.nyt / POPESTIMATE2019
         , cases_adj_r.nyt = cases_adj.nyt / POPESTIMATE2019
         , cases_adj_int_r.nyt = cases_adj_int.nyt / POPESTIMATE2019
         
         , deaths_r.nyt = deaths.nyt / POPESTIMATE2019
         , deaths_adj_r.nyt = deaths_adj.nyt / POPESTIMATE2019
         , deaths_adj_int_r.nyt = deaths_adj_int.nyt / POPESTIMATE2019
         
         , tested_r.cds = tested.cds / POPESTIMATE2019
         , tested_adj_r.cds = tested_adj.cds / POPESTIMATE2019
         , tested_adj_int_r.cds = tested_adj_int.cds / POPESTIMATE2019
         
         , new_cases_r.nyt = new_cases.nyt / POPESTIMATE2019
         , new_cases_adj_r.nyt = new_cases_adj.nyt / POPESTIMATE2019
         , new_cases_adj_int_r.nyt = new_cases_adj_int.nyt / POPESTIMATE2019
         
         , new_deaths_r.nyt = new_deaths.nyt / POPESTIMATE2019
         , new_deaths_adj_r.nyt = new_deaths_adj.nyt / POPESTIMATE2019
         , new_deaths_adj_int_r.nyt = new_deaths_adj_int.nyt / POPESTIMATE2019
         
         , new_tests_r.cds = new_tests.cds / POPESTIMATE2019
         , new_tests_adj_r.cds = new_tests_adj.cds / POPESTIMATE2019
         , new_tests_adj_int_r.cds = new_tests_adj_int.cds / POPESTIMATE2019 )


# Trim outcomes dataset to relevant variables
outcomes <- df %>%
  select(full_name, FIPS, state, county, date, pop_2019 = POPESTIMATE2019
         # Case data: 
         # Raw, incorrect data removed, and full data with interpolated values
         # Cumulative total by day, new, per capita total, per capita new
         , cases_raw = cases.nyt
         , cases_adj = cases_adj.nyt
         , cases_full = cases_adj_int.nyt
         , new_cases_raw = new_cases.nyt
         , new_cases_adj = new_cases_adj.nyt
         , new_cases_full = new_cases_adj_int.nyt
         , rate_cases_raw = cases_r.nyt 
         , rate_cases_adj = cases_adj_r.nyt 
         , rate_cases_full = cases_adj_int_r.nyt 
         , r_new_cases_raw = new_cases_r.nyt 
         , r_new_cases_adj = new_cases_adj_r.nyt 
         , r_new_cases_full = new_cases_adj_int_r.nyt 
         # Deaths data
         , deaths_raw = deaths.nyt
         , deaths_adj = deaths_adj.nyt
         , deaths_full = deaths_adj_int.nyt
         , new_deaths_raw = new_deaths.nyt
         , new_deaths_adj = new_deaths_adj.nyt
         , new_deaths_full = new_deaths_adj_int.nyt
         , rate_deaths_raw = deaths_r.nyt 
         , rate_deaths_adj = deaths_adj_r.nyt 
         , rate_deaths_full = deaths_adj_int_r.nyt 
         , r_new_deaths_raw = new_deaths_r.nyt 
         , r_new_deaths_adj = new_deaths_adj_r.nyt 
         , r_new_deaths_full = new_deaths_adj_int_r.nyt 
         # Tests data
         , tests_raw = tested.cds
         , tests_adj = tested_adj.cds
         , tests_full = tested_adj_int.cds
         , new_tests_raw = new_tests.cds
         , new_tests_adj = new_tests_adj.cds
         , new_tests_full = new_tests_adj_int.cds
         , rate_tests_raw = tested_r.cds 
         , rate_tests_adj = tested_adj_r.cds 
         , rate_tests_full = tested_adj_int_r.cds 
         , r_new_tests_raw = new_tests_r.cds 
         , r_new_tests_adj = new_tests_adj_r.cds 
         , r_new_tests_full = new_tests_adj_int_r.cds )

# Export to git directory
write_csv(outcomes, paste0("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/outcomes-",as.character(Sys.Date()),".csv"))



