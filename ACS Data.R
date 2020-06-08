library(tidyverse)
library(tidycensus)
library(tigris)
library(plotly)
library(sf)

census_api_key("700bf5181c794dd3d94c328aa8ffeb4161137d17")

options(tigris_use_cache = TRUE)

view_acs_18 <- load_variables(2018, "acs5", cache = TRUE)

acs_vars <- c(total_pop = "B01003_001"
          
          , male = "B01001_002"
          
          #Will generate "other" from total minus sum
          , white = "B03002_003"
          , black = "B03002_004"
          , asian = "B03002_006"
          , hispanic = "B03002_012"
          
          , median_age = "B01002_001"
          
          #Will add male and female to get age groups
          , male_0_4 = "B01001_003"
          , male_5_9 = "B01001_004"
          , male_10_14 ="B01001_005"
          , male_15_17="B01001_006"
          , male_18_19="B01001_007"
          , male_20="B01001_008"
          , male_21="B01001_009"
          , male_22_24="B01001_010"
          , male_25_29="B01001_011"
          , male_30_34="B01001_012"
          , male_35_39="B01001_013"
          , male_40_44="B01001_014"
          , male_45_49="B01001_015"
          , male_50_54="B01001_016"
          , male_55_59="B01001_017"
          , male_60_61="B01001_018"
          , male_62_64="B01001_019"
          , male_65_66="B01001_020"
          , male_67_69="B01001_021"
          , male_70_74="B01001_022"
          , male_75_79="B01001_023"
          , male_80_84="B01001_024"
          , male_85_plus="B01001_025"
          
          , female_0_4 = "B01001_027"
          , female_5_9 = "B01001_028"
          , female_10_14 ="B01001_029"
          , female_15_17="B01001_030"
          , female_18_19="B01001_031"
          , female_20="B01001_032"
          , female_21="B01001_033"
          , female_22_24="B01001_034"
          , female_25_29="B01001_035"
          , female_30_34="B01001_036"
          , female_35_39="B01001_037"
          , female_40_44="B01001_038"
          , female_45_49="B01001_039"
          , female_50_54="B01001_040"
          , female_55_59="B01001_041"
          , female_60_61="B01001_042"
          , female_62_64="B01001_043"
          , female_65_66="B01001_044"
          , female_67_69="B01001_045"
          , female_70_74="B01001_046"
          , female_75_79="B01001_047"
          , female_80_84="B01001_048"
          , female_85_plus="B01001_049"
          
          , median_income = "B19013_001"
          
          #Those under poverty level in last 12 months; total count is the total in that table to derive poverty rate. 
          , poverty = "B17005_002"
          #Will compare this total count to total population count. 
          , poverty_total_count = "B17005_001"
          
          #Education level: 25 and over; can also get this with B15003_ variables, but longer. 
          , less_hs = "B23006_002"
          , hs_grad = "B23006_009"
          , some_college = "B23006_016"
          , ba_higher = "B23006_023"
          #Use this total as denominator to get % for education level. 
          , total_25_over_ed = "B23006_001"
          
          #16 and over; unemployed = in civilian labor force and unemployed; out_labor_force = not in labor force
          , unemployed = "B23025_005"
          , out_labor_force = "B23025_007"
          #Use this total in civilian labor force to get % unemployed among those in labor force. 
          , total_civilian_labor_force = "B23025_002"
          #Use this total as denominator to get % ue + not in labor force out of total pop 16+.
          , total_16_over_employment = "B23025_001"
          
          #Measures in housing units; can compare to total pop for ratio and land area for density
          #Add rental units of 50+ to owned units of 50+ to count number of very large housing units
          , housing_units = "B25001_001"
          , own_50plus_units = "B25032_010"
          , rent_50plus_units = "B25032_021"
          
          #Total pop in occupied housing; homeles = total - pop in occupied housing (I think)
          , pop_occupied_housing = "B25008_001"
        
          #Percent of those in housing who rent is: renters / pop_occupied_housing
          , renters = "B25008_003"
          
          #Total population in occupied housing by unit size; just pulling 5+ unit size
          , own_5plus_units = "B25033_005"
          , rent_5plus_units = "B25033_011"
          #Use this as denominator to get ratio of those in housing who are in large units
          , total_occupied_housing_unitsize = "B25033_001"
          
          #Household size in people
          , avg_household_size = "B25010_001"
          
          #18 and over
          , live_alone = "B09021_002"
          #Use this as denom for % of those 18+ living alone
          , total_18plus_live_alone = "B09021_001"
          
          #Add all of these up and divide by total pop to get uninsured rate. 
          , male_0_6_nohealth = "B27001_005"
          , male_6_18_nohealth = "B27001_008"
          , male_19_25_nohealth = "B27001_011"
          , male_26_34_nohealth = "B27001_014"
          , male_35_44_nohealth = "B27001_017"
          , male_45_54_nohealth = "B27001_020"
          , male_55_64_nohealth = "B27001_023"
          , male_65_74_nohealth = "B27001_026"
          , male_75_plus_nohealth = "B27001_029"
          
          , female_0_6_nohealth = "B27001_033"
          , female_6_18_nohealth = "B27001_036"
          , female_19_25_nohealth = "B27001_039"
          , female_26_34_nohealth = "B27001_042"
          , female_35_44_nohealth = "B27001_045"
          , female_45_54_nohealth = "B27001_048"
          , female_55_64_nohealth = "B27001_051"
          , female_65_74_nohealth = "B27001_054"
          , female_75_plus_nohealth = "B27001_057"
          
          #This should count all individuals living in "limited English speaking" households.
          , pop_limited_english_households = "B16003_001"
          
          #I think this is total households and then total households that only speak English. 
          , total_households = "C16002_001"
          , hh_english_only = "C16002_002"
          #Will add these up and divide by total households to get % of households that are limited English speaking. 
          , hh_spanish_limited_english = "C16002_004"
          #Indo European languages
          , hh_ie_limited_english = "C16002_007"
          #Asian Pacific languages
          , hh_ap_limited_english = "C16002_010"
          , hh_other_limited_english = "C16002_013"
          )

acs_raw_CA <- get_acs(state = "CA", geography = "county", 
                  variables = acs_vars, year = 2018, output = "wide", geometry = TRUE)

#We are going to use the estimates for each variable, which are in columns using the "E" suffix, 
#as opposed to the "M" suffix denoting the margin of error for the estimate. 
#Rename these column names to remove the "E" at the end for ease of use. 
#First, rename the county column and extract just the county name to new column.
acs_raw_CA <- acs_raw_CA %>% rename(county_full = NAME)
acs_raw_CA <- acs_raw_CA %>%
  mutate(county_name = str_extract(county_full, "[^,]+"))

acs_raw_CA <- acs_raw_CA %>% rename_at(vars(ends_with("E")), 
                                       funs(str_replace(., "E","")))
#Check column names. 
str(acs_raw_CA)

#Combining variables and computing rates as needed to get relevant variables. 
CA_acs <- acs_raw_CA %>%
  mutate(other_race = total_pop - white - black - asian - hispanic
         , age_0_14 = male_0_4 + male_5_9 + male_10_14 + 
                       female_0_4 + female_5_9 + female_10_14 
         , age_15_24 = male_15_17 + male_18_19 + male_20 + male_21 + male_22_24 +
                        female_15_17 + female_18_19 + female_20 + female_21 + female_22_24
         , age_25_34 = male_25_29 + male_30_34 +
                        female_25_29 + female_30_34
         , age_35_44 = male_35_39 + male_40_44 + 
                        female_35_39 + female_40_44
         , age_45_54 = male_45_49 + male_50_54 +
                        female_45_49 + female_50_54
         , age_55_64 = male_55_59 + male_60_61 + male_62_64 +
                        female_55_59 + female_60_61 + female_62_64
         , age_65_74 = male_65_66 + male_67_69 + male_70_74 +
                        female_65_66 + female_67_69 + female_70_74 
         , age_75_84 = male_75_79 + male_80_84 + 
                        female_75_79 + female_80_84
         , age_85_plus = male_85_plus + female_85_plus
         
         , poverty_rate = poverty / poverty_total_count
         
         , less_hs_rate = less_hs / total_25_over_ed
         , hs_grad_rate = hs_grad / total_25_over_ed
         , some_college_rate = some_college / total_25_over_ed
         , ba_higher_rate = ba_higher / total_25_over_ed
         
         , unemployed_rate = unemployed / total_civilian_labor_force
         , unemployed_out_labor_rate = (unemployed + out_labor_force) / total_16_over_employment
        
         , housing_units_rate = housing_units / total_pop
         , homeless = total_pop - pop_occupied_housing
         , homeless_rate = (total_pop - pop_occupied_housing) / total_pop
         , renter_rate = renters / pop_occupied_housing
         , pop_large_housing = own_5plus_units + rent_5plus_units
         , pop_large_housing_rate = (own_5plus_units + rent_5plus_units)/total_occupied_housing_unitsize
         
         , live_alone_rate = live_alone / total_18plus_live_alone
         
         , uninsured_total = male_0_6_nohealth + male_6_18_nohealth + male_19_25_nohealth + 
                               male_26_34_nohealth + male_35_44_nohealth + male_45_54_nohealth + 
                               male_55_64_nohealth + male_65_74_nohealth + male_75_plus_nohealth + 
                               female_0_6_nohealth + female_6_18_nohealth + female_19_25_nohealth + 
                               female_26_34_nohealth + female_35_44_nohealth + female_45_54_nohealth + 
                               female_55_64_nohealth + female_65_74_nohealth + female_75_plus_nohealth
         , uninsured_rate = (male_0_6_nohealth + male_6_18_nohealth + male_19_25_nohealth + 
           male_26_34_nohealth + male_35_44_nohealth + male_45_54_nohealth + 
           male_55_64_nohealth + male_65_74_nohealth + male_75_plus_nohealth + 
           female_0_6_nohealth + female_6_18_nohealth + female_19_25_nohealth + 
           female_26_34_nohealth + female_35_44_nohealth + female_45_54_nohealth + 
           female_55_64_nohealth + female_65_74_nohealth + female_75_plus_nohealth) 
            / total_pop
         
         , individual_limited_english_rate = pop_limited_english_households / total_pop
         
         , hh_limited_english_rate = (hh_spanish_limited_english + hh_ie_limited_english +
           hh_ap_limited_english +  hh_other_limited_english) /  total_households        
          ) %>%
  
  select(GEOID, county_full, county_name, geometry
         , total_pop 
         
         #Race
         , white, black, asian, hispanic, other_race
         
         #Age
         , median_age
         , age_0_14, age_15_24, age_25_34, age_35_44, age_45_54, age_55_64
         , age_65_74, age_75_84, age_85_plus
         
         #Income and poverty
         , median_income
         #poverty_rate = poverty / poverty_total_count
         #***Should compare poverty_total_count to total_pop to see if they are the same. 
         , poverty_rate, poverty, poverty_total_count
         
         #Education rates are out of 25+ age population. 
         #***Should compare total_25_over_ed to adding up the age groups to see if they are the same. 
         , less_hs_rate, hs_grad_rate, some_college_rate, ba_higher_rate, total_25_over_ed
         
         #Employment rates are out of 16+ age population. 
         #unemployed_rate is those in the labor force who are unemployed -- denominator is total in civilian labor force.
         #unemployed_out_labor_rate is unemployed + out of the labor force out of total 16+ population. 
         , unemployed_rate, unemployed_out_labor_rate
         
         #housing_units_rate = total units / total population
         , housing_units_rate, housing_units
         
         #homeless = total population - population in occupied housing (pop_occupied_housing)
         , homeless, homeless_rate, pop_occupied_housing
         
         #renter rate = renters / pop_occupied_housing
         , renter_rate
         
         #pop_large_housing is total number of people in spaces with 5+ units
         , pop_large_housing 
         #***rate divides by total_occupied_housing_unitsize; compare this to pop_occupied_housing to see if they are the same
         , pop_large_housing_rate, total_occupied_housing_unitsize
         
         #Live alone is for all 18+
         , live_alone_rate
         
         #Uninsured rate is out of total population. Have it by age group and sex if needed. 
         , uninsured_rate, uninsured_total
         
         #individual_limited_english_rate is (I think) indviduals living in limited English speaking households with denominator of total_pop.
         #hh_limited_english_rate is the percentage of households that are limited English speaking out of total households. 
         , individual_limited_english_rate, hh_limited_english_rate 
  )

#Now I need to get land area to compute population and housing density. 
CA_area <- counties(year = 2018, state = "CA", cb = TRUE, class = "sf", progress_bar = FALSE) %>%
  mutate(area = ALAND / 2589988) %>%
  select(GEOID, area)

CA_acs <- CA_acs %>% left_join(CA_area %>% st_drop_geometry(), by = "GEOID") %>%
  mutate(pop_density = total_pop / area
         , housing_density = housing_units / area)


CA_denom_check <- CA_acs %>%
  mutate(pov_total = poverty_total_count - total_pop
         , ed_total = total_25_over_ed - (age_25_34 + age_35_44 + 
                                            age_45_54 + age_55_64 + 
                                            age_65_74 + age_75_84 + age_85_plus)
         , ed_rate_sum = 1 - (less_hs_rate + hs_grad_rate + some_college_rate + ba_higher_rate)
         , occ_housing = total_occupied_housing_unitsize - pop_occupied_housing) %>%
  select(GEOID, county_full, county_name
         , pov_total, ed_total, ed_rate_sum, occ_housing) %>%
  filter(pov_total != 0 | ed_total != 0 | ed_rate_sum != 0 | occ_housing != 0)

CA_denom_check

#We see  from these checks that the two different denominators used for total people living in 
#occupied housing are the same (total_occupied_housing_unitsize = pop_occupied_housing)

#The education rates essentially all add up to 100%.

#When calculating the poverty rate: 
#the poverty_total_count = "B17005_001" (Estimate!Total for POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY EMPLOYMENT STATUS)
#is consistently less than the total population. Maybe this is just people of a certain age? 

#When calculating the rates of people over 25 falling into each education category,
#the total_25_over_ed = "B23006_001" (Estimate!Total for EDUCATIONAL ATTAINMENT BY EMPLOYMENT STATUS FOR THE POPULATION 25 TO 64 YEARS)
#is consistently less than adding up all of the age groups incorporating age 25+. 


