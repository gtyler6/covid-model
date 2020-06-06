library(tidyverse)

outcomes <- read.csv("/Users/grahamtyler/Desktop/Github\ Repos/covid-model/outcomes-2020-04-26.csv")

outcomes$date <- as.Date(outcomes$date)

outcomes_summary <- outcomes %>%
  group_by(full_name, FIPS) %>%
  summarize(min_case = min(date[which(cases_raw > 0)], na.rm = T)
            , min_test = min(date[which(tests_raw >0)], na.rm = T)
            , max_death = max(deaths_raw, na.rm = T))

outcomes_summary_check <- outcomes_summary %>%
  filter(min_case <= as.Date("2020-03-05")
         , max_death > 500
         , !is.na(min_test))


qplot(data = outcomes %>% filter(FIPS == 36059), x=date, y=cases_raw)

nassau <- outcomes %>% 
  filter(FIPS == 36059) %>%
  select(full_name, FIPS, pop_2019, date, cases_raw, deaths_raw, tests_raw, new_cases_raw, new_deaths_raw, new_tests_raw)

write.csv(nassau, "/Users/grahamtyler/Desktop/nassau.csv")

