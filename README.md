# covid-model
Repo for Stan model estimating true underlying cases and predicting spread of COVID-19 as a function of relevant factors. Incudes associated datasets, visualizations, EDA. 

**Data Overview**

County Covid outcomes data -- outcomes-(date).csv 
- Cases, deaths from NYT: https://github.com/nytimes/covid-19-data
- Tests from Corona Data Scraper: https://coronadatascraper.com/#home
  + Also have cases and deaths data available
- Also have cases, deaths data available from Johns Hopkins: https://github.com/CSSEGISandData/COVID-19
- COVID Outcomes Data Comparison.Rmd explores and compares the three different data sources
- COVID Outcomes Data Clean.R downloads latest files, merges them, removes likely incorrect data, interpolates missing and incorrect data, and adds accurate population estimate 

County population estimates, births, deaths, migration -- census_2019_pop_estimate: 
- 2019 county population estimate from Census: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/co-est2019-alldata.pdf
- Currently just using 2019 population estimates

County demographics data -- acs.csv 
- 2014-2018 5-year average demographics by county from Census ACS
- ACS Data Clean.R script pulls data through Census API, defines and calculates variables

Merged data -- county_data-(date).csv
- Dataset Join.R joins all datasets, can be updated as data sources are added
- Resulting file is massive if including all data for each date-county row (> 2 GB), so resulting file is not included in repo
