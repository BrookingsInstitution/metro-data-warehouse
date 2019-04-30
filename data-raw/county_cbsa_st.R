## code to prepare `county_cbsa_st` dataset goes here

# Author: Sifan Liu
# Date: Fri Apr 26 11:23:04 2019
# --------------
library("censusapi")
source("data-raw/fetch_new_data.R")
source("data-raw/classify_geo.R")

# update delinations, population and employment from census ================
# Require api key from census, sign up here: https://api.census.gov/data/key_signup.html
key <- Sys.getenv("CENSUS_API_KEY")

# Get latest vintage, last updated at:
# Wed Apr 24 13:33:37 2019 ------------------------------
cbsa_url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"
urban_url <- "https://www2.census.gov/geo/docs/reference/ua/PctUrbanRural_County.xls"
cbp_year <- 2016
acs_year <- 2017

# function to construct the master file -----------------
update.master <- function(){

  # merge all files
  county_cbsa_st <- get_county.pop(acs_year) %>%
    left_join(get_county.emp(cbp_year), by = "GEOID") %>%
    left_join(get_msa2county(cbsa_url), by = "GEOID") %>%
    left_join(get_county_urban_rural(urban_url), by = "GEOID")%>%

    # classify metro types
    def_metrotype%>%

    # rename and keep only the selected columns
    select(code.county = GEOID,
           name.county = GEO_TTL,
           population.county = B01003_001E,
           employment.county = EMP,
           pct.urban.county,
           code.cbsa = `CBSA Code`,
           name.cbsa = `CBSA Title`,
           type.cbsa)%>%

    # Create state code and name from counties
    mutate(code.state = substr(code.county,1,2),
           name.state = gsub(".+\\, ","",name.county))%>%

    # Construct cbsa and state sum from county population and employment
    group_by(code.cbsa)%>%
    mutate(population.cbsa = case_when(!is.na(code.cbsa) ~ sum(population.county)),
           employment.cbsa = case_when(!is.na(code.cbsa) ~ sum(employment.county))
    )%>%
    group_by(code.state)%>%
    mutate(population.state = sum(population.county),
           employment.state = sum(employment.county))%>%
    ungroup()%>%

    # def other classifications
    def_metro100%>%
    def_metrosize%>%
    def_countytype

  # save output
   return(county_cbsa_st)

}

# UPDATE! ============================================
county_cbsa_st <- update.master()
# generate codebook
dataMaid::makeDataReport(county_cbsa_st,
                         # mode = "summarize",
                         replace = T, listChecks = F, codebook = T)
# save output
usethis::use_data(county_cbsa_st)
