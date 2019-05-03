## code to prepare `county_cbsa_st` dataset goes here

# Author: Sifan Liu
# Date: Fri Apr 26 11:23:04 2019
# --------------
library("censusapi")
library("dataMaid")

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
    select(stco_fips = GEOID,
           co_name = NAME,
           co_pop = B01003_001E,
           co_emp = EMP,
           co_pcturban = pct.urban.county,
           cbsa_code = `CBSA Code`,
           cbsa_name = `CBSA Title`,
           cbsa_type)%>%

    # Create state code and name from counties
    mutate(st_fips = substr(stco_fips,1,2),
           st_name = gsub(".+\\, ","",co_name))%>%

    # Construct cbsa and state sum from county population and employment
    group_by(cbsa_code)%>%
    mutate(cbsa_pop = case_when(!is.na(cbsa_code) ~ sum(co_pop)),
           cbsa_emp = case_when(!is.na(cbsa_code) ~ sum(co_emp))
    )%>%
    group_by(st_fips)%>%
    mutate(st_emp = sum(co_pop),
           st_emp = sum(co_emp))%>%
    ungroup()%>%

    # def other classifications
    def_metrosize%>%
    def_metro100%>%
    def_countytype%>%

    # order columns
    select(contains("co_"),contains("st_"),contains("cbsa_"))

  # save output
   return(county_cbsa_st)

}

# UPDATE! ============================================
county_cbsa_st <- update.master()
# generate codebook
dataMaid::makeDataReport(county_cbsa_st,
                         mode = "summarize",
                         render = F,
                         replace = T, listChecks = F, codebook = T)

# save output
usethis::use_data(county_cbsa_st, overwrite = T)

