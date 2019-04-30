library(dplyr)
library(stringr)
source("R/readxl_online.R")

# functions to update the file  ------------------------------

# get latest county population estimates from acs-5 year data using censusapi::
get_county.pop <- function(acs_year){
  getCensus(name = "acs/acs5",
            vintage = acs_year,
            vars = c("NAME","B01003_001E"),
            region = "county:*",
            key = key)%>%
    # take out PR
    filter(state != "72")%>%
    mutate(GEOID = paste0(str_pad(state,2,"left","0"),str_pad(county,3,"left","0")))
}

# get latest county employment estimates from county business pattern using censusapi::
get_county.emp <- function(cbp_year){
  getCensus(name = "cbp",
            vintage = cbp_year,
            vars = c("EMP", "GEO_TTL"),
            region = "county:*",
            key = key)%>%
    mutate(EMP = as.numeric(EMP),
           # fix two county names changes
           county = case_when(
             GEO_TTL == "Shannon County, South Dakota" ~ "102",
             GEO_TTL == "Wade Hampton Census Area, Alaska" ~ "158",
             TRUE ~ county
           ),
           GEOID = paste0(str_pad(state,2,"left","0"),str_pad(county,3,"left","0")))
}

# download and read xls file from: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
get_msa2county <- function(url){
  readxl_online(url,skip=2)%>%
    mutate(GEOID = paste0(str_pad(`FIPS State Code`,2,"left","0"),
                          str_pad(`FIPS County Code`,3,"left","0")))
}

get_county_urban_rural <- function(url){
  readxl_online(url)%>%
    mutate(COUNTY = case_when(
      STATENAME == "South Dakota" & COUNTYNAME == "Shannon" ~ "102",
      STATENAME == "Alaska" & COUNTYNAME == "Wade Hampton" ~ "158",
      TRUE ~ COUNTY),
      GEOID = paste0(STATE, COUNTY))%>%
    rename(pct.urban.county = POPPCT_URBAN)
}

