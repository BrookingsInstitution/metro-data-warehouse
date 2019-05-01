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

get_naics <- function(url){
  tmp <- readxl_online(url)%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)%>%
    select(-1)

  names(tmp) <- c("code.naics","name.naics")
  return(tmp)
}


get_naics_17_12<- function(url){
  tmp <- readxl_online(url, skip = 2,col_type = 'text')%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)

  names(tmp) <- c("code.naics6.2017","name.naics6.2017","code.naics6.2012","name.naics6.2012")
  return(tmp)
}

get_clustermapping <- function(url){
  tmp <- readxl_online(url,sheet = "NAICS")
  names(tmp) <- c("code.naics6.all","code.naics6.2012","code.naics6.2007","code.naics6.2002","code.naics6.1997",
                  "name.naics6.all", "traded.naics6",
                  "code.cluster.naics6","name.cluster.naics6","code.subcluster.naics6","name.subcluster.naics6")
  return(tmp)

}
