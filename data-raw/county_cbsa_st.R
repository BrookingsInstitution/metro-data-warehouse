## code to prepare `county_cbsa_st` dataset goes here

# Author: Sifan Liu
# Date: Fri Apr 26 11:23:04 2019
# --------------
pkgs <- c('tidyverse',"censusapi")

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  }

# update delinations, population and employment from census ================

# Require api key from census
# sign up here: https://api.census.gov/data/key_signup.html
key <- Sys.getenv("CENSUS_API_KEY")

# Latest vintage
# Wed Apr 24 13:33:37 2019 ------------------------------
url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"
cbp_year <- 2016
acs_year <- 2017


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

update.county_cbsa_st <- function(url, cbp_year, acs_year){

  # update all three inputs
  county.pop <- get_county.pop(acs_year)
  county.emp <- get_county.emp(cbp_year)
  msa2county <- get_msa2county(url)

  # merge three files
  county_cbsa_st <- county.pop %>%
    left_join(county.emp, by = "GEOID")%>%
    left_join(msa2county, by = "GEOID") %>%

    # code msa typopogy
    mutate(type.cbsa = case_when(
      `Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area" ~ "metro",
      `Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area" ~ "micro",
      is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro",
      TRUE ~ "NA"
    ))%>%

    # rename and keep only the selected columns
    select(code.county = GEOID,
           name.county = GEO_TTL,
           population.county = B01003_001E,
           employment.county = EMP,
           code.cbsa = `CBSA Code`,
           name.cbsa = `CBSA Title`,
           type.cbsa)%>%

    # Create state code and name from counties
    mutate(code.state = substr(code.county,1,2),
           name.state = gsub(".+\\, ","",name.county))%>%

    # Construct cbsa and state sum from county population and employment
    group_by(code.cbsa)%>%
    mutate(population.cbsa = sum(population.county),
           employment.cbsa = sum(employment.county))%>%
    group_by(code.state)%>%
    mutate(population.state = sum(population.county),
           employment.state = sum(employment.county))%>%
    ungroup()

  # save output
   return(county_cbsa_st)

}


# UPDATE!
county_cbsa_st <- update.county_cbsa_st(url, cbp_year, acs_year)

usethis::use_data("county_cbsa_st")
