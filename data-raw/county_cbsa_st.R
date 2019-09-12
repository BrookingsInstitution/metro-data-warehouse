## code to prepare `county_cbsa_st` dataset goes here

# Author: Sifan Liu
# Date: Fri Apr 26 11:23:04 2019
# --------------
library(censusapi)
library(readxl)
library(dplyr)
library(stringr)

source("R/readxl_online.R")


# update delinations, population and employment from census ================
# Require api key from census, sign up here: https://api.census.gov/data/key_signup.html
key <- Sys.getenv("CENSUS_API_KEY")

# Get latest vintage, last updated at:
# Wed Apr 24 13:33:37 2019 ------------------------------
cbsa_url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list1_Sep_2018.xls"
urban_url <- "https://www2.census.gov/geo/docs/reference/ua/PctUrbanRural_County.xls"
cbp_year <- 2016
acs_year <- 2017

# functions to fetch data ------------------------------

# get latest county population estimates from acs-5 year data using censusapi::
get_county.pop <- function(acs_year) {
  getCensus(
    name = "acs/acs5",
    vintage = acs_year,
    vars = c("NAME", "B01003_001E"),
    region = "county:*",
    key = key
  ) %>%

    # take out Puerto Rico counties
    filter(state != "72") %>%
    mutate(stco_code = paste0(str_pad(state, 2, "left", "0"), str_pad(county, 3, "left", "0")))
}

# get latest county employment estimates from county business pattern using censusapi::
get_county.emp <- function(cbp_year) {
  getCensus(
    name = "cbp",
    vintage = cbp_year,
    vars = c("EMP", "GEO_TTL"),
    region = "county:*",
    key = key
  ) %>%
    mutate(
      EMP = as.numeric(EMP),

      # fix two county names changes
      county = case_when(
        GEO_TTL == "Shannon County, South Dakota" ~ "102",
        GEO_TTL == "Wade Hampton Census Area, Alaska" ~ "158",
        TRUE ~ county
      ),

      stco_code = paste0(str_pad(state, 2, "left", "0"), str_pad(county, 3, "left", "0"))
    )
}

# download and read xwalk file from: https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
get_msa2county <- function(url) {
  readxl_online(url, skip = 2) %>%
    mutate(stco_code = paste0(
      str_pad(`FIPS State Code`, 2, "left", "0"),
      str_pad(`FIPS County Code`, 3, "left", "0")
    ))
}

# download census urbanized area from: https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural/2010-urban-rural.html
get_county_urban_rural <- function(url) {
  readxl_online(url) %>%
    mutate(
      COUNTY = case_when(
        STATENAME == "South Dakota" & COUNTYNAME == "Shannon" ~ "102",
        STATENAME == "Alaska" & COUNTYNAME == "Wade Hampton" ~ "158",
        TRUE ~ COUNTY
      ),
      stco_code = paste0(STATE, COUNTY)
    ) %>%
    rename(pct.urban.county = POPPCT_URBAN)
}


# functions to classify types ---------------------------------------
def_metrotype <- function(df) {
  df %>%
    # code msa typopogy
    mutate(cbsa_type = case_when(
      `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" ~ "metro",
      `Metropolitan/Micropolitan Statistical Area` == "Micropolitan Statistical Area" ~ "micro",
      is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro"
    ))
}


def_metro100 <- function(df) {
  temp <- (df %>%
    select(cbsa_code, cbsa_pop) %>%
    unique() %>%
    filter(rank(desc(cbsa_pop)) <= 100))$cbsa_code

  # for metros, change cbsa_type to top100 if applies
  df <- df %>%
    mutate(cbsa_is.top100 = ifelse(cbsa_code %in% temp,T,F))
  return(df)
}


def_countytype <- function(df) {
  df %>%
    # The Brookings urban classification of counties
    # within the 100 largest metropolitan areas consists of

    # urban core (counties that are at least 95 percent urbanized);
    # mature suburbs (75 percent to 95 percent urbanized);
    # emerging suburbs (25 percent to 75 percent urbanized);
    # and exurbs (less than 25 percent urbanized).

    mutate(co_type = ifelse(cbsa_type == "top100",
      # in top 100, create new labels based on urbanized area
      case_when(
        co_pcturban >= 95 ~ "Urban cores",
        co_pcturban >= 75 & co_pcturban < 95 ~ "Mature suburbs",
        co_pcturban >= 25 & co_pcturban < 75 ~ "Emerging suburbs",
        co_pcturban < 25 ~ "Exurbs"
      ),
      # not in top 100
      case_when(
        cbsa_type == "metro" ~ "small metro counties",
        cbsa_type == "micro" ~ "micropolitan counties",
        cbsa_type == "nonmetro" ~ "rural counties"
      )
    ))
}


# function to construct the master file -----------------

# merge all files
county_cbsa_st <- get_county.pop(acs_year) %>%
  left_join(get_county.emp(cbp_year), by = "stco_code") %>%
  left_join(get_msa2county(cbsa_url), by = "stco_code") %>%
  left_join(get_county_urban_rural(urban_url), by = "stco_code") %>%

  # classify metro types
  def_metrotype() %>%

  # rename and keep only the selected columns
  select(
    stco_code,
    stco_name = NAME,
    co_pop = B01003_001E,
    co_emp = EMP,
    co_pcturban = pct.urban.county,
    cbsa_code = `CBSA Code`,
    cbsa_name = `CBSA Title`,
    cbsa_type
  ) %>%

  # Create state code and name from counties
  mutate(
    st_code = substr(stco_code, 1, 2),
    st_name = gsub(".+\\, ", "", stco_name)
  ) %>%

  # Construct cbsa and state sum from county population and employment
  group_by(cbsa_code) %>%
  mutate(
    cbsa_pop = case_when(!is.na(cbsa_code) ~ sum(co_pop)),
    cbsa_emp = case_when(!is.na(cbsa_code) ~ sum(co_emp))
  ) %>%
  group_by(st_code) %>%
  mutate(
    st_pop = sum(co_pop),
    st_emp = sum(co_emp)
  ) %>%
  ungroup() %>%

  # def other classifications
  def_metro100() %>%
  def_countytype() %>%

  # order columns
  select(contains("co_"), contains("st_"), contains("cbsa_"))


# generate codebook
county_cbsa_st <- county_cbsa_st %>%
  mutate_at(c("cbsa_type", "co_type"), as.factor)

skimr::skim(county_cbsa_st)

# save output
usethis::use_data(county_cbsa_st, overwrite = T)

cbsa_st <- county_cbsa_st %>%
  select(-contains("co_")) %>%
  filter(!is.na(cbsa_code))%>%
  unique()

usethis::use_data(cbsa_st, overwrite = T)

