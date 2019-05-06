## code to prepare `county2BEAcounty` dataset goes here

library(dplyr)
library(tidyr)
library(stringr)
source("R/readxl_online.R")


url <- "https://apps.bea.gov/regional/xls/FIPSModifications.xlsx"
tmp <- readxl_online(url, skip = 1)

# BEA GDP by county, 2012-2015; https://www.bea.gov/data/gdp/gdp-county
# next release: Dec.12, 2019
gdp_url <- "https://www.bea.gov/system/files/2018-12/GCP_Release_1.xlsx"

get_county.gdp <- function(url) {
  tmp <- readxl_online(url, skip = 3)

  tmp %>%

    # rename columns
    select(
      stco_fips = ...1,
      co_name = ...2,
      st_name = ...3,
      ind_type = ...5,
      `2012`, `2013`, `2014`, `2015`
    ) %>%

    # reshape from wide to long
    gather(year, co_gdp, `2012`:`2015`) %>%

    # remove source line, etc.
    filter(str_length(stco_fips) == 5)
}

# update the county level gdp
county_gdp <- get_county.gdp(gdp_url)

# use employment as weights?

usethis::use_data("county2BEAcounty")
