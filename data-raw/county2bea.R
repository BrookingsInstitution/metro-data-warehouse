## code to prepare `county2bea` dataset goes here

# BEA modifications =======================================================
# BEA counties and county equivalents are identified by five-digit Federal
# Information Processing Standard (FIPS) codes. The first two digits are the
# FIPS state code and the last three digits are the county code within the
# state. BEA has created modified FIPS codes for the following conditions:
#
# Kalawao County, Hawaii is combined with Maui County and combined area is
# designated 15901.
#
# The independent cities of Virginia with populations of less than 100,000 have
# been combined with an adjacent county and given codes beginning with 51901. In
# the name of the combined area, the county name appears first and is followed
# by the city name(s).
#
# Menominee County, Wisconsin is combined with Shawano County for 1969–1988 as
# 55901. Separate estimates for Menominee and Shawano Counties begin in 1989.


# MAIN ===================================================================

library(dplyr)
library(tidyr)
library(stringr)
source("R/readxl_online.R")

# download census-bea county crosswalk from BEA ---------------
url <- "https://apps.bea.gov/regional/xls/FIPSModifications.xlsx"
bea_county <- readxl_online(url, skip = 1,col_types = "text")
names(bea_county) <- c("stco_bea_fips","stco_bea_name","stco_fips","co_name")

# download BEA GDP by county -------------------------
# 2012-2015; https://www.bea.gov/data/gdp/gdp-county
# next release: Dec.12, 2019
gdp_url <- "https://www.bea.gov/system/files/2018-12/GCP_Release_1.xlsx"

get_county.gdp <- function(url) {
  tmp <- readxl_online(url, skip = 3)

  tmp %>%

    # rename columns
    select(
      stco_bea_fips = ...1,
      co_bea_name = ...2,
      st_name = ...3,
      ind_type = ...5,
      `2012`, `2013`, `2014`, `2015`
    ) %>%

    # reshape from wide to long
    gather(year, co_bea_gdp, `2012`:`2015`) %>%

    # remove source line, etc.
    filter(str_length(stco_bea_fips) == 5)
}

# update the county level gdp
county_gdp <- get_county.gdp(gdp_url)

# join with county2bea crosswalk
county2bea <- county_gdp %>%

  # filter for industry total and latest year
  filter(ind_type =="All Industries") %>%
  filter(year == max(as.numeric(year))) %>%

  # left join, clean columns
  left_join(bea_county, by = c("stco_bea_fips"))%>%
  mutate(stco_fips = ifelse(is.na(stco_fips),stco_bea_fips,stco_fips),
         co_name = ifelse(is.na(co_name),co_bea_name, co_name))%>%
  select(stco_bea_fips,co_bea_name,stco_fips,co_name,st_name,
         co_bea_gdp,ind_type,year)

# SAVE =============================================
usethis::use_data(county2bea,overwrite = TRUE)

# METADATA file
write_meta(df = county2bea,
           filename = "data-raw/county2bea.txt")

