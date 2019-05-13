##============================================================================##
# Clean common outputs from MABLE Geocorr 2018
#
# David Harshbarger
# 03 May 2019
##============================================================================##

##============================================================================##
# SETUP
##============================================================================##

library(dplyr)
library(tidyr)
library(readr)

##============================================================================##
# load raw data for three common crosswalks
##============================================================================##

# 1) tract2place
# 2) zcta2county
# 3) place2county

tract2place_raw <- read_csv("data-raw/tract2place_raw.csv")
zcta2county_raw <- read_csv("data-raw/zcta2county_raw.csv")
place2county_raw <- read_csv("data-raw/place2county_raw.csv")

##============================================================================##
# clean data according to common conventions
##============================================================================##

## GEOCORR NAMING CONVENTIONS:
# variable names will be coded as "geography_description" as appropriate
# aggregate fips codes to the most specific level (e.g., 11-digit codes for tracts)
# always use leading zeros and format fips codes as character, and only use digits
# place the full code for the SOURCE geography in column 1 (corresponds to afact1)
# place the full code for the TARGET geography in column 2 (corresponds to afact2)

tract2place <- tract2place_raw %>%

  # remove the long-form variable names in the second row
  slice(-1) %>%

  # because of this second header, numerics were coerced to character
  # change them back
  mutate_at(.vars = vars(contains("pop"),
                         contains("afact"),
                         contains("AFACT")),
            .funs = as.numeric) %>%

  # remove the period from the tract column. Need escape characters
  mutate(tract = gsub(pattern = "\\.",
                      replacement = "",
                      x = tract)) %>%

  # unite tract fips with state and county fips
  unite(col = stcotract_fips,
        county, tract,
        sep = "",
        remove = TRUE) %>%

  # unite state fips with place fips
  unite(col = stpl_fips,
        state, placefp,
        sep = "",
        remove = FALSE) %>%

  # select needed columns and rename
  select(stcotract_fips,
         stpl_fips,
         st_fips = state,
         st_ab = stab,
         co_name = cntyname,
         pl_name = placenm,
         tract_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)

# repeat process for other 2 files

zcta2county <- zcta2county_raw %>%

  # remove the long-form variable names in the second row
  slice(-1) %>%

  # because of this second header, numerics were coerced to character
  # change them back
  mutate_at(.vars = vars(contains("pop"),
                         contains("afact"),
                         contains("AFACT")),
            .funs = as.numeric) %>%

  # select needed columns and rename
  select(zcta_fips = zcta5,
         stcofips = county,
         st_fips = state,
         st_ab = stab,
         co_name = cntyname,
         zcta_name = zipname,
         zcta_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)

place2county <- place2county_raw %>%

  # remove the long-form variable names in the second row
  slice(-1) %>%

  # because of this second header, numerics were coerced to character
  # change them back
  mutate_at(.vars = vars(contains("pop"),
                         contains("afact"),
                         contains("AFACT")),
            .funs = as.numeric) %>%

  # unite state fips with county fips
  unite(col = stpl_fips,
        state, placefp,
        sep = "",
        remove = FALSE) %>%

  # select needed columns and rename
  select(stpl_fips,
         stcofips = county,
         st_fips = state,
         st_ab = stab,
         pl_name = placenm,
         co_name = cntyname,
         pl_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)

