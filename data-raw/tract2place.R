## code to prepare `tract2place` dataset goes here

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
tract2place_raw <- read_csv("data-raw/tract2place_raw.csv")
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

skimr::skim(tract2place)
usethis::use_data(tract2place)
