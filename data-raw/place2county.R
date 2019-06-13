## code to prepare `place2county` dataset goes here

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


# 3) place2county


place2county_raw <- read_csv("data-raw/place2county_raw.csv")

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
         stco_fips = county,
         st_fips = state,
         st_ab = stab,
         pl_name = placenm,
         co_name = cntyname,
         pl_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)


skimr::skim(place2county)

usethis::use_data(place2county, overwrite = T)
