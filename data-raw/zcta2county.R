## code to prepare `zcta2county_raw` dataset goes here

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

zcta2county_raw <- read_csv("data-raw/zcta2county_raw.csv")

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
  select(zcta_code = zcta5,
         stco_code = county,
         st_code = state,
         st_ab = stab,
         stco_name = cntyname,
         zcta_name = zipname,
         zcta_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)

skimr::skim(zcta2county)
usethis::use_data(zcta2county_raw, overwrite = T)
