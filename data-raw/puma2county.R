## code to prepare `puma2county` dataset goes here

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


# 3) puma2county


puma2county_raw <- read_csv("data-raw/puma2county_raw.csv")

puma2county <- puma2county_raw %>%

  # remove the long-form variable names in the second row
  slice(-1) %>%

  # because of this second header, numerics were coerced to character
  # change them back
  mutate_at(.vars = vars(contains("pop"),
                         contains("afact"),
                         contains("AFACT")),
            .funs = as.numeric) %>%


  # select needed columns and rename
  select(stco_code = county14,
         st_code = state,
         st_ab = stab,
         puma_code = puma12,
         puma_name = PUMAname,
         stco_name = cntyname2,
         pl_pop16 = pop16,
         afact1 = afact,
         afact2 = AFACT2)


skimr::skim(puma2county)

usethis::use_data(puma2county, overwrite = T)
