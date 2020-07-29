## code to prepare `naics` dataset goes here

library(dplyr)
library(stringr)
source("R/readxl_online.R")

# update
url <- "https://www.census.gov/eos/www/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx"

# naics xwalk 07-12-17 ----
naics1712 <- metro.data::readxl_online("https://www.census.gov/eos/www/naics/concordances/2017_to_2012_NAICS.xlsx", skip = 2) %>%
  mutate(naics6_code_17 = as.character(`2017 NAICS Code`),
         naics6_code_12 = as.character(`2012 NAICS Code`))

naics0712 <- metro.data::readxl_online("https://www.census.gov/eos/www/naics/concordances/2007_to_2012_NAICS.xls", skip = 2) %>%
  mutate(naics6_code_07 = as.character(`2007 NAICS Code`),
         naics6_code_12 = as.character(`2012 NAICS Code`))


# get naics data ----------------------

get_naics <- function(url) {
  tmp <- readxl_online(url) %>%
    # remove columns contain only nas
    select_if(~ sum(!is.na(.)) > 0) %>%
    select(-1)

  names(tmp) <- c("naics_code", "naics_name")
  return(tmp)
}


# wrangle to wide relation file
reshape_naics <- function(df) {

  df %>%
    filter(str_length(naics_code) == 6) %>%
    mutate(
      naics5_code = substr(naics_code, 1, 5),
      naics4_code = substr(naics_code, 1, 4),
      naics3_code = substr(naics_code, 1, 3),
      naics2_code = substr(naics_code, 1, 2)
    ) %>%
    # correct for naics2 aggregates
    mutate(naics2_code = case_when(
      naics2_code %in% c("31", "32", "33") ~ "31-33",
      naics2_code %in% c("44", "45") ~ "44-45",
      naics2_code %in% c("48", "49") ~ "48-49",
      T ~ naics2_code
    )) %>%

    # make naics6 as unique rows, other idustry level as corresponding columns
    left_join(tmp, by = c("naics5_code" = "naics_code"), suffix = c("", "5")) %>%
    left_join(tmp, by = c("naics4_code" = "naics_code"), suffix = c("", "4")) %>%
    left_join(tmp, by = c("naics3_code" = "naics_code"), suffix = c("", "3")) %>%
    left_join(tmp, by = c("naics2_code" = "naics_code"), suffix = c("6", "2")) %>%
    select(
      naics6_code = naics_code,
      contains("6"), contains("5"), contains("4"), contains("3"), contains("2")
    )
}

# add advanced industry categories
load("data/naics4_ai.rda")
naics4_ai <- naics4_ai %>%
  rename(naics_code = naics4_code)

load("../metro-datasets/R/naics_sc.rda")
naics_sc <- naics_sc %>%
  select(naics_code = naics6_code,
         sector = sector,
         supply_chain = sc,
         traded = traded)

# run this ==================================================
naics <-  naics %>%
  # get_naics(url) %>%
  mutate(naics_level = ifelse(grepl("-",naics_code),2,
                              str_length(naics_code))) %>%
  left_join(naics4_ai, by = "naics_code") %>%
  left_join(naics1712[c("naics6_code_17","naics6_code_12")], by = c("naics_code" = "naics6_code_17")) %>%
  left_join(naics_sc, by = c("naics6_code_12"="naics_code")) %>%
  left_join(naics0712[c("naics6_code_07","naics6_code_12")], by =  "naics6_code_12")

# save
usethis::use_data(naics, overwrite = T)
# usethis::use_data(naics4_ai, overwrite = T)

