## code to prepare `naics` dataset goes here

library(dplyr)
library(stringr)
source("R/readxl_online.R")

# update
url <- "https://www.census.gov/eos/www/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx"

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
reshape_naics <- function(url) {
  tmp <- get_naics(url)

  tmp %>%
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
advanced_industries <- read.csv("V:/Sifan/R/xwalk/advanced industries.csv") %>%
  mutate(naics4_code = substr(NAICS, 1, 4)) %>%
  select(naics4_code, naics4_aitype = Type)

# run this ==================================================
naics <- reshape_naics(url) %>%
  left_join(advanced_industries, by = "naics4_code")

naics <- naics %>%
  left_join(naics_xwalk[c("code.naics6.2017","traded.naics6")],by = c("naics6_code" = "code.naics6.2017"))

naics <- naics %>%
  mutate(naics6_traded = as.factor(traded.naics6),
         naics4_aitype = as.factor(naics4_aitype))

# codebook
skimr::skim(naics)

naics4_ai <- naics %>%
  select(naics4_code, naics4_aitype) %>%
  unique()

# save
usethis::use_data(naics, overwrite = T)
usethis::use_data(naics4_ai, overwrite = T)

# dataMaid::makeDataReport(naics,render = F)
