## code to prepare `county_pres_votes` dataset goes here
county_pres_votes <- read.csv("V:/Sifan/R/xwalk/county vote history.csv")

library(dplyr)
library(stringr)

county_pres_votes <- county_pres_votes%>%
  mutate(stco_fips = str_pad(fip, 5, "left", pad = "0"))

usethis::use_data(county_pres_votes, overwrite = T)
