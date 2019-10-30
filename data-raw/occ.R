library(dplyr)

src_xwalk <- "https://www.bls.gov/soc/2018/soc_2010_to_2018_crosswalk.xlsx"
src18 <- "https://www.bls.gov/soc/2018/soc_structure_2018.xlsx"
source("R/readxl_online.R")

soc_xwalk <- readxl_online(src_xwalk, skip = 7)
soc_18 <- readxl_online(src18, skip = 7)

soc2_18 <- soc_18 %>%
  filter(!is.na(`Major Group`))%>%
  select(soc2_code = `Major Group`, soc2_name = ...5) %>%
  mutate(soc2_shortname = gsub(" Occupations", "",soc2_name))

usethis::use_data(soc2_18)

soc6_18 <- soc_18 %>%
  filter(!is.na(`Detailed Occupation`))%>%
  select(soc6_code = `Detailed Occupation`, soc6_name = ...5)

usethis::use_data(soc6_18)

