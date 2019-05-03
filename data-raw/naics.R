## code to prepare `naics` dataset goes here

# update
library(dplyr)
source("R/readxl_online.R")
url <- "https://www.census.gov/eos/www/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx"

# get naics data ----------------------

get_naics <- function(url){
  tmp <- readxl_online(url)%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)%>%
    select(-1)

  names(tmp) <- c("code.naics","name.naics")
  return(tmp)
}


# wrangle to wide relation file
reshape_naics <- function(url){
  tmp <- get_naics(url)

  tmp %>%
  filter(str_length(code.naics)==6)%>%
  mutate(code.naics5 = substr(code.naics, 1,5),
         code.naics4 = substr(code.naics, 1,4),
         code.naics3 = substr(code.naics, 1,3),
         code.naics2 = substr(code.naics, 1,2))%>%
# correct for naics2 aggregates
  mutate(code.naics2 = case_when(code.naics2 %in% c("31","32","33") ~ "31-33",
                                 code.naics2 %in% c("44","45") ~ "44-45",
                                 code.naics2 %in% c("48","49") ~ "48-49",
                                 T ~ code.naics2))%>%
  left_join(tmp,by = c("code.naics5" = "code.naics"), suffix = c("","5"))%>%
  left_join(tmp,by = c("code.naics4" = "code.naics"), suffix = c("","4"))%>%
  left_join(tmp,by = c("code.naics3" = "code.naics"), suffix = c("","3"))%>%
  left_join(tmp,by = c("code.naics2" = "code.naics"), suffix = c("6","2"))%>%
  select(code.naics6 = code.naics,
         contains("6"),contains("5"),contains("4"),contains("3"),contains("2"))
}

# add advanced industries
advanced_industries <- read.csv("V:/Sifan/R/xwalk/advanced industries.csv")%>%
  mutate(code.naics4 = substr(NAICS,1,4))%>%
  select(code.naics4,type.naics4.ai = Type)

naics <- reshape_naics(url)%>%
  left_join(advanced_industries, by = "code.naics4")


usethis::use_data(naics,overwrite = T)




