## code to prepare `naics_xwalk` dataset goes here
source("R/readxl_online.R")
library(tidyverse)

# 1997 - 2017 correspondance ==============================================================
url_naics <- "https://www.census.gov/eos/www/naics/concordances/2017_to_2012_NAICS.xlsx"
# 97-12, plus traded sectors
url_cluster <- "http://clustermapping.us/sites/default/files/files/page/US%20Cluster%20Definitions.xlsx"


# functions to get data ---------------------------------------
get_naics_17_12<- function(url){
  tmp <- readxl_online(url, skip = 2,col_type = 'text')%>%
    # remove columns contain only nas
    select_if(~sum(!is.na(.)) > 0)

  names(tmp) <- c("code_naics6_2017","name_naics6_2017","code_naics6_2012","name_naics6_2012")
  return(tmp)
}

get_clustermapping <- function(url){
  tmp <- readxl_online(url,sheet = "NAICS",col_types = "text")
  names(tmp) <- c("code_naics6_all","code_naics6_2012","code_naics6_2007","code_naics6_2002","code_naics6_1997",
                  "name_naics6_all", "traded.naics6",
                  "code.cluster.naics6","name.cluster.naics6","code.subcluster.naics6","name.subcluster.naics6")
  return(tmp)

}

# function to merge everything ---------------------------------------

naics_17_12 <- get_naics_17_12(url_naics)
naics_12_07 <- get_clustermapping(url_cluster)

naics_all <- naics_17_12 %>%
    full_join(naics_12_07, by = c("code_naics6_2017"="code_naics6_2012"))


naics_xwalk <- bind_rows(
    # codes that didn't change
    naics_all %>%
      filter(!is.na(code_naics6_all)),

    # use 2017-2012 corrspondance to define new NAICS codes using 2012 code
    naics_all %>%
      filter(is.na(code_naics6_all))%>%
      select(code_naics6_2017,name_naics6_2017,code_naics6_2012)%>%
      left_join(naics_12_07,by = "code_naics6_2012")%>%
      select(-contains("2012"))%>%
      unique()
  )%>%
    mutate(traded.naics6 = as.factor(traded.naics6),
           code_naics6_all = ifelse(is.na(code_naics6_all),code_naics6_2017,code_naics6_all),
           name_naics6_all = ifelse(is.na(name_naics6_all),name_naics6_2017,name_naics6_all))%>%
    select(code_naics6_all,code_naics6_2017,code_naics6_2012,code_naics6_2007,code_naics6_2002,code_naics6_2002,code_naics6_1997,
           name_naics6_all,name_naics6_2017,traded.naics6, contains("cluster"))%>%
    arrange(desc(traded.naics6),code_naics6_all)

# save
skimr::skim(naics_xwalk)
usethis::use_data(naics_xwalk)
