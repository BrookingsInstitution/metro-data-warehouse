## code to prepare `naics_xwalk` dataset goes here
source("data-raw/fetch_new_data.R")

# 1997 - 2017 correspondance ==============================================================
url_naics <- "https://www.census.gov/eos/www/naics/concordances/2017_to_2012_NAICS.xlsx"
# 97-12, plus traded sectors
url_cluster <- "http://clustermapping.us/sites/default/files/files/page/US%20Cluster%20Definitions.xlsx"

naics_xwalk <- function(url_naics,url_cluster){
  tmp <- get_naics_17_12(url_naics)%>%
    full_join(get_clustermapping(url_cluster), by = c("code.naics6.2017"="code.naics6.2012"))

  bind_rows(
    # codes that didn't change
    tmp %>%
      filter(!is.na(code.naics6.all)),

    # use 2017-2012 corrspondance to define new NAICS codes
    tmp %>%
      filter(is.na(code.naics6.all))%>%
      select(code.naics6.2017,name.naics6.2017,code.naics6.2012)%>%
      left_join(tradable_industries,by = "code.naics6.2012")%>%
      select(-contains("2012"))%>%
      unique()
  )%>%
    mutate(code.naics6.all = ifelse(is.na(code.naics6.all),code.naics6.2017,code.naics6.all),
           name.naics6.all = ifelse(is.na(name.naics6.all),name.naics6.2017,name.naics6.all))%>%
    select(code.naics6.all,code.naics6.2017,code.naics6.2012,code.naics6.2007,code.naics6.2002,code.naics6.2002,code.naics6.1997,
           name.naics6.all,name.naics6.2017,traded.naics6, contains("cluster"))%>%
    arrange(desc(traded.naics6),code.naics6.all)
}

naics_xwalk <- naics_xwalk(url_naics,url_cluster)

usethis::use_data(naics_xwalk)
