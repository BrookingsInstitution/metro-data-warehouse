def_metrosize <- function(df){
  df %>%
    mutate(size.cbsa = case_when(
      population.cbsa > 1000000 ~ "Large metro",
      population.cbsa <= 1000000 & population.cbsa >= 250000 ~ "Medium metro",
      population.cbsa < 250000 ~ "Small metro",
      TRUE ~ "NA"))
}



def_metrotype <- function(df){
  df %>%
    # code msa typopogy
    mutate(type.cbsa = case_when(
      `Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area" ~ "metro",
      `Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area" ~ "micro",
      is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro",
      TRUE ~ "NA"
    ))
  }


def_metro100 <- function(df){
  temp <- (df %>%
    select(code.cbsa, population.cbsa) %>%
    unique()%>%
    filter(rank(desc(population.cbsa)) <=100))$code.cbsa

  df <- df%>%mutate(type.cbsa = case_when(code.cbsa %in% temp ~ "top100",
                                          T ~ type.cbsa))
  return(df)
  }

def_countytype <- function(df){
  df %>%
    # create county type labels, using Alan.B methodology (see below)
    mutate(type.county = ifelse(type.cbsa == "top100",
                                # in top 100, create new labels based on urbanized area
                                case_when(
                                  pct.urban.county>99 ~ "Urban counties",
                                  pct.urban.county>95 & pct.urban.county<=99~ "High-density suburban counties",
                                  pct.urban.county>75 & pct.urban.county<=95~ "Mature suburban counties",
                                  pct.urban.county>25 & pct.urban.county<=75~ "Emerging suburban counties",
                                  pct.urban.county<=25~ "Exurban counties"),
                                # not in top 100, keep original type labels
                                paste0(type.cbsa," counties")))
}

# Alan's methodology =========================================
# types <- data.frame(stringsAsFactors=FALSE,
#                     county.type = c("Urban", "High-density suburban", "Mature suburban",
#                                     "Emerging suburban", "Exurban", "Small metro",
#                                     "Micropolitan", "Rural"),
#                     code = c("UR", "HD", "MS", "ES", "EX", "SM", "MI", "RU"),
#                     labels = c("Counties in top 100 metro areas > 99% urbanized",
#                                "Counties in top 100 metro areas 95% to 99% urbanized",
#                                "Counties in top 100 metro areas 75% to 95% urbanized",
#                                "Counties in top 100 metro areas 25% to 75% urbanized",
#                                "Counties in top 100 metro areas < 25% urbanized", "Counties in all other metro areas",
#                                "Counties in all micropolitan areas", "All other counties")
# )
