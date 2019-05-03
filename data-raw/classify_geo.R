
def_metrotype <- function(df){
  df %>%
    # code msa typopogy
    mutate(cbsa_type = case_when(
      `Metropolitan/Micropolitan Statistical Area`=="Metropolitan Statistical Area" ~ "metro",
      `Metropolitan/Micropolitan Statistical Area`=="Micropolitan Statistical Area" ~ "micro",
      is.na(`Metropolitan/Micropolitan Statistical Area`) ~ "nonmetro"))
}


def_metrosize <- function(df){
  df %>%
    mutate(cbsa_size = ifelse(cbsa_type=="metro",
                              case_when(
                                cbsa_pop > 1000000 ~ "large metro",
                                cbsa_pop <= 1000000 & cbsa_pop >= 250000 ~ "medium metro",
                                cbsa_pop < 250000 ~ "small metro"),
                             cbsa_type))
}



def_metro100 <- function(df){
  temp <- (df %>%
    select(cbsa_code, cbsa_pop) %>%
    unique()%>%
    filter(rank(desc(cbsa_pop)) <=100))$cbsa_code

  df <- df%>%mutate(cbsa_type = case_when(cbsa_code %in% temp ~ "top100",
                                          T ~ cbsa_code))
  return(df)
  }

def_countytype <- function(df){
  df %>%
    # create county type labels, using Alan.B methodology (see below)
    mutate(co_type = ifelse(cbsa_type == "top100",
                                # in top 100, create new labels based on urbanized area
                                case_when(
                                  co_pcturban>99 ~ "Urban counties",
                                  co_pcturban>95 & co_pcturban<=99~ "High-density suburban counties",
                                  co_pcturban>75 & co_pcturban<=95~ "Mature suburban counties",
                                  co_pcturban>25 & co_pcturban<=75~ "Emerging suburban counties",
                                  co_pcturban<=25~ "Exurban counties"),
                                # not in top 100, keep original type labels
                                paste0(cbsa_type," counties")))
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
