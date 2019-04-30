
# rank cbsa by population
get_code_cbsa100 <- function(){
  df <- county_cbsa_st %>%
    select(code.cbsa,population.cbsa) %>%
    unique()%>%
    filter(rank(desc(population.cbsa)) <=100)
  return(df$code.cbsa)
}

