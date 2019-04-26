
# rank cbsa by population
get_metro100 <- function(){
  county_cbsa_st %>%
    filter(type.cbsa=="metro") %>%
    select(code.cbsa, name.cbsa, population.cbsa) %>%
    unique()%>%
    mutate(rank.pop.cbsa = rank(desc(population.cbsa)),
           istop100.cbsa = rank.pop.cbsa <=100)
}
