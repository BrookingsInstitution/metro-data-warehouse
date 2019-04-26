
get_metrosize <- function(){
  county_cbsa_st %>%

    # filter(type.cbsa=="metro") %>% # Should non-metros included here?

    select(code.cbsa, name.cbsa, population.cbsa) %>%
    unique()%>%
    mutate(size.cbsa = case_when(
      population.cbsa > 1000000 ~ "Large metro",
      population.cbsa <= 1000000 & population.cbsa >= 250000 ~ "Medium metro",
      population.cbsa < 250000 ~ "Small metro",
      TRUE ~ "NA"))
}


