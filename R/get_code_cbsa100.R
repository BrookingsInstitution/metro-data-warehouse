
# rank cbsa by population
get_code_cbsa100 <- function(){
  df <- county_cbsa_st %>%
    select(cbsa_code,cbsa_type) %>%
    unique()%>%
    filter(cbsa_type == "top100")
  return(df$cbsa_code)
}

# get_code_cbsa100()
