# find cbsa_code by short name and all counties within the CBSA ------
find_cbsa_counties <- function(msa){
  county_cbsa_st %>%
    # metro.data::county_cbsa_st %>%
    filter(grepl(!!msa,cbsa_name,ignore.case = T)) %>%
    # select(cbsa_code, cbsa_name, stco_code, co_name) %>%
    unique()
}
