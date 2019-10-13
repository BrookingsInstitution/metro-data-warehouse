#' find cbsa_code by short name and return all counties within the CBSA
#' @param msa name of a metro
#' @return dataframe of counties with matching CBSA
#' @export

find_cbsa_counties <- function(msa){
  metro.data::county_cbsa_st %>%
    # metro.data::county_cbsa_st %>%
    dplyr::filter(grepl(!!msa,cbsa_name,ignore.case = T)) %>%
    # select(cbsa_code, cbsa_name, stco_code, co_name) %>%
    unique()
}
