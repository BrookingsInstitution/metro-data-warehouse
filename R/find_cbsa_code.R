#' This function returns cbsa code by matching fuzzy metro names
#' @param msa name of a metro
#' @return 5 digit CBSA code
#' @export

find_cbsa_code <- function(msa){
  metro.data::county_cbsa_st %>%
    dplyr::filter(grepl(!!msa,cbsa_name,ignore.case = T)) %>%
    dplyr::select(cbsa_code, cbsa_name) %>%
    unique()
}
