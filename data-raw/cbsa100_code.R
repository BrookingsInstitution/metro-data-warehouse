#' This function returns CBSA code of top 100 metros

cbsa100_code <- unique((metro.data::county_cbsa_st %>%
                          dplyr::filter(cbsa_is.top100))$cbsa_code)
