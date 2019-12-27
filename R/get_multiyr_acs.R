#' This function returns multiple years acs data using census api
#' @param yr year range
#' @return wide dataframe
#' @export

get_multiyr <- function(geo, vars, yr, key,...){
  map_dfr(yr,function(x)
    tidycensus::get_acs(geography = geo, variable = vars, year = x, key = key, output = "wide",...) %>%
      dplyr::mutate(year = x)
  ) %>%
    dplyr::select(code = GEOID, name = NAME, year, dplyr::everything())
}
