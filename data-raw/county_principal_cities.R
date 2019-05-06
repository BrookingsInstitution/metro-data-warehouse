## code to prepare `county_principal_cities` dataset goes here

# Tue Apr 30 17:37:39 2019 ------------------------------
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
# Sep.2018

url <- "https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2018/delineation-files/list2_Sep_2018.xls"

county_principal_cities <- readxl_online(url,skip=2)

usethis::use_data(county_principal_cities)
