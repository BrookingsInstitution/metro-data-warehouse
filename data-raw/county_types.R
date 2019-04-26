## code to prepare `county_types` dataset goes here

source("R/readxl_online.R")
source("R/classify_geo.R")

load("data/county_cbsa_st.Rda")

# get 2010 urbanized area data
url <- "https://www2.census.gov/geo/docs/reference/ua/PctUrbanRural_County.xls"
county_urban_rural <- readxl_online(url)

# add top 100 label to master file
top.100 <- (get_metro100()%>%filter(istop100.cbsa))$code.cbsa
county2msatype <- county_cbsa_st%>%
  mutate(type.cbsa.100 = ifelse(code.cbsa%in%top.100,"top100",type.cbsa))%>%
  select(code.county, type.cbsa.100)

# match urban/rural file with msa types
county_types <- county_urban_rural%>%
  mutate(code.county = paste0(STATE, COUNTY))%>%
  rename(pct.urban.county = POPPCT_URBAN)%>%
  left_join(county2msatype, by = "code.county")%>%
  # create county type labels, using Alan.B methodology (see below)
  mutate(type.county = ifelse(type.cbsa.100 == "top100",
                              # in top 100, create new labels based on urbanized area
                              case_when(
                                pct.urban.county>99 ~ "UR",
                                pct.urban.county>95 & pct.urban.county<=99~ "HD",
                                pct.urban.county>75 & pct.urban.county<=95~ "MS",
                                pct.urban.county>25 & pct.urban.county<=75~ "ES",
                                pct.urban.county<25~ "EX"),
                              # not in top 100, keep original type labels
                              type.cbsa.100))

usethis::use_data(county_types)

# Alan's methodology
types <- data.frame(stringsAsFactors=FALSE,
           county.type = c("Urban", "High-density suburban", "Mature suburban",
                           "Emerging suburban", "Exurban", "Small metro",
                           "Micropolitan", "Rural"),
                  code = c("UR", "HD", "MS", "ES", "EX", "SM", "MI", "RU"),
                number = c(58, 74, 168, 203, 97, 567, 658, 1318),
                 total = c(60222963, 70405226, 54216465, 16820947, 2189245,
                           58651644, 27615148, 18623900),
             urbanized = c(59978900, 67741913, 46156215, 6757017, 26588, 39016623,
                           228950, 15917),
        pct_.urbanized = c(99.6, 96.2, 85.1, 40.2, 1.2, 66.5, 0.8, 0.1),
                labels = c("Counties in top 100 metro areas > 99% urbanized",
                           "Counties in top 100 metro areas 95% to 99% urbanized",
                           "Counties in top 100 metro areas 75% to 95% urbanized",
                           "Counties in top 100 metro areas 25% to 75% urbanized",
                           "Counties in top 100 metro areas < 25% urbanized", "Counties in all other metro areas",
                           "Counties in all micropolitan areas", "All other counties")
     )

