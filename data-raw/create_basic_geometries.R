##============================================================================##
# Creating basic shapefiles for Metro Data Warehouse
#
# David Harshbarger
# August 2019
##============================================================================##


##============================================================================##
# SETUP
##============================================================================##
library(tidyverse)
library(magrittr)
library(tigris)
library(sf)
library(mapview)
library(readxl)

# homedir <- "V:/David/data warehouse sandbox"
# setwd(homedir)

# read in top 100 metros
load("data/county_cbsa_st.rda")
top100 <- county_cbsa_st %>%
  filter(cbsa_type == "top100") %>%
  select(cbsa_code, cbsa_name) %>%
  distinct()

# source functions for cleaning up tigris shapes and shifting AK/HI geometries
source("data-raw/create_basic_geometries_helper.R")

# when tigris downloads geographies they extend over water boundaries
# we will first import a "nation" shape that outlines the coast and other borders
# then we will clip state and other geographies to that extent
nation_sf <- tigris::nation(year = 2018) %>%

  # for some reason, this particular tigris function can't import directly as sf
  # so convert it now
  st_as_sf()

# for future use, define the continental us as the single polygon with the largest area
continentalus_sf <- nation_sf %>%
  st_cast(to = "POLYGON") %>%
  mutate(area = st_area(geometry)) %>%
  filter(area == max(area)) %>%
  select(-area)

# define the territories that we don't use so much, these will be filtered out
territories <- tibble::tribble(
  ~st_code, ~st_ab,
  60,   "AS",
  66,   "GU",
  69,   "MP",
  72,   "PR",
  78,   "VI"
)


# load crs info of census shapefiles (4269)
load("data-raw/crs_NAD83.RData")
crs_basic <- "+proj=longlat +datum=NAD83 +no_defs"

# transform nation shape to match the crs of the states (the govt default)
nation_sf <- nation_sf %>%
  st_transform(crs = crs_NAD83)



##============================================================================##
# IMPORT STATES, COUNTIES, METROS SHAPES
##============================================================================##

# sf refers to spatial data, ld/hd refers to low definition / high definition
# cleanup functions from helper script
states51_sf_ld <- tigris::states(class = "sf", cb = TRUE, year = 2018) %>%
  cleanup_tigris()
states51_sf_hd <- tigris::states(class = "sf", cb = FALSE, year = 2018) %>%
  cleanup_tigris()

counties51_sf_ld <- tigris::counties(class = "sf", cb = TRUE, year = 2018) %>%
  cleanup_tigris()
counties51_sf_hd <- tigris::counties(class = "sf", cb = FALSE, year = 2018) %>%
  cleanup_tigris()


cbsas51_sf_ld <- tigris::core_based_statistical_areas(class = "sf", cb = TRUE, year = 2018) %>%
  cleanup_tigris2()
cbsas51_sf_hd <- tigris::core_based_statistical_areas(class = "sf", cb = FALSE, year = 2018) %>%
  cleanup_tigris2()

metros51_sf_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAMELSAD,
               pattern = "Metro Area"))
metros51_sf_ld <- cbsas51_sf_ld %>%
  filter(GEOID %in% metros51_sf_hd$GEOID)

top100_sf_ld <- metros51_sf_ld %>%
  inner_join(top100, by = c("GEOID" = "cbsa_code"))
top100_sf_hd <- metros51_sf_hd %>%
  inner_join(top100, by = c("GEOID" = "cbsa_code"))


##============================================================================##
# CREATE CONTINENTAL VERSIONS
##============================================================================##

states49_sf_ld <- states51_sf_ld %>%
  filter(!(STATEFP %in% c("02", "15")))
states49_sf_hd <- states51_sf_hd %>%
  filter(!(STATEFP %in% c("02", "15")))

counties49_sf_ld <- counties51_sf_ld %>%
  filter(!(STATEFP %in% c("02", "15")))
counties49_sf_hd <- counties51_sf_hd %>%
  filter(!(STATEFP %in% c("02", "15")))

cbsas49_sf_ld <- cbsas51_sf_ld %>%
  filter(!grepl(x = NAME,
                pattern = ", PR|, AK|, HI"))
cbsas49_sf_hd <- cbsas51_sf_hd %>%
  filter(!grepl(x = NAME,
                pattern = ", PR|, AK|, HI"))

metros49_sf_ld <- metros51_sf_ld %>%
  filter(!grepl(x = NAME,
                pattern = ", PR|, AK|, HI"))
metros49_sf_hd <- metros51_sf_hd %>%
  filter(!grepl(x = NAME,
                pattern = ", PR|, AK|, HI"))

##============================================================================##
# CREATE INSET VERSIONS
##============================================================================##

# use "affine transformations" to get AK and HI into the right size and place
# affine transformations are of the form f(x) = xA + b
# where matrix A is used to flatten, scale and/or rotate, and b to translate x
# more info at https://cran.r-project.org/web/packages/sf/vignettes/sf3.html


HI_sf_ld <- states51_sf_ld %>%
  filter(STATEFP == "15") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)

HI_sf_hd <- states51_sf_hd %>%
  filter(STATEFP == "15") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)

AK_sf_ld <- states51_sf_ld %>%
  filter(STATEFP == "02") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)

AK_sf_hd <- states51_sf_hd %>%
  filter(STATEFP == "02") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)

# use the shift function defined in the helper script to create insets of AK and HI
HI_inset_ld <- HI_sf_ld %>%
  shift_AKorHI(AKorHI = "HI")

HI_inset_hd <- HI_sf_hd %>%
  shift_AKorHI(AKorHI = "HI")

AK_inset_ld <- AK_sf_ld %>%
  shift_AKorHI(AKorHI = "AK")

AK_inset_hd <- AK_sf_hd %>%
  shift_AKorHI(AKorHI = "AK")


# inspect visually with ggplot
# p <- ggplot() +
#   geom_sf(data = continentalus_sf %>% st_transform(3857)) +
#   geom_sf(data = HI_inset_sf) +
#   geom_sf(data = AK_inset_sf)

states51_inset_ld <- rbind(states49_sf_ld,
                           AK_inset_ld %>% st_transform(crs = crs_NAD83),
                           HI_inset_ld %>% st_transform(crs = crs_NAD83))

# use mapview to check the appearance
# mapview(continentalus_sf) + mapview(AK_inset_sf) + mapview(HI_inset_sf)

# use ggplot to check the apperance in an equal-area projection
# p + coord_sf(crs = 102009)


##============================================================================##
# APPLY INSET TRANSFORMATIONS TO COUNTIES, CBSAS, AND METROS
##============================================================================##

countiesHI_inset_hd <- counties51_sf_hd %>%
  filter(str_sub(GEOID, end = 2L) == "15") %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)
countiesHI_inset_ld <- counties51_sf_ld %>%
  filter(str_sub(GEOID, end = 2L) == "15") %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)


countiesAK_inset_hd <- counties51_sf_hd %>%
  filter(str_sub(GEOID, end = 2L) == "02") %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)
countiesAK_inset_ld <- counties51_sf_ld %>%
  filter(str_sub(GEOID, end = 2L) == "02") %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)

counties51_inset_hd <- rbind(counties49_sf_hd,
                             countiesAK_inset_hd,
                             countiesHI_inset_hd) %>%
  st_transform(crs = crs_NAD83)
counties51_inset_ld <- rbind(counties49_sf_ld,
                             countiesAK_inset_ld,
                             countiesHI_inset_ld) %>%
  st_transform(crs = crs_NAD83)

cbsasHI_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAME, pattern = ", HI")) %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)
cbsasHI_inset_ld <- cbsas51_sf_ld %>%
  filter(grepl(x = NAME, pattern = ", HI")) %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)

cbsasAK_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAME, pattern = ", AK")) %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)
cbsasAK_inset_ld <- cbsas51_sf_ld %>%
  filter(grepl(x = NAME, pattern = ", AK")) %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)

cbsas51_inset_hd <- rbind(cbsas49_sf_hd,
                          cbsasAK_inset_hd,
                          cbsasHI_inset_hd) %>%
  st_transform(crs = crs_NAD83)
cbsas51_inset_ld <- rbind(cbsas49_sf_ld,
                          cbsasAK_inset_ld,
                          cbsasHI_inset_ld) %>%
  st_transform(crs = crs_NAD83)

metrosHI_inset_hd <- metros51_sf_hd %>%
  filter(grepl(x = NAMELSAD, pattern = ", HI Metro Area")) %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)
metrosHI_inset_ld <- metros51_sf_ld %>%
  filter(GEOID %in% metrosHI_inset_hd$GEOID) %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)

metrosAK_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAMELSAD, pattern = ", AK Metro Area")) %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)
metrosAK_inset_ld <- cbsas51_sf_ld %>%
  filter(GEOID %in% metrosAK_inset_hd$GEOID) %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)

metros51_inset_hd <- rbind(metros49_sf_hd,
                           metrosAK_inset_hd,
                           metrosHI_inset_hd) %>%
  st_transform(crs = crs_NAD83)
metros51_inset_ld <- rbind(metros49_sf_ld,
                           metrosAK_inset_ld,
                           metrosHI_inset_ld) %>%
  st_transform(crs = crs_NAD83)


# split and merge
top100_inset_hd <- rbind(metrosAK_inset_hd,
                         metrosHI_inset_hd) %>%
  inner_join(top100, by = c("GEOID" = "cbsa_code")) %>%
  rbind(top100_sf_hd %>%
          filter(!(GEOID %in% (rbind(metrosAK_inset_hd,
                                     metrosHI_inset_hd))$GEOID))) %>%
  st_transform(crs = crs_NAD83)
top100_inset_ld <- rbind(metrosAK_inset_ld,
                         metrosHI_inset_ld) %>%
  inner_join(top100, by = c("GEOID" = "cbsa_code")) %>%
  rbind(top100_sf_ld %>%
          filter(!(GEOID %in% (rbind(metrosAK_inset_ld,
                                     metrosHI_inset_ld))$GEOID))) %>%
  st_transform(crs = crs_NAD83)


##============================================================================##
# WRITE OUTPUT FILES
##============================================================================##

##====================================##
# basic geometries (no insets)
##====================================##

## STATES
# hd
st_write(obj = states51_sf_hd, dsn = "data/states51_hd.shp")
st_write(obj = states49_sf_hd, dsn = "data/states49_hd.shp")
# ld
st_write(obj = states51_sf_ld, dsn = "data/states51_ld.shp")
st_write(obj = states49_sf_ld, dsn = "data/states49_ld.shp")

## COUNTIES
# hd
st_write(obj = counties51_sf_hd, dsn = "data/counties51_hd.shp")
st_write(obj = counties49_sf_hd, dsn = "data/counties49_hd.shp")
# ld
st_write(obj = counties51_sf_ld, dsn = "data/counties51_ld.shp")
st_write(obj = counties49_sf_ld, dsn = "data/counties49_ld.shp")

## CBSAS
# hd
st_write(obj = cbsas51_sf_hd, dsn = "data/cbsas51_hd.shp")
st_write(obj = cbsas49_sf_hd, dsn = "data/cbsas49_hd.shp")
# ld
st_write(obj = cbsas51_sf_ld, dsn = "data/cbsas51_ld.shp")
st_write(obj = cbsas49_sf_ld, dsn = "data/cbsas49_ld.shp")

## METROS
# hd
st_write(obj = metros51_sf_hd, dsn = "data/metros51_hd.shp")
st_write(obj = metros49_sf_hd, dsn = "data/metros49_hd.shp")
# ld
st_write(obj = metros51_sf_ld, dsn = "data/metros51_ld.shp")
st_write(obj = metros49_sf_ld, dsn = "data/metros49_ld.shp")

## TOP 100 METROS (no continental version, as HI has a top100 metro in Honolulu)
# hd
st_write(obj = top100_sf_hd, dsn = "data/top100_hd.shp")
# ld
st_write(obj = top100_sf_ld, dsn = "data/top100_ld.shp")

## ALASKA AND HAWAII
#hd
st_write(obj = AK_sf_hd %>% st_transform(crs = crs_NAD83), dsn = "data/AK_hd.shp")
st_write(obj = HI_sf_hd %>% st_transform(crs = crs_NAD83), dsn = "data/HI_hd.shp")

# ld
st_write(obj = AK_sf_ld %>% st_transform(crs = crs_NAD83), dsn = "data/AK_ld.shp")
st_write(obj = HI_sf_ld %>% st_transform(crs = crs_NAD83), dsn = "data/HI_ld.shp")


##====================================##
# inset geometries
##====================================##

## COUNTIES INSETS
# hd
st_write(obj = countiesAK_inset_hd, dsn = "data/countiesAK_inset_hd.shp")
st_write(obj = countiesHI_inset_hd, dsn = "data/countiesHI_inset_hd.shp")
st_write(obj = counties51_inset_hd, dsn = "data/counties51_inset_hd.shp")
# ld
st_write(obj = countiesAK_inset_ld, dsn = "data/countiesAK_inset_ld.shp")
st_write(obj = countiesHI_inset_ld, dsn = "data/countiesHI_inset_ld.shp")
st_write(obj = counties51_inset_ld, dsn = "data/counties51_inset_ld.shp")


## CBSAS INSETS
# hd
st_write(obj = cbsasAK_inset_hd, dsn = "data/cbsasAK_inset_hd.shp")
st_write(obj = cbsasHI_inset_hd, dsn = "data/cbsasHI_inset_hd.shp")
st_write(obj = cbsas51_inset_hd, dsn = "data/cbsas51_inset_hd.shp")
# ld
st_write(obj = cbsasAK_inset_ld, dsn = "data/cbsasAK_inset_ld.shp")
st_write(obj = cbsasHI_inset_ld, dsn = "data/cbsasHI_inset_ld.shp")
st_write(obj = cbsas51_inset_ld, dsn = "data/cbsas51_inset_ld.shp")


# METROS INSETS
# hd
st_write(obj = metrosAK_inset_hd, dsn = "data/metrosAK_inset_hd.shp")
st_write(obj = metrosHI_inset_hd, dsn = "data/metrosHI_inset_hd.shp")
st_write(obj = metros51_inset_hd, dsn = "data/metros51_inset_hd.shp")
# ld
st_write(obj = metrosAK_inset_ld, dsn = "data/metrosAK_inset_ld.shp")
st_write(obj = metrosHI_inset_ld, dsn = "data/metrosHI_inset_ld.shp")
st_write(obj = metros51_inset_ld, dsn = "data/metros51_inset_ld.shp")


## TOP 100 METROS INSET
# hd
st_write(obj = top100_inset_hd,   dsn = "data/top100_inset_hd.shp")
# ld
st_write(obj = top100_inset_ld,   dsn = "data/top100_inset_ld.shp")


## ALASKA AND HAWAII INSETS
# hd
st_write(obj = AK_inset_hd %>% st_transform(crs = crs_NAD83), dsn = "data/AK_inset_hd.shp")
st_write(obj = HI_inset_hd %>% st_transform(crs = crs_NAD83), dsn = "data/HI_inset_hd.shp")
# ld
st_write(obj = AK_inset_ld %>% st_transform(crs = crs_NAD83), dsn = "data/AK_inset_ld.shp")
st_write(obj = HI_inset_ld %>% st_transform(crs = crs_NAD83), dsn = "data/HI_inset_ld.shp")
