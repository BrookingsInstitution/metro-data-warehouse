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

homedir <- "V:/David/data warehouse sandbox"
setwd(homedir)

# read in top 100 metros
top100 <- read_excel("top100metros.xlsx")

# when tigris downloads geographies they extend over water boundaries
# we will first import a "nation" shape that outlines the coast and other borders
# then we will clip state and other geographies to that extent
nation_sf <- tigris::nation() %>%

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
load("crs_NAD83.RData")

# transform nation shape to match the crs of the states (the govt default)
nation_sf <- nation_sf %>%
  st_transform(crs = crs_NAD83)

##============================================================================##
# IMPORT STATES, COUNTIES, METROS SHAPES
##============================================================================##

# first, define functions that will be repeatedly used to filter and clip
cleanup_tigris <- function(shp){
  shp %>%

    # filter for 50 states + DC
    filter(!(STATEFP %in% territories$st_code)) %>%

    # clip to national extent
    st_intersection(nation_sf$geometry)
}

# this one is for cbsa, which cross state lines
cleanup_tigris2 <- function(shp){
  shp %>%

    # filter out Pureto Rico
    filter(!grepl(x = NAME,
                  pattern = ", PR")) %>%

    # clip to national extent
    st_intersection(nation_sf$geometry)
}

# sf refers to spatial data, ld/hd refers to low definition / high definition
states51_sf_ld <- tigris::states(class = "sf", cb = TRUE) %>%
  cleanup_tigris()
states51_sf_hd <- tigris::states(class = "sf", cb = FALSE) %>%
  cleanup_tigris()

counties51_sf_ld <- tigris::counties(class = "sf", cb = TRUE) %>%
  cleanup_tigris()
counties51_sf_hd <- tigris::counties(class = "sf", cb = FALSE) %>%
  cleanup_tigris()


cbsas51_sf_ld <- tigris::core_based_statistical_areas(class = "sf", cb = TRUE) %>%
  cleanup_tigris2()
cbsas51_sf_hd <- tigris::core_based_statistical_areas(class = "sf", cb = FALSE) %>%
  cleanup_tigris2()

metros51_sf_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAMELSAD,
               pattern = "Metro Area"))
metros51_sf_ld <- cbsas51_sf_ld %>%
  filter(GEOID %in% metros51_sf_hd$GEOID)

top100_sf_ld <- metros51_sf_ld %>%
  inner_join(top100, by = c("GEOID" = "CBSA"))
top100_sf_hd <- metros51_sf_hd %>%
  inner_join(top100, by = c("GEOID" = "CBSA"))


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

# define rotation function
# (this is not used below, but is included for potential future use)
rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

# only need low-def versions, as inset maps are not used for geoprocessing
HI_sf <- states51_sf_ld %>%
  filter(STATEFP == "15") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)

AK_sf <- states51_sf_ld %>%
  filter(STATEFP == "02") %>%
  st_transform(3857) %>%
  st_buffer(dist = 0)


# make a function to shift all HI or AK geometries the same way
shift_AKorHI <- function(shape, AKorHI, rescale = NULL){

  # test validity of supplied arguments
  if (is.na(st_crs(shape))) stop("'shape' must have crs set")
  if (!is.character(AKorHI)) stop("'AKorHI' must be character, one of 'AK', or 'HI'")

  # prepare the sf object
  prepared_shape <- shape %>%
    st_transform(3857) %>%
    st_buffer(dist = 0)

  # define AK and HI default centroids in this projection
  AK_centroid <- c(-16968368, 9610108)
  HI_centroid <- c(-17405371, 2303492)


  #### cases for default scaling of HI and AK ####

  # HAWAII:
  if (tolower(AKorHI) %in% c("hi", "hawaii")) {

    # use the centroid coords to translate the shape to the origin
    # this prevents unwanted translation when scaling
    # then rescale at the origin
    # then add centroid back to translate to original location, now smaller
    if (missing(rescale)){
      rescale <- 1.0
    }
    shape_scaled_sfc <- (prepared_shape$geometry - HI_centroid) * rescale + HI_centroid

    # replace the geometry with the new scaled geometry
    shape_scaled <- prepared_shape
    shape_scaled$geometry <- shape_scaled_sfc

    HI_shift_coords <- c(6000000, 400000)
    shape_scaled$geometry <- shape_scaled$geometry + HI_shift_coords
    shape_transformed <- shape_scaled %>%
      st_set_crs(3857)
  }

  # ALASKA:
  # this gets weird because the archipelago crosses 180 degrees, so
  # typical affine transformations don't work as expected
  # need to split geometries into two

  if (tolower(AKorHI) %in% c("ak", "alaska")) {

    if (missing(rescale)){
      rescale <- 0.2
    }

    # define a function to convert WKT to boxes as sf
    st_text_to_sf <- function(x){
      x %>%
        st_as_sfc() %>%
        st_set_crs(4269) %>%
        st_sf()
    }

    AK_box_right <- st_text_to_sf(
      "POLYGON((-179.999999999 73,-115 73,-115 40,-179.999999999 40,-179.999999999 73))))"
      )
    AK_box_left <- st_text_to_sf(
      "POLYGON((149 73,179.999999999 73,179.999999999 40,149 40,149 73))"
      )
    AK_boxes <- rbind(AK_box_right, AK_box_left) %>%
      st_as_sf() %>%
      mutate(box = c("right", "left")) %>%
      select(box, geometry) %>%
      st_transform(crs = st_crs(prepared_shape))

    prepared_shape_int <- prepared_shape %>%
      # split in two by the boxes
      st_intersection(AK_boxes)

    # shift the right-side piece to the origin, by subtracting the centroid coords
    AK_right_sfc <- (prepared_shape_int %>% filter(box == "right"))$geometry - AK_centroid
    # this strips the coordinate info, so replace it
    AK_right_sfc <- st_set_crs(AK_right_sfc, st_crs(prepared_shape_int$geometry))

    # shift the left-side piece to the origin
    # need coordinates that describe a -360 rotation in this projection:
    wrap360_epsg3857 <- c(-40052752.78, 0)
    AK_left_sfc <- (prepared_shape_int %>% filter(box == "left"))$geometry + wrap360_epsg3857 - AK_centroid
    AK_left_sfc <- st_set_crs(AK_left_sfc, st_crs(prepared_shape_int$geometry))

    # with both pieces of AK now at the origin, union them into one MULTIPOLYGON
    AK_origin <- prepared_shape_int %>%
      mutate(geometry = c(AK_right_sfc, AK_left_sfc)) %>%
      select(-box) %>%
      # group by everything EXCEPT geometry
      group_by_at(vars(-geometry)) %>%
      summarise() %>%
      ungroup()


    # by default shrink to 20%, then add centroid coords back to put the shrunk shape back
    shape_scaled <- (AK_origin$geometry * rescale + AK_centroid) %>%
      st_set_crs(st_crs(prepared_shape$geometry))

    # define the arbitrary shift coordinates that will determine where AK shows up on the map
    AK_shift_coords <- c(4400000, -6400000)

    shape_scaled <- (shape_scaled + AK_shift_coords) %>%
      st_set_crs(st_crs(prepared_shape$geometry)) %>%
      st_buffer(dist = 0)

    shape_transformed <- AK_origin %>%
      mutate(geometry = shape_scaled)
  }

  warning("'shift_AKorHI' changes coordinates reference system to epsg 3857, measured in meters.")
  return(shape_transformed)
}

# use the shift function defined above to create low-def insets of AK and HI
HI_inset_sf <- HI_sf %>%
  shift_AKorHI(AKorHI = "HI")

AK_inset_sf <- AK_sf %>%
  shift_AKorHI(AKorHI = "AK")

# inspect visually with ggplot
p <- ggplot() +
  geom_sf(data = continentalus_sf %>% st_transform(3857)) +
  geom_sf(data = HI_inset_sf) +
  geom_sf(data = AK_inset_sf)


# use mapview to check the appearance
# mapview(continentalus_sf) + mapview(AK_inset_sf) + mapview(HI_inset_sf)

# use ggplot to check the apperance in an equal-area projection
p + coord_sf(crs = 102009)


##============================================================================##
# APPLY INSET TRANSFORMATIONS TO COUNTIES, CBSAS, AND METROS
##============================================================================##

# unlike state insets, sub-state inset geographies are to be high def

countiesHI_inset_hd <- counties51_sf_hd %>%
  filter(str_sub(GEOID, end = 2L) == "15") %>%
  shift_AKorHI(AKorHI = "HI") %>%
  st_transform(crs = crs_NAD83)

countiesAK_inset_hd <- counties51_sf_hd %>%
  filter(str_sub(GEOID, end = 2L) == "02") %>%
  shift_AKorHI(AKorHI = "AK") %>%
  st_transform(crs = crs_NAD83)

counties51_inset_hd <- rbind(counties49_sf_hd,
                             countiesAK_inset_hd,
                             countiesHI_inset_hd)

cbsasHI_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAME, pattern = ", HI")) %>%
  shift_AKorHI(AKorHI = "HI")

cbsasAK_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAME, pattern = ", AK")) %>%
  shift_AKorHI(AKorHI = "AK")

metrosHI_inset_hd <- metros51_sf_hd %>%
  filter(grepl(x = NAMELSAD, pattern = ", HI Metro Area")) %>%
  shift_AKorHI(AKorHI = "HI")

metrosAK_inset_hd <- cbsas51_sf_hd %>%
  filter(grepl(x = NAMELSAD, pattern = ", AK Metro Area")) %>%
  shift_AKorHI(AKorHI = "AK")

# split and merge
top100_inset_hd <- rbind(metrosAK_inset_hd,
                         metrosHI_inset_hd) %>%
  inner_join(top100, by = c("GEOID" = "CBSA")) %>%
  rbind(top100_sf_hd %>%
          filter(!(GEOID %in% (rbind(metrosAK_inset_hd,
                                     metrosHI_inset_hd))$GEOID)) %>%
          st_transform(crs = 3857))





# test
p +
  geom_sf(data = top100_inset_hd %>% st_centroid(),
          col = "red",
          size = 3) +
  coord_sf(crs = 102009)
