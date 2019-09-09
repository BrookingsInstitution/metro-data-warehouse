##============================================================================##
# Helper functions for:
#  Creating basic shapefiles for Metro Data Warehouse
#
# David Harshbarger
# August 2019
##============================================================================##

libs <- c("dplyr", "tigris", "sf")
libs_unattached <- setdiff(libs,
                           .packages(all.available = T))

if (length(libs_unattached == 0)){
  break
} else{
  lapply(libs_unattached,
         library,
         character.only = TRUE)
}

rm(libs, libs_unattached)

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

##============================================================================##
# function to shift AK or HI geometries to inset locations
##============================================================================##

# make a function to shift all HI or AK geometries the same way
shift_AKorHI <- function(shape, AKorHI, rescale = NULL){

  # shape is the AK or HI (alone) geometry which is supplied as an sf object
  # AKorHI tells the function which transformation to apply
  # rescale (optional) gives a multiplier to change the size of the geometries
  #  1.0 = 100% (no change in size, the default for HI)
  #  0.2 = 20%  (one-fifth in size, the defalut for AK)

  # test validity of supplied arguments
  if (!("sf" %in% class(shape))) stop("'shape' must be an sf object")
  if (is.na(st_crs(shape)))      stop("'shape' must have crs set")
  if (!is.character(AKorHI))     stop("'AKorHI' must be character, one of 'AK', or 'HI'")

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

  # print a warning that the resulting sf object will always be in epsg:3857, no matter what
  warning("'shift_AKorHI' changes coordinates reference system to epsg:3857, measured in meters.")
  return(shape_transformed)
}
