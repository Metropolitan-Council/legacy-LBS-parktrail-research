## These functions are used to add shorelines or rivers to park polygons in the 02_ processing Rmds

addshore_fxn <- function(.geography, .water, .name, .buffer_dist, .hole_tolerance){
  #' Add a shoreline to a park polygon
  #'
  #' @description Extend the boundary of a park polygon into a lake or river. Use `addriver_fxn` to add an entire river to a park boundary.
  #'
  #' @param geography List containing one sf object for each park boundary, output by `01_fetch_park_geography.R`.
  #' @param water List containing one sf object for each park's associated water, output by `01_fetch_park_geography.R`.
  #' @param name Zone name of the given park.
  #' @param buffer_dist Distance, in meters, for the park boundary to be expanded. The buffer must be large enough to intersect with the water feature.
  #' @param hole_tolerance Area, in square meters, under which holes, or gaps, in the park boundary should be filled.
  #'
  .geography[[.name]] %>%
    st_make_valid() %>%
    st_union(.geography[[.name]] %>% 
               st_buffer(.buffer_dist) %>% 
               # intersect - water feature is also buffered to minimize gaps 
               st_intersection(st_buffer(.water[[.name]], .buffer_dist))) %>% 
    group_by(zone_name) %>%
    summarise(do_union = TRUE) %>%
    smoothr::fill_holes(.hole_tolerance)
}

addriver_fxn <- function(.geography, .water, .name, .hole_tolerance){
  #' Add a river to a park polygon
  #'
  #' @description Include an entire river in a park boundary. This function is intended for parks with rivers passing through them, 
  #' not for parks on only one shore of the Mississippi River, for example. Use `addshore_fxn` to add only part of a river's shore.
  #'
  #' @param geography List containing one sf object for each park boundary, output by `01_fetch_park_geography.R`.
  #' @param water List containing one sf object for each park's associated water, output by `01_fetch_park_geography.R`.
  #' @param name Zone name of the given park.
  #' @param buffer_dist Distance, in meters, for the park boundary to be expanded. The buffer must be large enough to intersect with the water feature.
  #' @param hole_tolerance Area, in square meters, under which holes, or gaps, in the park boundary should be filled.
  #'
  .geography[[.name]] %>%
    st_union(.geography[[.name]] %>% 
               st_buffer(50) %>% 
               #intersect with water layer to add rivers
               st_intersection(st_buffer(.water[[.name]], 50))) %>% 
    smoothr::fill_holes(.hole_tolerance) %>%
    group_by(zone_name) %>%
    summarise(do_union = TRUE)
}

