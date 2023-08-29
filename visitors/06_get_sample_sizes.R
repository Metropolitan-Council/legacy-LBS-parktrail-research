## This script creates individual StreetLight analyses for each of the 10 over-sampled units
## used for validating home locations and demographics. Creating individual analyses for each
## unit allows us to collect sample size information to be compared to the number of surveys
## completed at each unit. Data is saved to `data-intermediate/visitors` and
## used for documentation purposes.

##### setup #####

## oversampled units
oversampled_units <-
  c(
    "Battle-Creek_park_Metro-Regional_Ramsey-County",
    "Cleary-Lake_park_Metro-Regional_Three-Rivers",
    "Como-Zoo-and-Conservatory_park_Metro-Regional_Saint-Paul",
    "Hyland-Bush-Anderson-Lakes_park_Metro-Regional_Bloomington",
    "Lake-Elmo_park_Metro-Regional_Washington-County",
    "Lake-Minnewashta_park_Metro-Regional_Carver-County",
    "Lebanon-Hills_park_Metro-Regional_Dakota-County",
    "North-Mississippi_park_Metro-Regional_MPRB",
    "Lake-Minnetonka-LRT_trail_Metro-Regional_Three-Rivers",
    "Rice-Creek-West_trail_Metro-Regional_Anoka-County"
  )

## geography
park_geo <- read_sf(file.path(here(), "data-intermediate/parks/shp/metro_perim_2023.07.02.shp")) %>%
  filter(zone_name %in% oversampled_units)

trail_geo <- readRDS(file.path(here(), "data-intermediate/trails/trail-polygons.RDS")) %>%
  filter(unit_id %in% oversampled_units) %>%
  rename(zone_name = unit_id)

geos <- bind_rows(park_geo, trail_geo)

##### iterate over each zone #####

### create & upload zone set ###
purrr::map(.x = 1:nrow(geos), .f = function(x) {
  zone <- geos[x, ] %>%
    mutate(
      name = zone_name,
      is_pass = 1
    ) %>%
    st_make_valid()

  upload_zone_set(
    login_email = login_email,
    zones = zone,
    zone_set_name = substr(paste0(zone$zone_name), 1, 50)
  )

  Sys.sleep(30)
})


### run analysis ###
purrr::map(.x = 1:nrow(geos), .f = function(x) {
  zone <- geos[x, ]

  Sys.sleep(10)
  if (str_detect(zone$zone_name, "_park_")) {
    try(create_streetlight_analysis(
      login_email = login_email,
      analysis_type = "Zone_Activity_Analysis",
      travel_mode_type = "All_Vehicles",
      output_type = "Volume",
      origin_zone_set = substr(paste0(zone$zone_name), 1, 50),
      traveler_attributes = TRUE,
      enable_home_work_locations = TRUE,
      hwl_enable_visitor = TRUE,
      analysis_name = paste0(
        substr(paste0(zone$zone_name), 1, 20),
        "_veh_ss"
      ),
      date_ranges = list(
        start_date = "06/01/2021",
        end_date = "08/31/2021"
      ),
      tags = list("streetlightR"),
      zone_intersection_type = "trips_by_pass_through_setting"
    ))
  }

  Sys.sleep(10)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    travel_mode_type = "Bicycle",
    output_type = "Volume",
    origin_zone_set = substr(paste0(zone$zone_name), 1, 50),
    traveler_attributes = TRUE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    analysis_name = paste0(
      substr(paste0(zone$zone_name), 1, 20),
      "_bike_ss"
    ),
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    tags = list("streetlightR"),
    zone_intersection_type = "trips_by_pass_through_setting"
  ))

  Sys.sleep(10)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    travel_mode_type = "Pedestrian",
    output_type = "Volume",
    origin_zone_set = substr(paste0(zone$zone_name), 1, 50),
    traveler_attributes = TRUE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    analysis_name = paste0(
      substr(paste0(zone$zone_name), 1, 20),
      "_ped_ss"
    ),
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    tags = list("streetlightR"),
    zone_intersection_type = "trips_by_pass_through_setting"
  ))
})



### fetch analyses ###
sample_size_raw <- purrr::map_dfr(.x = 1:nrow(geos), .f = function(x) {
  zone <- geos[x, ]

  Sys.sleep(6)
  bike <- get_analysis_data(
    analysis_name = paste0(substr(paste0(zone$zone_name), 1, 20), "_bike_ss"),
    metric = "sample_size"
  ) %>%
    mutate_all(as.character)

  Sys.sleep(6)
  ped <- get_analysis_data(
    analysis_name = paste0(substr(paste0(zone$zone_name), 1, 20), "_ped_ss"),
    metric = "sample_size"
  ) %>%
    mutate_all(as.character)

  if (str_detect(zone$zone_name, "_park_")) {
    Sys.sleep(6)
    veh <- get_analysis_data(
      analysis_name = paste0(substr(paste0(zone$zone_name), 1, 20), "_veh_ss"),
      metric = "sample_size"
    ) %>%
      mutate_all(as.character)

    res <- bind_rows(bike, ped, veh) %>%
      mutate(zone_name = zone$zone_name)
  }

  if (str_detect(zone$zone_name, "_trail_")) {
    res <- bind_rows(bike, ped) %>%
      mutate(zone_name = zone$zone_name)
  }

  res
})

### clean ###
sample_size <- sample_size_raw %>%
  select(zone_name, mode_of_travel, approximate_device_count) %>%
  mutate(
    mode = case_when(
      str_detect(mode_of_travel, "Bicycle") ~ "Bicycle",
      str_detect(mode_of_travel, "Pedestrian") ~ "Pedestrian",
      TRUE ~ "Vehicle"
    ),
    less_than = ifelse(str_detect(approximate_device_count, "<"), TRUE, FALSE),
    approximate_device_count = str_remove(approximate_device_count, "<"),
    devices = as.numeric(approximate_device_count),
    approx_devices = ifelse(less_than == TRUE, 500, devices)
  )

sample_size %>%
  group_by(mode, less_than) %>%
  summarise(count = n())

sample_size <- sample_size %>%
  select(-c(less_than, mode_of_travel, approximate_device_count, devices)) %>%
  pivot_wider(
    names_from = mode,
    values_from = approx_devices
  ) %>%
  replace_na(list(Vehicle = 0)) %>%
  mutate(Total = Bicycle + Pedestrian + Vehicle)

### save ###
save(sample_size,
  file = file.path(here(), "data-intermediate/visitors/sample-size.rda")
)
