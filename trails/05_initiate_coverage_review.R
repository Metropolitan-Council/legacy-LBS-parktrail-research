# This script uses the zones created in `04_create_zonesets.R` to run
# StreetLight analyses for the summer of 2021. These analyses will be used
# for demographic and home location data. The purpose of this script is
# to get zone sets approved by the StreetLight team before running monthly analyses.

run_trail_21_analysis <- function(.analysis_name, .segment_zone_set, .buffer_zone_set) {
  # run demographics on segment zones
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("summer_bike_", .analysis_name),
    travel_mode_type = "Bicycle",
    output_type = "Volume",
    origin_zone_set = .segment_zone_set,
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = TRUE,
    trip_attributes = FALSE
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("full_bike_", .analysis_name),
    travel_mode_type = "Bicycle",
    output_type = "Volume",
    origin_zone_set = .segment_zone_set,
    date_ranges = list(
      start_date = "01/01/2021",
      end_date = "12/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = TRUE,
    trip_attributes = FALSE
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("summer_ped_", .analysis_name),
    travel_mode_type = "Pedestrian",
    output_type = "Volume",
    origin_zone_set = .segment_zone_set,
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = TRUE,
    trip_attributes = FALSE
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("full_ped_", .analysis_name),
    travel_mode_type = "Pedestrian",
    output_type = "Volume",
    origin_zone_set = .segment_zone_set,
    date_ranges = list(
      start_date = "01/01/2021",
      end_date = "12/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = TRUE,
    trip_attributes = FALSE
  ))

  # run home locations on buffer zones
  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("summer_bike_homes_", .analysis_name),
    travel_mode_type = "Bicycle",
    output_type = "Volume",
    origin_zone_set = .buffer_zone_set,
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = FALSE,
    trip_attributes = FALSE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    zone_intersection_type = "trips_by_pass_through_setting"
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("full_bike_homes_", .analysis_name),
    travel_mode_type = "Bicycle",
    output_type = "Volume",
    origin_zone_set = .buffer_zone_set,
    date_ranges = list(
      start_date = "01/01/2021",
      end_date = "12/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = FALSE,
    trip_attributes = FALSE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    zone_intersection_type = "trips_by_pass_through_setting"
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("summer_ped_homes_", .analysis_name),
    travel_mode_type = "Pedestrian",
    output_type = "Volume",
    origin_zone_set = .buffer_zone_set,
    date_ranges = list(
      start_date = "06/01/2021",
      end_date = "08/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = FALSE,
    trip_attributes = FALSE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    zone_intersection_type = "trips_by_pass_through_setting"
  ))

  Sys.sleep(6)
  try(create_streetlight_analysis(
    login_email = login_email,
    analysis_type = "Zone_Activity_Analysis",
    analysis_name = paste0("full_ped_homes_", .analysis_name),
    travel_mode_type = "Pedestrian",
    output_type = "Volume",
    origin_zone_set = .buffer_zone_set,
    date_ranges = list(
      start_date = "01/01/2021",
      end_date = "12/31/2021"
    ),
    day_types = day_types,
    day_parts = day_parts,
    tags = list("streetlightR"),
    traveler_attributes = FALSE,
    trip_attributes = FALSE,
    enable_home_work_locations = TRUE,
    hwl_enable_visitor = TRUE,
    zone_intersection_type = "trips_by_pass_through_setting"
  ))
}

analysis_date <- "2023.07.12"

if (initiate_coverage_review == TRUE) {
  run_trail_21_analysis(
    .analysis_name = paste0("dnr_", analysis_date),
    .segment_zone_set = dnr_segment_zones,
    .buffer_zone_set = dnr_buffer_zones
  )
  Sys.sleep(6)
  run_trail_21_analysis(
    .analysis_name = paste0("gmn_", analysis_date),
    .segment_zone_set = gmn_segment_zones,
    .buffer_zone_set = gmn_buffer_zones
  )
  Sys.sleep(6)
  run_trail_21_analysis(
    .analysis_name = paste0("metro_", analysis_date),
    .segment_zone_set = metro_segment_zones,
    .buffer_zone_set = metro_buffer_zones
  )
}



fetch_trail_coverage_data <- function(.analysis_name) {
  ### summer & full 2021 activity ###
  cat("zone activity")
  bike_summer <- get_analysis_data(
    analysis_name = paste0("summer_bike_", .analysis_name),
    metric = "za_bike"
  )
  Sys.sleep(6)
  bike_full <- get_analysis_data(
    analysis_name = paste0("full_bike_", .analysis_name),
    metric = "za_bike"
  )
  Sys.sleep(6)
  ped_summer <- get_analysis_data(
    analysis_name = paste0("summer_ped_", .analysis_name),
    metric = "za_ped"
  )
  Sys.sleep(6)
  ped_full <- get_analysis_data(
    analysis_name = paste0("full_ped_", .analysis_name),
    metric = "za_ped"
  )

  summer21_activity <- bind_rows(bike_summer, ped_summer)
  full21_activity <- bind_rows(bike_full, ped_full)

  ### fetch summer & full 2021 demographic data ###
  cat("demographics")
  Sys.sleep(6)
  bike_summer_equity <- get_analysis_data(
    analysis_name = paste0("summer_bike_", .analysis_name),
    metric = "zone_traveler_equity_bike"
  )
  Sys.sleep(6)
  bike_full_equity <- get_analysis_data(
    analysis_name = paste0("full_bike_", .analysis_name),
    metric = "zone_traveler_equity_bike"
  )
  Sys.sleep(6)
  ped_summer_equity <- get_analysis_data(
    analysis_name = paste0("summer_ped_", .analysis_name),
    metric = "zone_traveler_equity_ped"
  )
  Sys.sleep(6)
  ped_full_equity <- get_analysis_data(
    analysis_name = paste0("full_ped_", .analysis_name),
    metric = "zone_traveler_equity_ped"
  )
  Sys.sleep(6)
  bike_summer_education_income <- get_analysis_data(
    analysis_name = paste0("summer_bike_", .analysis_name),
    metric = "zone_traveler_education_income_bike"
  )
  Sys.sleep(6)
  bike_full_education_income <- get_analysis_data(
    analysis_name = paste0("full_bike_", .analysis_name),
    metric = "zone_traveler_education_income_bike"
  )
  Sys.sleep(6)
  ped_summer_education_income <- get_analysis_data(
    analysis_name = paste0("summer_ped_", .analysis_name),
    metric = "zone_traveler_education_income_ped"
  )
  Sys.sleep(6)
  ped_full_education_income <- get_analysis_data(
    analysis_name = paste0("full_ped_", .analysis_name),
    metric = "zone_traveler_education_income_ped"
  )

  summer21_demos <- bind_rows(
    full_join(bike_summer_equity, bike_summer_education_income),
    full_join(ped_summer_equity, ped_summer_education_income)
  )
  full21_demos <- bind_rows(
    full_join(bike_full_equity, bike_full_education_income),
    full_join(ped_full_equity, ped_full_education_income)
  )

  ### fetch summer & full 2021 home locations: zips, block groups, states ###
  cat("home locations")
  Sys.sleep(6)
  summer_bike_home_zips <- get_analysis_data(
    analysis_name = paste0("summer_bike_homes_", .analysis_name),
    metric = "home_zip_codes_bike"
  )
  Sys.sleep(6)
  summer_bike_home_bgs <- get_analysis_data(
    analysis_name = paste0("summer_bike_homes_", .analysis_name),
    metric = "home_block_groups_bike"
  )
  Sys.sleep(6)
  summer_bike_home_states <- get_analysis_data(
    analysis_name = paste0("summer_bike_homes_", .analysis_name),
    metric = "home_state_bike"
  )
  Sys.sleep(6)
  full_bike_home_zips <- get_analysis_data(
    analysis_name = paste0("full_bike_homes_", .analysis_name),
    metric = "home_zip_codes_bike"
  )
  Sys.sleep(6)
  full_bike_home_bgs <- get_analysis_data(
    analysis_name = paste0("full_bike_homes_", .analysis_name),
    metric = "home_block_groups_bike"
  )
  Sys.sleep(6)
  full_bike_home_states <- get_analysis_data(
    analysis_name = paste0("full_bike_homes_", .analysis_name),
    metric = "home_state_bike"
  )
  Sys.sleep(6)
  summer_ped_home_zips <- get_analysis_data(
    analysis_name = paste0("summer_ped_homes_", .analysis_name),
    metric = "home_zip_codes_ped"
  )
  Sys.sleep(6)
  summer_ped_home_bgs <- get_analysis_data(
    analysis_name = paste0("summer_ped_homes_", .analysis_name),
    metric = "home_block_groups_ped"
  )
  Sys.sleep(6)
  summer_ped_home_states <- get_analysis_data(
    analysis_name = paste0("summer_ped_homes_", .analysis_name),
    metric = "home_state_ped"
  )
  Sys.sleep(6)
  full_ped_home_zips <- get_analysis_data(
    analysis_name = paste0("full_ped_homes_", .analysis_name),
    metric = "home_zip_codes_ped"
  )
  Sys.sleep(6)
  full_ped_home_bgs <- get_analysis_data(
    analysis_name = paste0("full_ped_homes_", .analysis_name),
    metric = "home_block_groups_ped"
  )
  Sys.sleep(6)
  full_ped_home_states <- get_analysis_data(
    analysis_name = paste0("full_ped_homes_", .analysis_name),
    metric = "home_state_ped"
  )

  summer21_homes <- list(
    zips = bind_rows(summer_bike_home_zips, summer_ped_home_zips),
    bgs = bind_rows(summer_bike_home_bgs, summer_ped_home_bgs),
    states = bind_rows(summer_bike_home_states, summer_ped_home_states)
  )
  full21_homes <- list(
    zips = bind_rows(full_bike_home_zips, full_ped_home_zips),
    bgs = bind_rows(full_bike_home_bgs, full_ped_home_bgs),
    states = bind_rows(full_bike_home_states, full_ped_home_states)
  )

  return(list(
    summer21_activity = summer21_activity,
    full21_activity = full21_activity,
    summer21_demos = summer21_demos,
    full21_demos = full21_demos,
    summer21_homes = summer21_homes,
    full21_homes = full21_homes
  ))
}

if (fetch_coverage_review == TRUE) {
  dnr_demos_homes <- fetch_trail_coverage_data(.analysis_name = paste0("dnr_", analysis_date))
  gmn_demos_homes <- fetch_trail_coverage_data(.analysis_name = paste0("gmn_", analysis_date))
  metro_demos_homes <- fetch_trail_coverage_data(.analysis_name = paste0("metro_", analysis_date))

  save(dnr_demos_homes, gmn_demos_homes, metro_demos_homes,
    file = file.path(here(), "data-intermediate/trails/coverage-review-data.rda")
  )
}
