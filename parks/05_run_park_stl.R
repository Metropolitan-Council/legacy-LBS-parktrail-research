# This script creates all StreetLight analyses used for park analysis. Raw StreetLight results are saved to
# `data-intermediate/parks`.

##### Weekly analyses #####

if (run_weekly == TRUE | fetch_weekly == TRUE) {
  if (run_index == TRUE | fetch_index == TRUE) {
    ### index ###
    week_index <- run_stl_za(
      run = run_index,
      fetch = fetch_index,
      metric = "Index",
      mode = "All_Vehicles",
      date_sequence = week_sequence,
      date_sequence_label = "label",
      time_step = "Weekly",
      zone_set_name = zone_set_name,
      analysis_prefix = paste0(park_stl_date_id, "_p_wk_ind")
    )
  }

  ### volume ###
  week_vehicle <- run_stl_za(
    run = run_weekly,
    fetch = fetch_weekly,
    metric = "Volume",
    mode = "All_Vehicles",
    date_sequence = week_sequence,
    date_sequence_label = "label",
    time_step = "Weekly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_wk_veh")
  )
}

if (fetch_weekly == TRUE) {
  save(week_vehicle, file = file.path(here(), "data-intermediate/parks/raw-week-volume.rda"))
}
if (fetch_index == TRUE) {
  save(week_index, file = file.path(here(), "data-intermediate/parks/raw-week-index.rda"))
}

##### Biweekly analyses #####
if (run_biweekly == TRUE | fetch_biweekly == TRUE) {
  ### volume ###
  biweekly_vehicle <- run_stl_za(
    run = run_biweekly,
    fetch = fetch_biweekly,
    metric = "Volume",
    mode = "All_Vehicles",
    date_sequence = biweekly_sequence,
    date_sequence_label = "label",
    time_step = "Biweekly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_biwk_veh")
  )
}

if (fetch_biweekly == TRUE) {
  save(biweekly_vehicle, file = file.path(here(), "data-intermediate/parks/raw-biweekly-volume.rda"))
}

##### Monthly analyses #####
if (run_monthly == TRUE | fetch_monthly == TRUE) {
  ### vehicle ###
  month_veh <- run_stl_za(
    run = run_monthly,
    fetch = fetch_monthly,
    metric = "Volume",
    mode = "All_Vehicles",
    date_sequence = month_sequence,
    date_sequence_label = "label",
    time_step = "Monthly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_month_veh")
  )

  ### bike ###
  month_bike <- run_stl_za(
    run = run_monthly,
    fetch = fetch_monthly,
    metric = "Volume",
    mode = "Bicycle",
    date_sequence = month_sequence,
    date_sequence_label = "label",
    time_step = "Monthly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_month_bike")
  )

  ### ped ###
  month_ped <- run_stl_za(
    run = run_monthly,
    fetch = fetch_monthly,
    metric = "Volume",
    mode = "Pedestrian",
    date_sequence = month_sequence,
    date_sequence_label = "label",
    time_step = "Monthly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_month_ped")
  )
}

if (fetch_monthly == TRUE) {
  save(month_veh, month_bike, month_ped,
    file = file.path(here(), "data-intermediate/parks/raw-monthly-volume.rda")
  )
}

#### Origin-Destination analyses #####
if (run_od == TRUE | fetch_od == TRUE) {
  ### vehicle
  week_od_veh <- run_stl_od(
    run = run_od,
    fetch = fetch_od,
    metric = "Volume",
    mode = "All_Vehicles",
    date_sequence = week_sequence,
    date_sequence_label = "label",
    time_step = "Weekly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_veh_od")
  )

  ### bicycle
  month_od_bike <- run_stl_od(
    run = run_od,
    fetch = fetch_od,
    metric = "Volume",
    mode = "Bicycle",
    date_sequence = month_sequence,
    date_sequence_label = "label",
    time_step = "Monthly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_bike_od")
  )

  ### pedestrian
  month_od_ped <- run_stl_od(
    run = run_od,
    fetch = fetch_od,
    metric = "Volume",
    mode = "Pedestrian",
    date_sequence = month_sequence,
    date_sequence_label = "label",
    time_step = "Monthly",
    zone_set_name = zone_set_name,
    analysis_prefix = paste0(park_stl_date_id, "_p_ped_od")
  )
}

if (fetch_od == TRUE) {
  save(week_od_veh, month_od_bike, month_od_ped,
    file = file.path(here(), "data-intermediate/parks/raw-od-volume.rda")
  )
}


##### Supplemental analyses #####
if (run_supplemental == TRUE | fetch_supplemental == TRUE) {
  ### home locations ###
  dnr_home_locations <- run_home_locations(
    run = run_supplemental,
    fetch = fetch_supplemental,
    include_demographics = TRUE,
    date_sequence = demographic_date_sequence,
    date_sequence_label = "label",
    zone_set_name = str_replace(perim_zone_set_name, "legacy", "dnr"),
    analysis_prefix = paste0(park_stl_date_id, "_dnr")
  )

  gmn_home_locations <- run_home_locations(
    run = run_supplemental,
    fetch = fetch_supplemental,
    include_demographics = TRUE,
    date_sequence = demographic_date_sequence,
    date_sequence_label = "label",
    zone_set_name = str_replace(perim_zone_set_name, "legacy", "gmn"),
    analysis_prefix = paste0(park_stl_date_id, "_gmn")
  )

  metro_home_locations <- run_home_locations(
    run = run_supplemental,
    fetch = fetch_supplemental,
    include_demographics = TRUE,
    date_sequence = demographic_date_sequence,
    date_sequence_label = "label",
    zone_set_name = str_replace(perim_zone_set_name, "legacy", "metro"),
    analysis_prefix = paste0(park_stl_date_id, "_metro")
  )

  if (fetch_supplemental == TRUE) {
    save(dnr_home_locations,
      gmn_home_locations,
      metro_home_locations,
      file = file.path(here(), "data-intermediate/parks/raw-home-locations.rda")
    )
  }

  ### hourly arrivals ###
  hourly <- run_hourly_activity(
    run = run_supplemental,
    fetch = fetch_supplemental,
    dates = demographic_date_sequence[1, ],
    label = "summer2021",
    zone_set_name = zone_set_name,
    analysis_prefix = park_stl_date_id
  )

  if (fetch_supplemental == TRUE) {
    save(hourly, file = file.path(here(), "data-intermediate/parks/raw-hourly.rda"))
  }
}
