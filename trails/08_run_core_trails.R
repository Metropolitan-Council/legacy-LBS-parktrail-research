# This script runs the core StreetLight analyses for trails: monthly bike & pedestrian
# zone activity analyses, and hourly bike & pedestrian zone activity analyses for the summer
# of 2021. Once analyses are fetched, raw data is saved to `data-intermediate/trails`.


##### setup #####
source(file.path(here(), "R/_run_stl_functions.R"))
trail_stl_date_id <- str_remove_all(substr(analysis_date, 3, nchar(analysis_date)), "\\.|0")


summer_date_sequence <- data.frame(start_date = "06/01/2021",
                            end_date = "08/31/2021",
                            label = "sum21")

##### DNR #####
dnr_bike <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Bicycle", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = "segments-DNR_2023.06.06", 
  analysis_prefix = paste0(trail_stl_date_id, "_dnr_bikes")
)

dnr_ped <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Pedestrian", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = "segments-DNR_2023.06.06", 
  analysis_prefix = paste0(trail_stl_date_id, "_dnr_peds")
)

if(fetch_trails == TRUE){
  dnr <- bind_rows(dnr_bike, dnr_ped)
}

dnr_hourly <- run_hourly_activity(
  run = run_trails, 
  fetch = fetch_trails,
  trail = TRUE,
  dates = summer_date_sequence,
  label = "summer2021",
  zone_set_name = "segments-DNR_2023.06.06", 
  analysis_prefix = paste0(trail_stl_date_id, "_dnrt")
)

##### Greater MN #####
gmn_bike <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Bicycle", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = paste0("segments_greatermn_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_greatermn_bike")
)

gmn_ped <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Pedestrian", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = paste0("segments_greatermn_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_greatermn_ped")
)

if(fetch_trails == TRUE){
gmn <- bind_rows(gmn_bike, gmn_ped)
}

gmn_hourly <- run_hourly_activity(
  run = run_trails, 
  fetch = fetch_trails,
  trail = TRUE,
  dates = summer_date_sequence,
  label = "summer2021",
  zone_set_name = paste0("segments_greatermn_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_greatermn")
)

##### Metro #####
metro_bike <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Bicycle", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = paste0("segments_metro_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_metro_bike")
)

metro_ped <- run_stl_za(
  run = run_trails, 
  fetch = fetch_trails, 
  metric = "Volume", 
  mode = "Pedestrian", 
  date_sequence = month_sequence,
  date_sequence_label = "label",
  time_step = "Monthly", 
  zone_set_name = paste0("segments_metro_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_metro_ped")
)

if(fetch_trails == TRUE){
metro <- bind_rows(metro_bike, metro_ped)
}

metro_hourly <- run_hourly_activity(
  run = run_trails, 
  fetch = fetch_trails,
  trail = TRUE,
  dates = summer_date_sequence,
  label = "summer2021",
  zone_set_name = paste0("segments_metro_", trail_zone_suffix), 
  analysis_prefix = paste0(trail_stl_date_id, "_met")
)

##### save #####
if(fetch_trails == TRUE){
  save(dnr, dnr_hourly, gmn, gmn_hourly, metro, metro_hourly,
       file = file.path(here(), "data-intermediate/trails/raw-trail-stl.rda"))
}
