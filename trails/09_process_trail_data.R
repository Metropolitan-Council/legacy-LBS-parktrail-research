# This script processes the raw trail StreetLight data fetched in `08_run_core_trails.R`
# Both segment-level and trail-level data is generated. Processed data is saved to
# `data-intermediate/processed`.

##### setup #####
load(file.path(here(), "data-intermediate/trails/raw-trail-stl.rda"))
load(file.path(here(), "data-intermediate/trails/trail-metadata.rda"))

## combine all trail data
monthly_trails <- bind_rows(dnr, gmn, metro)
raw_monthly_segments <- monthly_trails
save(raw_monthly_segments, file = file.path(here(), "data-intermediate/trails/raw-monthly-segments.rda"))

##### segment volume #####

monthly_segment_volume <- monthly_trails %>%
  mutate(volume = as.numeric(average_daily_zone_traffic_st_l_volume)) %>%
  left_join(trail_segment_metadata %>%
    select(zone_name, approx_length_feet, approx_length_miles)) %>%
  # remove segments less than 50ft
  filter(approx_length_feet >= set_units(50, "ft")) %>%
  # separate bike & ped
  pivot_wider(names_from = mode_of_travel, values_from = volume) %>%
  rename(
    bike = `Bicycle - StL Bicycle Volume`,
    ped = `Pedestrian - StL Pedestrian Volume`
  ) %>%
  # get number of days in month
  left_join(month_sequence) %>%
  # convert average daily values to monthly totals
  replace_na(list(bike = 0, ped = 0)) %>%
  mutate(
    bike_use = bike * days_in_month,
    ped_use = ped * days_in_month,
    bike_miles = bike * days_in_month * as.numeric(approx_length_miles),
    ped_miles = ped * days_in_month * as.numeric(approx_length_miles)
  ) %>%
  group_by(zone_name, day_type, day_part, start_date) %>%
  summarise(
    counter_estimate = sum(bike_use, ped_use),
    bike_counter_estimate = sum(bike_use),
    ped_counter_estimate = sum(ped_use),
    miles_traveled = sum(bike_miles, ped_miles),
    bike_miles_traveled = sum(bike_miles),
    ped_miles_traveled = sum(ped_miles), .groups = "keep"
  ) %>%
  # fill missing values with zero
  ungroup() %>%
  complete(start_date, day_type, zone_name,
    fill = list(
      counter_estimate = 0, bike_counter_estimate = 0, ped_counter_estimate = 0,
      miles_traveled = 0, bike_miles_traveled = 0, ped_miles_traveled = 0
    )
  ) %>%
  # calculate mode share
  mutate(
    modeshare_bike_miles = bike_miles_traveled / miles_traveled,
    modeshare_ped_miles = ped_miles_traveled / miles_traveled
  ) %>%
  # replace NaNs with NAs
  mutate(
    modeshare_bike_miles = ifelse(is.nan(modeshare_bike_miles), 0, modeshare_bike_miles),
    modeshare_ped_miles = ifelse(is.nan(modeshare_ped_miles), 0, modeshare_ped_miles)
  ) %>%
  # join all metadata back in
  left_join(trail_segment_metadata) %>%
  # get month/year
  mutate(
    month = month(mdy(start_date), label = TRUE),
    year = year(mdy(start_date))
  )

correct_trail_meta_test <- filter(monthly_segment_volume, is.na(system))
if (nrow(correct_trail_meta_test) > 0) {
  stop("Some cleaned trail segments are missing correct metadata")
}

monthly_segment_volume %>%
  filter(
    year == 2021,
    str_detect(day_type, "All"),
    unit_label == "Paul Bunyan"
  ) %>%
  group_by(osm_id) %>%
  summarise(annual_counter_estimate = sum(counter_estimate), .groups = "keep") %>%
  arrange(-annual_counter_estimate) # 74333 at osm_id 98942505

# TODO: document number of segments entirely dropped

##### monthly trail volume #####
monthly_trail_volume <- monthly_segment_volume %>%
  select(-modeshare_bike_miles, -modeshare_ped_miles) %>%
  # summarise at trail level
  group_by(unit_id, start_date, month, year, day_type) %>%
  summarise(
    counter_estimate = max(counter_estimate),
    bike_counter_estimate = max(bike_counter_estimate),
    ped_counter_estimate = max(ped_counter_estimate),
    miles_traveled = sum(miles_traveled),
    bike_miles_traveled = sum(bike_miles_traveled),
    ped_miles_traveled = sum(ped_miles_traveled),
    .groups = "keep"
  ) %>%
  mutate(
    modeshare_bike_miles = bike_miles_traveled / miles_traveled,
    modeshare_ped_miles = ped_miles_traveled / miles_traveled
  ) %>%
  mutate(
    modeshare_bike_miles = ifelse(is.nan(modeshare_bike_miles), NA, modeshare_bike_miles),
    modeshare_ped_miles = ifelse(is.nan(modeshare_ped_miles), NA, modeshare_ped_miles)
  ) %>%
  replace_na(list(modeshare_bike_miles = 0, modeshare_ped_miles = 0)) %>%
  left_join(trail_metadata)

monthly_trail_volume %>%
  filter(str_detect(day_type, "All")) %>%
  group_by(year) %>%
  summarise(total = sum(miles_traveled) / 1e6, .groups = "keep")

monthly_trail_volume %>%
  filter(
    str_detect(day_type, "All"),
    year == 2021,
    unit_label == "Paul Bunyan"
  ) %>%
  group_by(year) %>%
  summarise(annual_counter_estimate = sum(counter_estimate), .groups = "keep") # 92565

##### annual trail volume #####
annual_trail_volume <- monthly_trail_volume %>%
  group_by(unit_id, year, day_type) %>%
  summarise(
    counter_estimate = sum(counter_estimate),
    bike_counter_estimate = sum(bike_counter_estimate),
    ped_counter_estimate = sum(ped_counter_estimate),
    miles_traveled = sum(miles_traveled),
    bike_miles_traveled = sum(bike_miles_traveled),
    ped_miles_traveled = sum(ped_miles_traveled),
    .groups = "keep"
  ) %>%
  mutate(
    modeshare_bike_miles = bike_miles_traveled / miles_traveled,
    modeshare_ped_miles = ped_miles_traveled / miles_traveled
  ) %>%
  mutate(
    modeshare_bike_miles = ifelse(is.nan(modeshare_bike_miles), 0, modeshare_bike_miles),
    modeshare_ped_miles = ifelse(is.nan(modeshare_ped_miles), 0, modeshare_ped_miles)
  ) %>%
  left_join(trail_metadata)

annual_trail_volume %>%
  filter(str_detect(day_type, "All")) %>%
  group_by(system, year) %>%
  summarise(total = sum(miles_traveled) / 1e6, .groups = "keep")

annual_trail_volume %>%
  filter(
    str_detect(day_type, "All"),
    year == 2021,
    unit_label == "Paul Bunyan"
  ) # 92565


##### seasonal trail volume #####
seasonal_trail_volume <- monthly_trail_volume %>%
  mutate(season = case_when(
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",
    month %in% c("Mar", "Apr", "May") ~ "Spring",
    month %in% c("Jun", "Jul", "Aug") ~ "Summer",
    TRUE ~ "Fall"
  )) %>%
  group_by(unit_id, season, year, day_type) %>%
  summarise(
    counter_estimate = sum(counter_estimate),
    bike_counter_estimate = sum(bike_counter_estimate),
    ped_counter_estimate = sum(ped_counter_estimate),
    miles_traveled = sum(miles_traveled),
    bike_miles_traveled = sum(bike_miles_traveled),
    ped_miles_traveled = sum(ped_miles_traveled),
    .groups = "keep"
  ) %>%
  mutate(
    modeshare_bike_miles = bike_miles_traveled / miles_traveled,
    modeshare_ped_miles = ped_miles_traveled / miles_traveled
  ) %>%
  mutate(
    modeshare_bike_miles = ifelse(is.nan(modeshare_bike_miles), 0, modeshare_bike_miles),
    modeshare_ped_miles = ifelse(is.nan(modeshare_ped_miles), 0, modeshare_ped_miles)
  ) %>%
  left_join(trail_metadata)

seasonal_trail_volume %>%
  filter(str_detect(day_type, "All")) %>%
  group_by(year) %>%
  summarise(total = sum(miles_traveled) / 1e6, .groups = "keep")

##### hourly trail activity #####
hourly <- bind_rows(
  dnr_hourly$bike_hourly, dnr_hourly$ped_hourly,
  gmn_hourly$bike_hourly, gmn_hourly$ped_hourly,
  metro_hourly$bike_hourly, metro_hourly$ped_hourly
)

hourly_trails <- hourly %>%
  filter(!str_detect(day_part, "All")) %>%
  left_join(trail_segment_metadata, by = "zone_name") %>%
  # remove segments less than 50ft
  filter(approx_length_feet >= set_units(50, "ft")) %>%
  mutate(volume = as.numeric(average_daily_zone_traffic_st_l_volume)) %>%
  select(day_type, mode_of_travel, zone_name, unit_id, osm_id, day_part, volume) %>%
  pivot_wider(names_from = mode_of_travel, values_from = volume) %>%
  rename(
    bike = `Bicycle - StL Bicycle Volume`,
    ped = `Pedestrian - StL Pedestrian Volume`
  ) %>%
  complete(day_part, day_type, nesting(zone_name, unit_id, osm_id),
    fill = list(bike = 0, ped = 0)
  ) %>%
  # get percent of use by hour
  group_by(unit_id, day_part, day_type) %>%
  summarise(
    sumbike = sum(bike),
    sumped = sum(ped),
    sumtotal = sumbike + sumped,
    .groups = "keep"
  ) %>%
  ungroup() %>%
  group_by(unit_id, day_type) %>%
  mutate(
    percent_bike = sumbike / sum(sumbike),
    percent_ped = sumped / sum(sumped),
    percent_total = sumtotal / sum(sumtotal)
  ) %>%
  mutate(
    percent_bike = ifelse(is.nan(percent_bike), 0, percent_bike),
    percent_ped = ifelse(is.nan(percent_ped), 0, percent_bike),
    percent_total = ifelse(is.nan(percent_total), 0, percent_total)
  ) %>%
  # clean up hourly labels
  group_by(day_part) %>%
  mutate(
    hourm = str_remove_all(str_extract(day_part, "\\([A-Za-z0-9]+-"), "\\(|-"),
    ampm = ifelse(str_detect(hourm, "pm"), "pm", "am"),
    hour = as.numeric(str_remove_all(hourm, "[A-Za-z]")),
    hour = ifelse(ampm == "pm", hour + 12, hour),
    time = as.POSIXct(strptime(paste0(hour, ":00"), format = "%H:%M"), format = "%H:%M")
  ) %>%
  # select(-c(hourm, ampm, hour)) %>%
  left_join(trail_metadata)

##### save everything together #####
save(monthly_segment_volume, monthly_trail_volume, seasonal_trail_volume, annual_trail_volume, hourly_trails,
  file = file.path(here(), "data-intermediate/processed/trail-volume.rda")
)
