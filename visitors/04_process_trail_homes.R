## This script reads the park home locations from `data-intermediate/trails` and processes
## the data to align with surveyed home locations (for validation) and with park home locations
## (so that park & trail data can be combined). The results are saved in `data-intermediate`
## before being combined with park data.

##### setup #####
load(file.path(here(), "data-intermediate/trails/coverage-review-data.rda"))


##### combine geographies across systems #####
bgs1 <- bind_rows(
  dnr_demos_homes$summer21_homes$bgs,
  gmn_demos_homes$summer21_homes$bgs,
  metro_demos_homes$summer21_homes$bgs
) %>%
  mutate(label = "summer21") %>%
  bind_rows(
    bind_rows(
      dnr_demos_homes$full21_homes$bgs,
      gmn_demos_homes$full21_homes$bgs,
      metro_demos_homes$full_21_homes$bgs
    ) %>%
      mutate(label = "year21")
  )

zips1 <- bind_rows(
  dnr_demos_homes$summer21_homes$zips,
  gmn_demos_homes$summer21_homes$zips,
  metro_demos_homes$summer21_homes$zips
) %>%
  mutate(label = "summer21") %>%
  bind_rows(
    bind_rows(
      dnr_demos_homes$full21_homes$zips,
      gmn_demos_homes$full21_homes$zips,
      metro_demos_homes$full_21_homes$zips
    ) %>%
      mutate(label = "year21")
  )


states1 <- bind_rows(
  dnr_demos_homes$summer21_homes$states,
  gmn_demos_homes$summer21_homes$states,
  metro_demos_homes$summer21_homes$states
) %>%
  mutate(label = "summer21") %>%
  bind_rows(
    bind_rows(
      dnr_demos_homes$full21_homes$states,
      gmn_demos_homes$full21_homes$states,
      metro_demos_homes$full_21_homes$states
    ) %>%
      mutate(label = "year21")
  )

##### process to match parks #####
bgs <- bgs1 %>%
  mutate(
    volume = as.numeric(average_daily_zone_traffic_st_l_volume),
    mode = case_when(
      str_detect(mode_of_travel, "Bicycle") ~ "Bicycle",
      TRUE ~ "Pedestrian"
    ),
    num_by_home_location = as.numeric(percent_by_home_location) * volume
  ) %>%
  select(-c(
    mode_of_travel, home_and_work_filter, intersection_type, zone_id,
    average_daily_zone_traffic_st_l_volume
  )) %>%
  pivot_wider(names_from = mode, values_from = num_by_home_location) %>%
  replace_na(list(Bicycle = 0, Pedestrian = 0)) %>%
  mutate(total_by_home_location = Bicycle + Pedestrian) %>%
  filter(total_by_home_location != 0) %>%
  # and convert back to percentages
  group_by(label, day_type, day_part, zone_name, block_group_id) %>%
  summarise(total_by_bg = sum(total_by_home_location)) %>%
  ungroup() %>%
  group_by(label, day_type, day_part, zone_name) %>%
  mutate(
    zone_total = sum(total_by_bg),
    percent_by_bg = total_by_bg / zone_total
  ) %>%
  ungroup()

## confirm percentages add up correctly
bgs %>%
  group_by(label, day_type, day_part, zone_name) %>%
  summarise(total = sum(percent_by_bg))

zips <- zips1 %>%
  mutate(
    volume = as.numeric(average_daily_zone_traffic_st_l_volume),
    mode = case_when(
      str_detect(mode_of_travel, "Bicycle") ~ "Bicycle",
      TRUE ~ "Pedestrian"
    ),
    num_by_home_location = as.numeric(percent_by_home_location) * volume
  ) %>%
  select(-c(
    mode_of_travel, home_and_work_filter, intersection_type, zone_id,
    average_daily_zone_traffic_st_l_volume
  )) %>%
  pivot_wider(names_from = mode, values_from = num_by_home_location) %>%
  replace_na(list(Bicycle = 0, Pedestrian = 0)) %>%
  mutate(total_by_home_location = Bicycle + Pedestrian) %>%
  filter(total_by_home_location != 0) %>%
  # and convert back to percentages
  group_by(label, day_type, day_part, zone_name, zip_code) %>%
  summarise(total_by_zip = sum(total_by_home_location)) %>%
  ungroup() %>%
  group_by(label, day_type, day_part, zone_name) %>%
  mutate(
    zone_total = sum(total_by_zip),
    percent_by_zip = total_by_zip / zone_total
  ) %>%
  ungroup()

states <- states1 %>%
  mutate(
    volume = as.numeric(average_daily_zone_traffic_st_l_volume),
    mode = case_when(
      str_detect(mode_of_travel, "Bicycle") ~ "Bicycle",
      TRUE ~ "Pedestrian"
    ),
    num_by_home_location = as.numeric(percent_by_home_location) * volume
  ) %>%
  select(-c(
    mode_of_travel, home_and_work_filter, intersection_type, zone_id,
    average_daily_zone_traffic_st_l_volume
  )) %>%
  pivot_wider(names_from = mode, values_from = num_by_home_location) %>%
  replace_na(list(Bicycle = 0, Pedestrian = 0)) %>%
  mutate(total_by_home_location = Bicycle + Pedestrian) %>%
  filter(total_by_home_location != 0) %>%
  # and convert back to percentages
  group_by(label, day_type, day_part, zone_name, state_name) %>%
  summarise(total_by_state = sum(total_by_home_location)) %>%
  ungroup() %>%
  group_by(label, day_type, day_part, zone_name) %>%
  mutate(
    zone_total = sum(total_by_state),
    percent_by_state = total_by_state / zone_total
  ) %>%
  ungroup()

##### save all #####
trail_bgs <- bgs
trail_zips <- zips
trail_states <- states

save(trail_bgs, trail_zips, trail_states,
  file = file.path(here(), "data-intermediate/visitors/trail-homes.rda")
)
