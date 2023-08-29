# This script fills in any remaining missing data from `06_fill_in_volume.R`. The script reads partially filled
# data from `data-intermediate/parks` and uses the `imputeTS` package to fill in missing records.
# Imputation is performed at the weekly level for vehicle estimates and at the monthly level for bicycle and
# pedestrian estimates. Bicycle and pedestrian intrapark trips are removed in this script. Imputation is not
# performed if >50% of records are missing (for any mode). Data is saved to `data-intermediate/parks`

##### load data #####
load(file.path(here(), "data-intermediate/parks/partially-filled-volume.rda"))
load(file.path(here(), "data-intermediate/parks/raw-monthly-volume.rda"))

##### vehicle #####
needs_vehicle_imputation <- unique(filter(filled_weekly_results, is.na(volume))$zone_name)

setDT(filled_weekly_results)
start_dates <- unique(filled_weekly_results$start_date) %>%
  as.Date() %>%
  sort()
end_dates <- unique(filled_weekly_results$end_date) %>%
  as.Date() %>%
  sort()

veh_results <- data.frame()

for (i in 1:length(needs_vehicle_imputation)) {
  dat <- filled_weekly_results %>%
    filter(zone_name == needs_vehicle_imputation[i]) %>%
    as.data.table() %>%
    arrange(start_date)

  # create time series of existing estimates, including NAs
  vol <- ts(dat[order(start_date)]$volume, frequency = 52)

  # apply a moving average to impute NAs
  imputed_vol <- na_ma(vol)

  # save results
  new_results <- data.frame(
    zone_name = needs_vehicle_imputation[i],
    volume = vol,
    imputed_volume = imputed_vol,
    start_date = c(start_dates),
    end_date = c(end_dates)
  ) %>%
    # make sure no negative values are imputed
    mutate(imputed_volume = ifelse(imputed_volume < 0, 0, imputed_volume))

  veh_results <- bind_rows(veh_results, new_results)
}

# correctly mark where volume is imputed!
veh_results <- veh_results %>%
  mutate(
    imputed = ifelse((is.na(volume) & !is.na(imputed_volume)),
      TRUE, FALSE
    ),
    zone_name = as.character(zone_name)
  ) %>%
  as.data.frame()

# join back into results
weekly <- filled_weekly_results %>%
  left_join(veh_results) %>%
  mutate(
    source = ifelse(is.na(source), "Imputed", source),
    imputed_volume = round(imputed_volume),
    volume = coalesce(volume, imputed_volume)
  ) %>%
  select(-biweekly_volume, -imputed, -imputed_volume, -monthly_volume)

summarytab <- weekly %>%
  group_by(source) %>%
  summarise(count = n()) %>%
  as.data.table()

print(summarytab)

##### bicycle #####

### basic processing
bike_monthly1 <- month_bike %>%
  # remove any zones dropped from vehicle data
  filter(zone_name %in% park_metadata$zone_name) %>%
  left_join(month_sequence, by = "label") %>%
  filter(
    str_detect(day_part, "All"),
    str_detect(day_type, "All"),
    intersection_type == "Trip End"
  ) %>%
  mutate(
    raw_bike = as.numeric(average_daily_zone_traffic_st_l_volume),
    source = "Monthly StreetLight",
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    end_date = as.Date(end_date, format = "%m/%d/%Y")
  ) %>%
  select(
    zone_name, start_date, end_date, label, month, year,
    source, raw_bike
  )

bike_intrapark <- month_od_bike %>%
  filter(origin_zone_name %in% park_metadata$zone_name) %>%
  left_join(month_sequence, by = "label") %>%
  filter(
    str_detect(day_part, "All"),
    str_detect(day_type, "All"),
    origin_zone_name == destination_zone_name
  ) %>%
  mutate(daily_bike_intrapark_trips = as.numeric(average_daily_o_d_traffic_st_l_volume)) %>%
  select(destination_zone_name, label, daily_bike_intrapark_trips) %>%
  mutate(zone_name = as.character(destination_zone_name)) %>%
  select(-destination_zone_name)

### remove intrapark
bike_monthly <- bike_monthly1 %>%
  left_join(bike_intrapark, by = c("zone_name", "label")) %>%
  rowwise() %>%
  mutate(
    od_removed = ifelse(!is.na(daily_bike_intrapark_trips), TRUE, FALSE),
    removed_bike_intrapark_trips = ifelse(is.na(daily_bike_intrapark_trips), 0, daily_bike_intrapark_trips),
    bike_volume = max(raw_bike - removed_bike_intrapark_trips, 0)
  )


### impute ###
# check for zones that returned no bike data
no_bike_data <- park_metadata %>%
  filter(!zone_name %in% bike_monthly$zone_name) %>%
  select(zone_name)

bike_monthly_expanded <- bike_monthly %>%
  select(zone_name, start_date, end_date, label, month, year, bike_volume, removed_bike_intrapark_trips) %>%
  # make sure zones with no bike data are still represented
  bind_rows(no_bike_data %>%
    mutate(
      start_date = as.Date("2019-01-01"),
      end_date = as.Date("2019-01-31"),
      label = "1.2019",
      month = 1,
      year = 2019
    )) %>%
  filter(!is.na(start_date)) %>%
  mutate(source = "Monthly StreetLight") %>%
  complete(zone_name, nesting(start_date, end_date, label, month, year))

bike_monthly_missing <- bike_monthly_expanded %>%
  filter(is.na(bike_volume)) %>%
  select(-bike_volume)

cat(paste0(
  "\n\nBICYCLE SUMMARY: Missing ", nrow(bike_monthly_missing), " records at ", length(unique(bike_monthly_missing$zone_name)), " zones (",
  prettyNum(100 * (nrow(bike_monthly_missing) / nrow(bike_monthly_expanded)), digits = 3), "% of expected records)."
))

# anywhere missing >50%?
no_bike_imputation <- bike_monthly_expanded %>%
  group_by(zone_name) %>%
  summarise(
    missing = sum(is.na(bike_volume)),
    count = n(),
    pct_missing = sum(is.na(bike_volume) / n())
  ) %>%
  arrange(-pct_missing) %>%
  filter(pct_missing >= 0.5)

# yes! 55 zones will not get bike imputation (NAs will be filled with 0)
needs_bike_imputation <- bike_monthly_missing %>%
  filter(!zone_name %in% no_bike_imputation$zone_name) %>%
  select(zone_name) %>%
  unique()

month_start_dates <- unique(bike_monthly_expanded$start_date) %>%
  as.Date() %>%
  sort()
month_end_dates <- unique(bike_monthly_expanded$end_date) %>%
  as.Date() %>%
  sort()
bike_results <- data.frame()

for (i in 1:nrow(needs_bike_imputation)) {
  dat <- bike_monthly_expanded %>%
    filter(zone_name == needs_bike_imputation[i, ]$zone_name) %>%
    as.data.table() %>%
    arrange(start_date)

  # create time series of existing estimates, including NAs
  vol <- ts(dat[order(start_date)]$bike_volume, frequency = 12)

  # apply a moving average to impute NAs
  imputed_vol <- na_ma(vol)

  # save results
  new_results <- data.frame(
    zone_name = needs_bike_imputation[i, ]$zone_name,
    bike_volume = vol,
    imputed_bike_volume = imputed_vol,
    start_date = c(month_start_dates),
    end_date = c(month_end_dates)
  ) %>%
    # make sure no negative values are imputed
    mutate(imputed_bike_volume = ifelse(imputed_bike_volume < 0, 0, imputed_bike_volume))

  bike_results <- bind_rows(bike_results, new_results)
}

filled_monthly_bike <- bike_monthly_expanded %>%
  left_join(bike_results) %>%
  mutate(
    source = ifelse(is.na(source), "Imputed", source),
    imputed_bike_volume = round(imputed_bike_volume),
    bike_volume = coalesce(bike_volume, imputed_bike_volume)
  ) %>%
  select(-imputed_bike_volume)

# check: this should not return anything
# filled_monthly_bike %>%
#   filter(is.na(bike_volume)) %>%
#   filter(!zone_name %in% no_bike_imputation$zone_name)

# fill remaining NAs with 0
filled_monthly_bike <- filled_monthly_bike %>%
  replace_na(list(
    bike_volume = 0,
    removed_bike_intrapark_trips = 0
  ))

##### pedestrian #####
### basic processing
ped_monthly1 <- month_ped %>%
  # remove any zones dropped from vehicle data
  filter(zone_name %in% park_metadata$zone_name) %>%
  left_join(month_sequence, by = "label") %>%
  filter(
    str_detect(day_part, "All"),
    str_detect(day_type, "All"),
    intersection_type == "Trip End"
  ) %>%
  mutate(
    raw_ped = as.numeric(average_daily_zone_traffic_st_l_volume),
    source = "Monthly StreetLight",
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    end_date = as.Date(end_date, format = "%m/%d/%Y")
  ) %>%
  select(
    zone_name, start_date, end_date, label, month, year,
    source, raw_ped
  )

ped_intrapark <- month_od_ped %>%
  filter(origin_zone_name %in% park_metadata$zone_name) %>%
  left_join(month_sequence, by = "label") %>%
  filter(
    str_detect(day_part, "All"),
    str_detect(day_type, "All"),
    origin_zone_name == destination_zone_name
  ) %>%
  mutate(daily_ped_intrapark_trips = as.numeric(average_daily_o_d_traffic_st_l_volume)) %>%
  select(destination_zone_name, label, daily_ped_intrapark_trips) %>%
  mutate(zone_name = as.character(destination_zone_name)) %>%
  select(-destination_zone_name)

### remove intrapark
ped_monthly <- ped_monthly1 %>%
  left_join(ped_intrapark, by = c("zone_name", "label")) %>%
  rowwise() %>%
  mutate(
    od_removed = ifelse(!is.na(daily_ped_intrapark_trips), TRUE, FALSE),
    removed_ped_intrapark_trips = ifelse(is.na(daily_ped_intrapark_trips), 0, daily_ped_intrapark_trips),
    ped_volume = max(raw_ped - removed_ped_intrapark_trips, 0)
  )


### impute ###
# check for zones that returned no ped data
no_ped_data <- park_metadata %>%
  filter(!zone_name %in% ped_monthly$zone_name) %>%
  select(zone_name)
# none

ped_monthly_expanded <- ped_monthly %>%
  select(zone_name, start_date, end_date, label, month, year, ped_volume, removed_ped_intrapark_trips) %>%
  filter(!is.na(start_date)) %>%
  mutate(source = "Monthly StreetLight") %>%
  complete(zone_name, nesting(start_date, end_date, label, month, year))

ped_monthly_missing <- ped_monthly_expanded %>%
  filter(is.na(ped_volume)) %>%
  select(-ped_volume)

cat(paste0(
  "\n\nBICYCLE SUMMARY: Missing ", nrow(ped_monthly_missing), " records at ", length(unique(ped_monthly_missing$zone_name)), " zones (",
  prettyNum(100 * (nrow(ped_monthly_missing) / nrow(ped_monthly_expanded)), digits = 3), "% of expected records)."
))

# anywhere missing >50%?
no_ped_imputation <- ped_monthly_expanded %>%
  group_by(zone_name) %>%
  summarise(
    missing = sum(is.na(ped_volume)),
    count = n(),
    pct_missing = sum(is.na(ped_volume) / n())
  ) %>%
  arrange(-pct_missing) %>%
  filter(pct_missing >= 0.5)
# also none

needs_ped_imputation <- ped_monthly_missing %>%
  filter(!zone_name %in% no_ped_imputation$zone_name) %>%
  select(zone_name) %>%
  unique()

ped_results <- data.frame()

for (i in 1:nrow(needs_ped_imputation)) {
  dat <- ped_monthly_expanded %>%
    filter(zone_name == needs_ped_imputation[i, ]$zone_name) %>%
    as.data.table() %>%
    arrange(start_date)

  # create time series of existing estimates, including NAs
  vol <- ts(dat[order(start_date)]$ped_volume, frequency = 12)

  # apply a moving average to impute NAs
  imputed_vol <- na_ma(vol)

  # save results
  new_results <- data.frame(
    zone_name = needs_ped_imputation[i, ]$zone_name,
    ped_volume = vol,
    imputed_ped_volume = imputed_vol,
    start_date = c(month_start_dates),
    end_date = c(month_end_dates)
  ) %>%
    # make sure no negative values are imputed
    mutate(imputed_ped_volume = ifelse(imputed_ped_volume < 0, 0, imputed_ped_volume))

  ped_results <- bind_rows(ped_results, new_results)
}

filled_monthly_ped <- ped_monthly_expanded %>%
  left_join(ped_results) %>%
  mutate(
    source = ifelse(is.na(source), "Imputed", source),
    imputed_ped_volume = round(imputed_ped_volume),
    ped_volume = coalesce(ped_volume, imputed_ped_volume)
  ) %>%
  select(-imputed_ped_volume) %>%
  replace_na(list(removed_ped_intrapark_trips = 0))

# check: this should not return anything
# filled_monthly_ped %>%
#   filter(is.na(ped_volume))

save(weekly, filled_monthly_bike, filled_monthly_ped, no_bike_imputation,
  file = file.path(here(), "data-intermediate/parks/filled-imputed-volume.rda")
)
