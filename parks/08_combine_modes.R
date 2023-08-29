# This script combines the filled & imputed vehicle, bicycle, and pedestrian data. Bicycle and
# pedestrian data is estimated at the weekly level using scaling factors based on monthly data.
# Combined data is saved to `data-intermediate/parks` and `data-intermediate/processed`.

##### setup #####
load(file.path(here(), "data-intermediate/parks/filled-imputed-volume.rda"))

### vehicle ###
vehicle <- weekly %>%
  mutate(mode = "Vehicle", 
         year = year(start_date)) %>%
  rename(vehicle_source = source)

### bike ###
bike <- filled_monthly_bike %>%
  mutate(mode = "Bicycle", 
         days_in_month = days_in_month(month(start_date)), 
         monthly_bike_volume = bike_volume * days_in_month, 
         removed_monthly_bike_intrapark_trips = removed_bike_intrapark_trips * days_in_month) %>%
  rename(bike_source = source)


### ped ###
ped <- filled_monthly_ped %>%
  mutate(mode = "Pedestrian", 
         days_in_month = days_in_month(month(start_date)), 
         monthly_ped_volume = ped_volume * days_in_month, 
         removed_monthly_ped_intrapark_trips = removed_ped_intrapark_trips * days_in_month) %>%
  rename(ped_source = source)

##### combine all modes #####

## first, expand vehicle data so we have DAILY records
veh_expanded <- vehicle[, list(zone_name = zone_name, mode = mode, year = year, start_date = start_date, end_date = end_date, label = label,
                               vehicle_source = vehicle_source, removed_daily_intrapark_trips = removed_daily_intrapark_trips, volume = volume,
                               date = seq(start_date, end_date, by = "day")), by = 1:nrow(vehicle)] %>%
  rename(vehicles = volume)

## use daily records to get monthly vehicles
# use daily rather than monthly data because daily includes all of the imputation, etc from previous scripts
monthly_vehicle <- veh_expanded %>%
  mutate(month_lab = month(date, label = TRUE), 
         month = month(date),
         vehicle_source = factor(vehicle_source, levels = c("Weekly StreetLight", "Biweekly StreetLight", "Monthly StreetLight", "Imputed"),
                                 ordered = TRUE)) %>%
  group_by(month, month_lab, year, zone_name) %>%
  summarise(monthly_vehicles = sum(vehicles),
            removed_monthly_intrapark_vehicles = sum(removed_daily_intrapark_trips),
            vehicle_source = max(vehicle_source)) %>%
  mutate(label = paste0(month, ".", year))

## combine monthly data
all_monthly <- monthly_vehicle %>%
  full_join(bike %>% select(-mode), by = c("month", "year", "zone_name", "label")) %>%
  full_join(ped %>% select(-mode), by = c("month", "year", "zone_name", "label", "start_date", "end_date", "days_in_month"))

# save this data for plots
saveRDS(all_monthly, file.path(here(), "data-intermediate/parks/all-monthly-with-intrapark.RDS"))

## get bike & ped scaling factors using monthly data
# from here on out, disregard intrapark 
scaling_factors <- all_monthly %>%
  group_by(zone_name, month, year) %>%
  summarise(bike_scaling_factor = monthly_bike_volume/monthly_vehicles,
            ped_scaling_factor = monthly_ped_volume/monthly_vehicles)

## use scaling factors to convert monthly bike/ped data to daily/weekly 
all_modes_expanded <- veh_expanded %>%
  mutate(month = month(date)) %>%
  full_join(scaling_factors) %>%
  rowwise() %>%
  mutate(bike = max(ceiling(vehicles * bike_scaling_factor), 0),
         ped = max(ceiling(vehicles * ped_scaling_factor), 0))

## add metadata to get vehicle multipliers
all_modes_expanded <- all_modes_expanded %>%
  full_join(park_metadata) %>%
  mutate(vehicle_visitors = ceiling(vehicles * vehicle_multiplier), 
         total = vehicle_visitors + bike + ped)

save(all_modes_expanded, file = file.path(here(), "data-temp/all_modes_expanded.rda"))

##### combine binned time modes #####
load(file.path(here(), "data-intermediate/parks/raw-hourly.rda"))

hourly_parks <- do.call(bind_rows, hourly) %>%
  filter(intersection_type == "Trip End", 
         !str_detect(day_part, "All")) %>%
  left_join(park_metadata) %>%
  pivot_wider(names_from = mode_of_travel, values_from = average_daily_zone_traffic_st_l_volume) %>%
  rename(vehicles = `All Vehicles LBS Plus - StL All Vehicles Volume`, 
         bike = `Bicycle - StL Bicycle Volume`, 
         ped = `Pedestrian - StL Pedestrian Volume`) %>%
  mutate(vehicle_visitors = vehicles * vehicle_multiplier) %>%
  complete(zone_name, day_type, day_part) %>%
  replace_na(list(vehicle_visitors = 0, bike = 0, ped = 0)) %>%
  group_by(zone_name, day_type, day_part) %>%
  summarise(total = sum(vehicle_visitors + bike + ped), 
            vehicle_visitors = sum(vehicle_visitors), 
            bike = sum(bike), 
            ped = sum(ped)) %>%
  mutate(vehicle_share = vehicle_visitors/total,
         bike_share = bike/total, 
         ped_share = ped/total) %>%
  group_by(day_part) %>%
  mutate(hourm = str_remove_all(str_extract(day_part, "\\([A-Za-z0-9]+-"), "\\(|-"),
         ampm = ifelse(str_detect(hourm, "pm"), "pm", "am"),
         hour = as.numeric(str_remove_all(hourm, "[A-Za-z]")),
         hour = ifelse(ampm == "pm", hour + 12, hour),
         time = as.POSIXct(strptime(paste0(hour, ":00"), format = "%H:%M"), format = "%H:%M")) %>%
  select(-c(hourm, ampm, hour))

hourly_parks_long <- hourly_parks %>%
  select(-c(vehicle_share, bike_share, ped_share, total)) %>%
  rename(Vehicle = vehicle_visitors, Bicycle = bike, Pedestrian = ped) %>%
  pivot_longer(cols = c(Vehicle, Bicycle, Pedestrian),
               names_to = "Mode", values_to = "volume") %>%
  group_by(zone_name, day_type, time) %>%
  mutate(total_volume = sum(volume)) %>%
  ungroup() %>%
  group_by(zone_name, day_type, time, Mode) %>%
  mutate(share = ifelse(total_volume == 0, 0,
                        volume / total_volume)) 

## save ##
save(hourly_parks, hourly_parks_long, 
     file = file.path(here(), "data-intermediate/processed/hourly-parks.rda"))
