# This script aggregates the data output by `08_combine_modes.R` into weekly, monthly, seasonal, and 
# annual estimates. Data is saved to `data-intermediate/processed`.

load(file.path(here(), "data-temp/all_modes_expanded.rda"))

##### weekly #####
weekly_volume <- all_modes_expanded %>%
  group_by(zone_name, start_date, end_date) %>%
  summarise(weekly_vehicle_visitors = sum(vehicle_visitors), 
            weekly_vehicles = sum(vehicles), 
            weekly_bike = sum(bike), 
            weekly_ped = sum(ped), 
            weekly_total = weekly_vehicle_visitors + weekly_bike + weekly_ped) %>%
  mutate(vehicle_mode_share = weekly_vehicle_visitors/weekly_total,
         bike_mode_share = weekly_bike/weekly_total,
         ped_mode_share = weekly_ped/weekly_total,
         vehicle_mode_share = ifelse(weekly_bike == 0 & weekly_ped == 0, 1, vehicle_mode_share)) %>%
  ungroup() %>%
  left_join(park_metadata) %>%
  mutate(year = year(start_date))

##### monthly #####
monthly_volume <- all_modes_expanded %>%
  group_by(zone_name, month, year) %>%
  summarise(monthly_vehicle_visitors = sum(vehicle_visitors), 
            monthly_vehicles = sum(vehicles), 
            monthly_bike = sum(bike), 
            monthly_ped = sum(ped), 
            monthly_total = monthly_vehicle_visitors + monthly_bike + monthly_ped) %>%
  mutate(vehicle_mode_share = monthly_vehicle_visitors/monthly_total,
         bike_mode_share = monthly_bike/monthly_total,
         ped_mode_share = monthly_ped/monthly_total,
         vehicle_mode_share = ifelse(monthly_bike == 0 & monthly_ped == 0, 1, vehicle_mode_share)) %>%
  ungroup() %>%
  left_join(park_metadata)

##### season #####
season_volume <- all_modes_expanded %>%
  mutate(season = case_when(
    month %in% c(12, 1, 2) ~ "Winter", 
    month %in% c(3, 4, 5) ~ "Spring", 
    month %in% c(6, 7, 8) ~ "Summer", 
    TRUE ~ "Fall"
    ), 
    season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)) %>%
  group_by(zone_name, season, year) %>%
  summarise(season_vehicle_visitors = sum(vehicle_visitors), 
            season_vehicles = sum(vehicles), 
            season_bike = sum(bike), 
            season_ped = sum(ped), 
            season_total = season_vehicle_visitors + season_bike + season_ped) %>%
  mutate(vehicle_mode_share = season_vehicle_visitors/season_total,
         bike_mode_share = season_bike/season_total,
         ped_mode_share = season_ped/season_total,
         vehicle_mode_share = ifelse(season_bike == 0 & season_ped == 0, 1, vehicle_mode_share)) %>%
  ungroup() %>%
  left_join(park_metadata)

##### annual #####
annual_volume <- all_modes_expanded %>%
  filter(year != 2022) %>%
  group_by(zone_name, year) %>%
  summarise(annual_vehicle_visitors = sum(vehicle_visitors), 
            annual_vehicles = sum(vehicles), 
            annual_bike = sum(bike), 
            annual_ped = sum(ped), 
            annual_total = annual_vehicle_visitors + annual_bike + annual_ped) %>%
  mutate(vehicle_mode_share = annual_vehicle_visitors/annual_total,
         bike_mode_share = annual_bike/annual_total,
         ped_mode_share = annual_ped/annual_total,
         vehicle_mode_share = ifelse(annual_bike == 0 & annual_ped == 0, 1, vehicle_mode_share)) %>%
  ungroup() %>%
  left_join(park_metadata)
  
  
##### save #####

## replace all NaNs with 0
is.nan.data.frame <- function(x){do.call(cbind, lapply(x, is.nan))}

weekly_volume[is.nan(weekly_volume)] <- 0
monthly_volume[is.nan(monthly_volume)] <- 0
season_volume[is.nan(season_volume)] <- 0
annual_volume[is.nan(annual_volume)] <- 0

## check that these are all equivalent
# weekly_volume %>%
#   group_by(system, year) %>%
#   summarise(total = sum(weekly_total)/1e6)
# 
# monthly_volume %>%
#   group_by(system, year) %>%
#   summarise(total = sum(monthly_total)/1e6)
# 
# annual_volume %>%
#   group_by(system, year) %>%
#   summarise(total = sum(annual_total)/1e6)

## and save
save(weekly_volume, monthly_volume, season_volume, annual_volume, 
     file = file.path(here(), "data-intermediate/processed/park-volume.rda"))
