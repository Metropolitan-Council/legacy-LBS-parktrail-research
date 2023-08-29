## This script reads the park home locations from `data-intermediate/parks` and processes
## the data to align with surveyed home locations (for validation) and with trail home locations
## (so that park & trail data can be combined). The results are saved in `data-intermediate` 
## before being combined with trail data. 


##### setup #####
load(file.path(here(), "data-intermediate/parks/raw-home-locations.rda"))

##### combine geographies across systems #####

bgs1 <- bind_rows(dnr_home_locations$bgs, 
                 gmn_home_locations$bgs, 
                 metro_home_locations$bgs)

zips1 <- bind_rows(dnr_home_locations$zips, 
                 gmn_home_locations$zips, 
                 metro_home_locations$zips)

states1 <- bind_rows(dnr_home_locations$states, 
                 gmn_home_locations$states, 
                 metro_home_locations$states)


##### convert to number by home location #####

bgs <- bgs1 %>%
  # get vehicle multipliers
  left_join(park_metadata %>% select(zone_name, vehicle_multiplier), 
             by = "zone_name") %>%
  mutate(volume = as.numeric(average_daily_zone_traffic_st_l_volume), 
         mode = case_when(
           str_detect(mode_of_travel, "Vehicles") ~ "Vehicle", 
           str_detect(mode_of_travel, "Bicycle") ~ "Bicycle", 
           TRUE ~ "Pedestrian"),
         volume = ifelse(mode == "Vehicle", volume * vehicle_multiplier, volume), 
         num_by_home_location = as.numeric(percent_by_home_location) * volume
  ) %>%
  select(-c(mode_of_travel, home_and_work_filter, intersection_type, zone_id, 
            average_daily_zone_traffic_st_l_volume)) %>%
  pivot_wider(names_from = mode, values_from = num_by_home_location) %>%
  replace_na(list(Vehicle = 0, Bicycle = 0, Pedestrian = 0)) %>%
  mutate(total_by_home_location = Vehicle + Bicycle + Pedestrian) %>%
  filter(total_by_home_location != 0) %>%
  # and convert back to percentages
  group_by(label, day_type, day_part, zone_name, block_group_id) %>%
  summarise(total_by_bg = sum(total_by_home_location)) %>%
  ungroup() %>%
  group_by(label, day_type, day_part, zone_name) %>%
  mutate(zone_total = sum(total_by_bg), 
         percent_by_bg = total_by_bg/zone_total) %>%
  ungroup() 

## confirm percentages add up correctly 
bgs %>%
  group_by(label, day_type, day_part, zone_name) %>%
  summarise(total = sum(percent_by_bg))
  
  