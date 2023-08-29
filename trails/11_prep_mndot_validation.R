## This script cleans data from MnDOT's permanent bike & pedestrian counters (https://www.dot.state.mn.us/bike-ped-counting/index.html).
## This data is used for data validation and is saved in `data-intermediate`

mndot <- read_excel(file.path(here(), "data-raw/mndot/2014-2021_AllUserData_4Website.xlsx")) %>%
  mutate(date_day = ymd(date_day)) %>%
  filter(date_day >= ymd("2019-01-01")) 

## correct some errors in the mndot data
mndot_errors <- mndot %>%
  distinct(site, latitude, longitude, city, county) %>% # for some reason browns creek lat/long is populated a lot
  mutate(error = case_when(
    str_detect(site, "Red Wing") & latitude > 44.89 & latitude < 44.9 ~ "error",
    str_detect(site, "Hutchinson") & latitude > 44.8 ~ "error",
    str_detect(site, "Moorhead") & round(latitude, 5) == 46.84831 ~ "error", #this is an underpass
    str_detect(site, "Root River") & latitude > 43.72317 ~ "error",
    str_detect(site, "Rush Creek Trail") & round(longitude, 5) == -93.35984 ~ "error", #this is an underpass
    !str_detect(site, "Brown's") & city == "Stillwater" ~ "error",
    str_detect(site, "Luce") & latitude < 44.5 ~ "error", 
    TRUE ~ "ok"
  )) %>%
  filter(error == "error")

mndot_fix <- mndot %>%
  distinct(site, latitude, longitude, city) %>%
  filter(
    site %in% mndot_errors$site,
    city != "Stillwater"
  ) %>%
  # this is not exactly where the counter is, but this will allow us to map correctly
  mutate(latitude = ifelse(str_detect(site, "Luce"), 44.891029, latitude), 
         longitude = ifelse(str_detect(site, "Luce"), -94.360209, longitude)) %>%
  mutate(error = case_when(str_detect(site, "Red Wing") & latitude > 44.89 & latitude < 44.9 ~ "error",
                           str_detect(site, "Hutchinson") & longitude > -93 ~ "error",
                           str_detect(site, "Moorhead") & round(latitude, 5) == 46.84831 ~ "error", #this is an underpass
                           str_detect(site, "Root River") & latitude > 43.72317 ~ "error",
                           str_detect(site, "Rush Creek Trail") & round(longitude, 5) == -93.35984 ~ "error", #this is an underpass
                           TRUE ~ "ok")) %>%
  filter(error != "error") %>%
  dplyr::rename(
    latitude_fix = latitude,
    longitude_fix = longitude,
    city_fix = city
  ) %>%
  select(-error) %>%
  unique()

mndot_counters <- mndot %>%
  full_join(mndot_fix) %>%
  mutate(
    longitude = if_else(is.na(longitude_fix), longitude, longitude_fix),
    latitude = if_else(is.na(latitude_fix), latitude, latitude_fix),
    city = if_else(is.na(city_fix), city, city_fix)
  ) 

counter_coords <- mndot_counters %>%
  distinct(site, latitude, longitude) %>%
  group_by(site) %>%
  slice(1) %>% #hack to get the first occurance
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# get just a small section of the osm segments which overlap with trail counters
validation_osm_partial <- counter_coords %>%
  st_buffer(20) %>%
  st_transform(26915) %>%
  st_intersection(cleaned_trail_segments %>% st_transform(26915)) %>%
  unique() %>% # need to get unique segments
  rename(
    mndot_name = site
  ) %>%
  left_join(trail_segment_metadata) %>%
  group_by(mndot_name) %>%
  mutate(max = max(approx_length_meters)) %>%
  filter(max == approx_length_meters)

# and then join back in to get the full length of the osm segments which overlap with the trail counter locations
validation_osm_full <- validation_osm_partial %>% # in some instances, there are 2 trail segments nearby, so pick the longest one... william munger?
  st_drop_geometry() %>%
  # # because we used approximate geography above, force correct segment to be selected for luce line
  # bind_rows(data.frame(zone_name = c("Luce-Line_DNR_178701049"),
  #                      osm_id = c("178701049"),
  #                      mndot_name = c("D8 | Hutchinson | Luce Line Trail"))) %>%
  left_join(cleaned_trail_segments %>% select(osm_id), by = "osm_id") %>% # again, need to get full geometry of osm line segment
  st_as_sf()


##### run & fetch validation analyses #####
if (run_mndot_validation == TRUE|fetch_mndot_validation == TRUE) {
  
  # try(upload_zone_set(login_email, 
  #                 geom_type = "line",
  #                 zones = validation_osm_full %>%
  #                   mutate(is_pass = 1),
  #                 zone_set_name = "validate_trail_segments_fullOSM"
  # ))
  # 
  #######
  # Bike
  ######
  full_osm_data_bike <- try(run_stl_za(
    run = run_mndot_validation,
    fetch = fetch_mndot_validation,
    zone_set_name = "validate_trail_segments_fullOSM",
    analysis_prefix = paste0("trail-validate-july23"),
    date_sequence = month_sequence,
    date_sequence_label = "label",
    mode = "Bicycle",
    time_step = "Monthly"
  ))
  
  #######
  # Pedestrian
  ######
  
  full_osm_data_ped <- try(run_stl_za(
    run = run_mndot_validation,
    fetch = fetch_mndot_validation,
    zone_set_name = "validate_trail_segments_fullOSM",
    analysis_prefix = paste0("trail-validate-jun23"),
    date_sequence = month_sequence,
    date_sequence_label = "label",
    mode = "Pedestrian",
    time_step = "Monthly"
  ))
  
  
  ######
  # combine
  #####
  
  mndot_trails_monthly_za <- full_osm_data_bike %>%
    mutate(type = "full osm segment") %>%
    bind_rows(full_osm_data_ped %>% mutate(type = "full osm segment")) %>%
    left_join(month_sequence, by = "label")
}

save(mndot_trails_monthly_za, 
     file = file.path(here(), "data-temp/mndot_trails_monthly_za"))
  
### prep monthly mndot analyses for validation ###
stl_all <- mndot_trails_monthly_za %>%
  filter(year %in% c(2019, 2020, 2021)) %>%
  mutate(visitors = as.numeric(average_daily_zone_traffic_st_l_volume), 
         mode = ifelse(str_detect(mode_of_travel, "Bicycle"), "Bicyclist", "Pedestrian"), 
         day_type = str_remove_all(day_type, "1: Average |2: Average ")) %>%
  filter(
    zone_is_pass_through == "yes",
    str_detect(day_part, "All Day"),
    !str_detect(day_type, "All"), 
    type == "full osm segment"
  ) %>%
  group_by(zone_name, year, month, day_type) %>%
  summarise(LBS = sum(visitors)) %>%
  ungroup() %>%
  complete(zone_name, year, month, day_type)
  
stl_all_mode <- mndot_trails_monthly_za %>%
  filter(year %in% c(2019, 2020, 2021)) %>%
  mutate(visitors = as.numeric(average_daily_zone_traffic_st_l_volume), 
         mode = ifelse(str_detect(mode_of_travel, "Bicycle"), "Bicyclist", "Pedestrian"), 
         day_type = str_remove_all(day_type, "1: Average |2: Average ")) %>%
  filter(
    zone_is_pass_through == "yes",
    str_detect(day_part, "All Day"),
    !str_detect(day_type, "All"), 
    type == "full osm segment"
  ) %>%
  complete(zone_name, nesting(year, month), day_type, mode) %>%
  rename(LBS = visitors)

stl_missing <- stl_all %>%
  group_by(zone_name) %>%
  summarise(missing = sum(is.na(LBS)),
            total = n(),
            pct_missing = 100 * missing/n()) 

stl_remove <- stl_missing %>%
  filter(pct_missing >= 25)

stl_all <- stl_all %>%
  filter(!zone_name %in% stl_remove$zone_name) %>%
  replace_na(list(LBS = 0))

stl_all_mode <- stl_all_mode %>%
  filter(!zone_name %in% stl_remove$zone_name) %>%
  replace_na(list(LBS = 0))

### prep mndot counter data for comparison ###
mndot_all <- mndot %>%
  filter(mode != "All") %>%
  right_join(st_drop_geometry(validation_osm_full), # %>%
             by = c("site" = "mndot_name")
  ) %>%
  filter(
    date_day >= mdy("01/01/2019"),
    date_day <= mdy("12/31/2021")
  ) %>%
  select(date_day, site, date_day, weekend, total, mode,
         facility_type, on_off_road, install_year, device, technology) %>%
  mutate(
    year = year(date_day),
    month = month(date_day)
  ) %>% 
  group_by(site, weekend, year, month, date_day) %>%
  # get total of bike + ped
  summarise(total = sum(total)) %>%
  # get monthly average
  ungroup() %>%
  group_by(site, weekend, year, month) %>%
  summarise(
    MNDOTavg = mean(total),
    MNDOTse = sd(total) / sqrt(n()),
  ) %>%
  mutate(day_type = if_else(weekend == "weekend", "Weekend Day (Sa-Su)", "Weekday (M-F)")) %>%
  ungroup() 
  

mndot_all_mode <- mndot %>%
  filter(mode != "All") %>%
  right_join(st_drop_geometry(validation_osm_full), # %>%
             by = c("site" = "mndot_name")
  ) %>%
  filter(date_day >= mdy("01/01/2019"),
         date_day <= mdy("04/30/2022")
  ) %>%
  select(date_day, site, date_day, weekend, total, mode,
         facility_type, on_off_road, install_year, device, technology) %>%
  mutate(
    year = year(date_day),
    month = month(date_day)
  ) %>% # filter(str_detect(zone_name, "Brown"))
  group_by(site, weekend, year, month, mode) %>%
  summarise(
    MNDOTavg = mean(total),
    MNDOTse = sd(total) / sqrt(n()),
  ) %>%
  mutate(day_type = if_else(weekend == "weekend", "Weekend Day (Sa-Su)", "Weekday (M-F)")) %>%
  ungroup()

zone_names <- data.frame(site = c("Brown's Creek ST - Stillwater",
                                  "D3 | Brainerd | Paul Bunyan Trail",
                                  "D4 | Moorhead | TH75 Trail",
                                  "D6 | Lanesboro | Root River Trail",
                                  "Glacial Lakes ST - Spicer",
                                  "Heartland ST - Nevis",
                                  "Metro | Brooklyn Park | Rush Creek Trail",
                                  "Metro | Minneapolis | West River Greenway",
                                  "Sakatah Singing Hills ST - Morristown",
                                  "Willard Munger ST - Mission Creek", 
                                  "D8 | Hutchinson | Luce Line Trail"),
                         zone_name = c("Brown's-Creek_DNR_172936910", 
                                       "Paul-Bunyan_DNR_769764427", 
                                       "Moorhead-River-Corridor_GreaterMN_611668236", 
                                       "Root-River_DNR_175680583", 
                                       "Glacial-Lakes_DNR_174025703",
                                       "Heartland_DNR_173820752", 
                                       "Rush-Creek_Three-Rivers-Park-District_156338965",
                                       "Mississippi-Gorge-West-River-Parkway_Minneapolis-Park-and-Recreation-Board_722795493", 
                                       "Sakatah-Singing-Hills_DNR_18229873", 
                                       "Willard-Munger-Hinkley-Duluth-Fire-Segment_DNR_772271706", 
                                       "Luce-Line_DNR_178701049"), 
                         Site = c("Brown's Creek State Trail - Stillwater", 
                                  "Paul Bunyan State Trail - Brainerd",
                                  "TH75 Trail - Moorhead", 
                                  "Root River State Trail - Lanesboro", 
                                  "Glacial Lakes State Trail - Spicer", 
                                  "Heartland State Trail - Nevis", 
                                  "Rush Creek Regional Trail - Brooklyn Park", 
                                  "West River Greenway Regional Trail - Minneapolis", 
                                  "Sakatah Singing Hills State Trail - Morristown", 
                                  "Willard Munger State Trail - Mission Creek", 
                                  "Luce Line Regional Trail - Hutchinson"))

### combine ###
compare_all <- mndot_all %>%
  left_join(zone_names, by = "site") %>%
  full_join(stl_all) %>%
  mutate(LBS = if_else(is.na(LBS), 0, LBS)) %>% # if no coverage, fill in zero
  ungroup() %>%
  mutate(season = case_when(
    month %in% c(1:2, 12) ~ "winter",
    month %in% c(3:5) ~ "spring",
    month %in% c(6:8) ~ "summer",
    month %in% c(9:11) ~ "fall"
  )) %>%
  filter(!zone_name %in% stl_remove$zone_name, 
         !is.na(site), !is.na(zone_name), 
         !str_detect(site, "Moorhead"))

compare_all_mode <- mndot_all_mode %>%
  left_join(zone_names, by = "site") %>%
  full_join(stl_all_mode) %>%
  mutate(LBS = if_else(is.na(LBS), 0, LBS)) %>% # if no coverage, fill in zero
  ungroup() %>%
  mutate(season = case_when(
    month %in% c(1:2, 12) ~ "winter",
    month %in% c(3:5) ~ "spring",
    month %in% c(6:8) ~ "summer",
    month %in% c(9:11) ~ "fall"
  )) %>%
  filter(!zone_name %in% stl_remove$zone_name, 
         !is.na(site), !is.na(zone_name), 
         !str_detect(site, "Moorhead"))


### get % of missing/imputed records by source ###
pct_imputed_data <- mndot %>% 
  filter(site %in% mndot_all$site) %>%
  group_by(site) %>%
  summarise(mndot_pct_imputed = 100*sum(imputed)/n()) %>%
  left_join(zone_names) %>%
  left_join(stl_missing %>% select(zone_name, lbs_pct_missing = pct_missing)) %>%
  filter(!zone_name %in% stl_remove)


### create correlation table ###
cor_tab_dat <- compare_all_mode %>%
  group_by(year, mode) %>%
  summarise(cor = round(cor(LBS, MNDOTavg), 2)) %>%
  pivot_wider(names_from = "mode", values_from = "cor") %>%
  mutate(year = as.character(year))

total_mode_cor <- compare_all_mode %>%
  group_by(mode) %>%
  summarise(cor = round(cor(LBS, MNDOTavg), 2)) %>%
  pivot_wider(names_from = "mode", values_from = "cor") %>%
  mutate(year = "Total")

total_ann_cor <- compare_all_mode %>%
  group_by(year) %>%
  summarise(cor = round(cor(LBS, MNDOTavg), 2)) %>%
  pivot_wider(names_from = "year", values_from = "cor") 

total_cor <- round(cor(compare_all$LBS, compare_all$MNDOTavg), 2)

cor_tab <- rbind(cor_tab_dat, total_mode_cor) %>%
  select(Year = year, Bicyclist, Pedestrian) %>%
  as.data.frame() %>%
  ungroup() %>%
  mutate(`Total` = case_when(Year == "2019" ~ total_ann_cor$`2019`, 
                             Year == "2020" ~ total_ann_cor$`2020`, 
                             Year == "2021" ~ total_ann_cor$`2021`, 
                             TRUE ~ total_cor))


### save everything needed in Rmd for documentation ###
save(cor_tab, pct_imputed_data, counter_coords, compare_all, 
     compare_all_mode, zone_names, validation_osm_full, mndot, 
     file = file.path(here(), "data-intermediate/trails/mndot-validation-data.rda"))

