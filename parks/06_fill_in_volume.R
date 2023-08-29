# This script takes weekly, biweekly, and monthly vehicle data from `05_run_park_stl.R`. Intrapark trips are removed. 
# Biweekly and monthly records are used to fill in missing weekly estimates. If >50% of weekly records are missing for
# a given park, that park is removed from the project sample. This script saves partially filled vehicle 
# volume to `data-intermediate/parks`.

##### load data #####
load(file.path(here(), "data-intermediate/parks/raw-week-volume.rda"))
load(file.path(here(), "data-intermediate/parks/raw-biweekly-volume.rda"))
load(file.path(here(), "data-intermediate/parks/raw-monthly-volume.rda"))
load(file.path(here(), "data-intermediate/parks/raw-od-volume.rda"))

##### basic processing #####

weekly_results1 <- week_vehicle %>%
  left_join(week_sequence, by = "label") %>%
  filter(str_detect(day_part, "All"), 
         str_detect(day_type, "All"), 
         intersection_type == "Trip End") %>%
  mutate(raw_volume = as.numeric(average_daily_zone_traffic_st_l_volume), # raw_volume = numbers from StL, no edits
         source = "Weekly StreetLight",
         start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  select(zone_name, start_date, end_date, label, week_of_year, year,
         day_type, day_part, source, raw_volume)

biweekly_results <- biweekly_vehicle %>%
  left_join(biweekly_sequence, by = "label") %>%
  filter(str_detect(day_part, "All"), 
         str_detect(day_type, "All"), 
         intersection_type == "Trip End") %>%
  mutate(raw_volume = as.numeric(average_daily_zone_traffic_st_l_volume), # raw_volume = numbers from StL, no edits
         source = "Biweekly StreetLight",
         start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  select(zone_name, start_date, end_date, label, week_of_year, year,
         day_type, day_part, source, raw_volume)

monthly_results <- month_veh %>% 
  left_join(month_sequence, by = "label") %>%
  filter(str_detect(day_part, "All"), 
         str_detect(day_type, "All"), 
         intersection_type == "Trip End") %>%
  mutate(raw_volume = as.numeric(average_daily_zone_traffic_st_l_volume), # raw_volume = numbers from StL, no edits
         source = "Monthly StreetLight",
         start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  select(zone_name, start_date, end_date, label, month, year,
         day_type, day_part, source, raw_volume)

##### remove intrapark trips by zone & week #####
# working under the assumption that if volume is NA, intrapark trips = 0

intrapark <- week_od_veh %>%
  left_join(week_sequence, by = "label") %>%
  filter(str_detect(day_part, "All"), 
         str_detect(day_type, "All"), 
         origin_zone_name == destination_zone_name) %>%
  mutate(daily_intrapark_trips = as.numeric(average_daily_o_d_traffic_st_l_volume)) %>%
  select(destination_zone_name, label, daily_intrapark_trips) %>%
  rename(zone_name = destination_zone_name)

weekly_results2 <- weekly_results1 %>%
  left_join(intrapark, by = c("zone_name", "label")) %>%
  mutate(od_removed = ifelse(!is.na(daily_intrapark_trips), TRUE, FALSE),
         removed_daily_intrapark_trips = replace_na(daily_intrapark_trips, 0),
         volume = raw_volume - removed_daily_intrapark_trips) # remove intrapark trips right away

##### identify missing records #####

# expand weekly data 
missing_weeks <- weekly_results2 %>%
  select(zone_name, start_date, end_date, label, week_of_year, year, volume) %>%
  # nesting means that only existing combinations of those variables will be used
  # ensures we don't end up with something like start_date = january 1, 2019, year = 2021
  complete(zone_name, nesting(start_date, end_date, label, week_of_year, year)) %>% 
  filter(is.na(volume)) %>%
  select(-volume)

cat(paste0("\n\n\nVEHICLE SUMMARY: initially missing ", nrow(missing_weeks), " weekly records at ", length(unique(missing_weeks$zone_name)), " zones (",
           prettyNum(nrow(missing_weeks)/(nrow(week_sequence) * 211), digits = 2), "% of expected records)"))  

# identify units with >50% missing data
pct_missing_weekly_records <- weekly_results2 %>%
  select(zone_name, start_date, end_date, label, week_of_year, year, volume) %>%
  # nesting means that only existing combinations of those variables will be used
  # ensures we don't end up with something like start_date = january 1, 2019, year = 2021
  complete(zone_name, nesting(start_date, end_date, label, week_of_year, year)) %>%
  group_by(zone_name) %>%
  summarise(pct_missing = sum(is.na(volume))/n()) %>%
  arrange(-pct_missing)

# parks dropped for coverage issues; use this to update metadata later
parks_dropped_coverage <- pct_missing_weekly_records %>%
  filter(pct_missing > 0.5) %>%
  left_join(park_metadata)

save(parks_dropped_coverage, file = file.path(here(), "data-intermediate/parks/parks-dropped-coverage.rda"))

# then remove those from metadata

park_metadata <- park_metadata %>%
  filter(!zone_name %in% parks_dropped_coverage$zone_name)

saveRDS(park_metadata, file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))

##### biweekly data #####
# can any missing weeks be replaced by biweekly results?

fillin_biweekly <- missing_weeks %>%
  left_join(biweekly_results %>% select(-end_date), 
            by = c("zone_name", "year", "week_of_year", "start_date", "label")) %>%
  rename(biweekly_volume = raw_volume) %>%
  select(zone_name, start_date, end_date, label, week_of_year, source, biweekly_volume)

cat(paste0("\n\nVEHICLE SUMMARY: biweekly data fills in ", prettyNum(100*nrow(fillin_biweekly %>% filter(!is.na(biweekly_volume)))/nrow(missing_weeks), digits = 3), "% of missing records"))

# add missing values
filled_weekly_results1 <- plyr::rbind.fill(weekly_results2, fillin_biweekly) %>%
  mutate(volume = coalesce(volume, biweekly_volume)) %>%
  select(zone_name, start_date, end_date, label, week_of_year, year, source, od_removed, removed_daily_intrapark_trips, volume, biweekly_volume)

filled_weekly_results1 %>%
  group_by(source) %>%
  summarise(count = n())

still_missing <- filled_weekly_results1 %>%
  filter(is.na(volume))

cat(paste("\n\nVEHICLE SUMMARY: Still missing", nrow(still_missing), "records at", length(unique(still_missing$zone_name)), "zones after biweekly data"))

##### monthly data ####
# repeat process above with monthly data


# probably excessive, but this addresses weeks that span 2 months 
still_missing_monthly <- still_missing %>%
  mutate(month1 = month(start_date),
         month2 = month(end_date))

still_missing_monthly <- still_missing_monthly %>%
  rowwise() %>%
  mutate(all_dates = list(seq(start_date, end_date, by = "day")),
         all_months = list(month(unlist(all_dates))),
         days_in_month1 = sum(unlist(all_months) == month1),
         days_in_month2 = sum(unlist(all_months) == month2),
         days_in_month2 = ifelse(month1 == month2, 0, days_in_month2), 
         year = year(start_date)) %>%
  select(-all_dates, -all_months)

fillin_monthly <- still_missing_monthly %>%
  left_join(monthly_results %>%
              select(zone_name, year, source, month1 = month, month1_volume = raw_volume), by = c("zone_name", "year", "month1")) %>%
  left_join(monthly_results %>%
              select(zone_name, year, month2 = month, month2_volume = raw_volume), by = c("zone_name", "year", "month2")) %>%
  # get volume across whole week, if either months are NA let this be NA and impute
  group_by(zone_name, label) %>%
  mutate(total_week_volume = days_in_month1 * month1_volume + days_in_month2 * month2_volume,
         monthly_volume = round(total_week_volume/7)) %>% # get "average daily" to match other sources
  ungroup() %>%
  mutate(source = coalesce(source.x, source.y)) %>%
  select(zone_name, start_date, end_date, label, week_of_year, year, source, monthly_volume)
  
cat(paste0("\n\nVEHICLE SUMMARY: monthly data fills in ", prettyNum(100* nrow(fillin_monthly %>% filter(!is.na(monthly_volume)))/nrow(still_missing_monthly), digits = 3), "% of remaining records"))

filled_weekly_results <- plyr::rbind.fill(filled_weekly_results1, fillin_monthly) %>% 
  mutate(volume = coalesce(volume, biweekly_volume, monthly_volume)) %>%
  select(zone_name, start_date, end_date, label, week_of_year, year, source, 
         removed_daily_intrapark_trips, volume, biweekly_volume, monthly_volume) %>%
  # correct any introduced NAs
  group_by(zone_name, start_date, end_date) %>%
  fill(everything(), .direction = "downup") %>%
  ungroup() %>%
  unique() %>%
  mutate(source = ifelse(is.na(volume), NA, source),
         removed_daily_intrapark_trips = replace_na(removed_daily_intrapark_trips, 0))
  
## check sources

# filled_weekly_results %>%
#   filter(is.na(volume) & is.na(biweekly_volume) & is.na(monthly_volume)) %>%
#   select(source) %>% unique() # should be only NA
# 
# filled_weekly_results %>%
#   filter(!is.na(volume) & is.na(biweekly_volume) & is.na(monthly_volume)) %>%
#   select(source) %>% unique() # should be only weekly
# 
# filled_weekly_results %>%
#   filter(!is.na(volume) & !is.na(biweekly_volume) & is.na(monthly_volume)) %>%
#   select(source) %>% unique() # should be only biweekly
# 
# filled_weekly_results %>%
#   filter(!is.na(volume) & is.na(biweekly_volume) & !is.na(monthly_volume)) %>%
#   select(source) %>% unique() # should be only monthly
# 
veh_records_by_source <- filled_weekly_results %>%
  group_by(source) %>%
  summarise(count = n())

save(veh_records_by_source, 
     file = file.path(here(), "data-intermediate/parks/veh-records-by-source.rda"))

## exclude parks with missing records
filled_weekly_results <- filled_weekly_results %>%
  filter(!zone_name %in% parks_dropped_coverage$zone_name)

cat(paste0("\n\nVEHICLE SUMMARY: Still missing ", prettyNum(nrow(filled_weekly_results %>% filter(is.na(volume)))), " records at ",
           filled_weekly_results %>% filter(is.na(volume)) %>% select(zone_name) %>% unique() %>% nrow(), " zones"))

filled_weekly_results %>%
  filter(is.na(volume)) %>%
  group_by(zone_name) %>%
  summarise(still_missing = n()) %>%
  arrange(-still_missing)

## check that all zones have the same number of records
filled_weekly_results %>%
  group_by(zone_name) %>%
  summarise(count = n()) %>%
  arrange(-count) 

## save
save(filled_weekly_results, 
     file = file.path(here(), "data-intermediate/parks/partially-filled-volume.rda"))

## update metadata to remove dropped zones
park_metadata <- park_metadata %>%
  filter(!zone_name %in% parks_dropped_coverage$zone_name)

saveRDS(park_metadata, file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))

