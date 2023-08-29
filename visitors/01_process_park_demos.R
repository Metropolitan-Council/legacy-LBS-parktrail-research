## This script reads the park demographics from `data-intermediate/parks` and processes
## the data to align with surveyed demographics (for validation) and with trail demographics
## (so that park & trail data can be combined). The results are saved in `data-intermediate`
## before being combined with trail data.


##### read raw data #####

load(file.path(here(), "data-intermediate/parks/raw-home-locations.rda"))

### combine systems within modes ###

equity1 <- bind_rows(
  dnr_home_locations$equity,
  gmn_home_locations$equity,
  metro_home_locations$equity
)

edu_inc1 <- bind_rows(
  dnr_home_locations$education_income,
  gmn_home_locations$education_income,
  metro_home_locations$education_income
)


### equity ####
veh_equity <- equity1 %>%
  filter(str_detect(mode_of_travel, "Vehicles")) %>%
  mutate(
    unit_id = zone_name,
    volume = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  left_join(park_metadata %>% select(zone_name, unit_id, vehicle_multiplier)) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume,
    non_hispanic, foreign_born, non_foreign_born, speak_english_less_than_very_well,
    with_a_disability, without_a_disability
  )) %>%
  ## convert to relative percentages (so categories including hispanic sum to 100%)
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "unit_id", "zone_name", "day_part",
    "day_type", "label"
  ))), as.numeric) %>%
  rowwise() %>%
  # get new denominator
  mutate(totalp = white + black + american_indian + asian + pacific_islander +
    other_race + multiple_races + hispanic) %>%
  # and re-calculate percentages
  mutate_at(vars(c(
    white, black, american_indian, asian, pacific_islander,
    other_race, multiple_races, hispanic
  )), ~ . / totalp) %>%
  ## convert recast percentages to numbers
  # apply vehicle multiplier
  mutate(vehicle_visitors = volume * vehicle_multiplier) %>%
  select(-volume, -totalp) %>%
  # and calculate percentages
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "vehicle_multiplier", "vehicle_visitors"
  ))), ~ . * vehicle_visitors)

bike_equity <- equity1 %>%
  filter(str_detect(mode_of_travel, "Bicycle")) %>%
  mutate(
    unit_id = zone_name,
    bike = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  left_join(park_metadata %>% select(zone_name, unit_id, vehicle_multiplier)) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume,
    non_hispanic, foreign_born, non_foreign_born, speak_english_less_than_very_well,
    with_a_disability, without_a_disability
  )) %>%
  ## convert to relative percentages (so categories including hispanic sum to 100%)
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "unit_id", "zone_name", "day_part",
    "day_type", "label"
  ))), as.numeric) %>%
  rowwise() %>%
  # get new denominator
  mutate(totalp = white + black + american_indian + asian + pacific_islander +
    other_race + multiple_races + hispanic) %>%
  # and re-calculate percentages
  mutate_at(vars(c(
    white, black, american_indian, asian, pacific_islander,
    other_race, multiple_races, hispanic
  )), ~ . / totalp) %>%
  ## convert recast percentages to numbers
  # and calculate percentages
  select(-totalp) %>%
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "bike"
  ))), ~ . * bike)

ped_equity <- equity1 %>%
  filter(str_detect(mode_of_travel, "Pedestrian")) %>%
  mutate(
    unit_id = zone_name,
    ped = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  left_join(park_metadata %>% select(zone_name, unit_id, vehicle_multiplier)) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume,
    non_hispanic, foreign_born, non_foreign_born, speak_english_less_than_very_well,
    with_a_disability, without_a_disability
  )) %>%
  ## convert to relative percentages (so categories including hispanic sum to 100%)
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "unit_id", "zone_name", "day_part",
    "day_type", "label"
  ))), as.numeric) %>%
  rowwise() %>%
  # get new denominator
  mutate(totalp = white + black + american_indian + asian + pacific_islander +
    other_race + multiple_races + hispanic) %>%
  # and re-calculate percentages
  mutate_at(vars(c(
    white, black, american_indian, asian, pacific_islander,
    other_race, multiple_races, hispanic
  )), ~ . / totalp) %>%
  ## convert recast percentages to numbers
  # and calculate percentages
  select(-totalp) %>%
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "ped"
  ))), ~ . * ped)

equity <- bind_rows(veh_equity, bike_equity, ped_equity) %>%
  select(-c(vehicle_multiplier, mode_of_travel)) %>%
  # get totals across mode
  complete(nesting(zone_name, unit_id), label, day_type, day_part) %>%
  replace(is.na(.), 0) %>%
  group_by(zone_name, unit_id, label, day_type, day_part) %>%
  # get total volume (denominator)
  mutate(total_volume = vehicle_visitors + bike + ped) %>%
  # sum categories across modes
  summarise_at(vars(-one_of("vehicle_visitors", "bike", "ped")), sum) %>%
  # convert back to percentages
  mutate_at(vars(-one_of(
    "zone_name", "unit_id", "label", "day_type",
    "day_part", "total_volume"
  )), ~ . / total_volume) %>%
  # remove NaNs caused by 0/0
  filter(!is.nan(white))

##### education & income #####
veh_edu_inc <- edu_inc1 %>%
  filter(str_detect(mode_of_travel, "Vehicles")) %>%
  mutate(
    unit_id = zone_name,
    volume = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  left_join(park_metadata %>% select(zone_name, unit_id, vehicle_multiplier)) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume
  )) %>%
  # convert all percentages to numeric
  mutate_at(vars(-one_of(c("mode_of_travel", "unit_id", "zone_name", "day_part", "day_type", "label"))), as.numeric) %>%
  rowwise() %>%
  # apply vehicle multiplier
  mutate(vehicle_visitors = volume * vehicle_multiplier) %>%
  select(-volume) %>%
  # convert percentages to numbers
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "vehicle_multiplier", "vehicle_visitors"
  ))), ~ . * vehicle_visitors)


bike_edu_inc <- edu_inc1 %>%
  filter(str_detect(mode_of_travel, "Bicycle")) %>%
  mutate(
    unit_id = zone_name,
    bike = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume
  )) %>%
  # convert all percentages to numeric
  mutate_at(vars(-one_of(c("mode_of_travel", "unit_id", "zone_name", "day_part", "day_type", "label"))), as.numeric) %>%
  rowwise() %>%
  # multiply all percentages by total bike volume
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "bike"
  ))), ~ . * bike)


ped_edu_inc <- edu_inc1 %>%
  filter(str_detect(mode_of_travel, "Pedestrian")) %>%
  mutate(
    unit_id = zone_name,
    ped = as.numeric(average_daily_zone_traffic_st_l_volume)
  ) %>%
  select(-c(
    zone_id, data_periods, home_and_work_filter, intersection_type, zone_is_pass_through,
    zone_direction_degrees, zone_is_bi_direction, average_daily_zone_traffic_st_l_volume
  )) %>%
  # convert all percentages to numeric
  mutate_at(vars(-one_of(c("mode_of_travel", "unit_id", "zone_name", "day_part", "day_type", "label"))), as.numeric) %>%
  rowwise() %>%
  # multiply all percentages by total ped volume
  mutate_at(vars(-one_of(c(
    "mode_of_travel", "zone_name", "day_type", "day_part",
    "label", "unit_id", "ped"
  ))), ~ . * ped)

edu_inc <- bind_rows(veh_edu_inc, bike_edu_inc, ped_edu_inc) %>%
  select(-c(vehicle_multiplier, mode_of_travel)) %>%
  # get totals across mode
  complete(nesting(zone_name, unit_id), label, day_type, day_part) %>%
  replace(is.na(.), 0) %>%
  group_by(zone_name, unit_id, label, day_type, day_part) %>%
  # get total volume (denominator)
  mutate(total_volume = vehicle_visitors + bike + ped) %>%
  # sum categories across modes
  summarise_at(vars(-one_of("vehicle_visitors", "bike", "ped")), sum) %>%
  # convert back to percentages
  mutate_at(vars(-one_of(
    "zone_name", "unit_id", "label", "day_type",
    "day_part", "total_volume"
  )), ~ . / total_volume) %>%
  # remove NaNs caused by 0/0
  filter(!is.nan(income_less_than_10k)) %>%
  # combine categories for later analyses
  transmute(
    zone_name = zone_name,
    unit_id = unit_id,
    label = label,
    day_type = day_type,
    day_part = day_part,
    total_volume = total_volume,
    income_less_than_25k = income_less_than_10k + income_10k_to_15k + income_15k_to_20k + income_20k_to_25k,
    income_25k_to_40k = income_25k_to_30k + income_30k_to_35k + income_35k_to_40k,
    income_40k_to_60k = income_40k_to_45k + income_45k_to_50k + income_50k_to_60k,
    income_60k_to_75k = income_60k_to_75k,
    income_75k_to_100k = income_75k_to_100k,
    income_100k_to_150k = income_100k_to_125k + income_125k_to_150k,
    income_more_than_150k = income_150k_to_200k + income_more_than_200k,
    hs_edu = high_school_graduate + x9th_to_12th_grade_no_diploma + less_than_9th_grade,
    associates_lt = associates_degree + some_college_no_degree,
    bachelors_degree = bachelors_degree,
    graduate_or_professional_degree = graduate_or_professional_degree
  )


##### combine
stl_dem <- equity %>%
  filter(!is.na(label)) %>%
  full_join(edu_inc %>% filter(!is.na(label))) %>%
  filter(str_detect(day_part, "All")) %>%
  pivot_longer(
    names_to = "group",
    values_to = "percent",
    -c(
      zone_name, unit_id, label,
      day_type, day_part, total_volume
    )
  ) %>%
  mutate(category = case_when(
    str_detect(group, "white|black|ameri|asia|multiple|pacific|other_race|hispanic") ~ "Race/ethnicity",
    str_detect(group, "income") ~ "Income",
    TRUE ~ "Education"
  )) %>%
  rename(use = total_volume) %>%
  mutate(
    category = case_when(
      str_detect(group, "white|black|ameri|asia|multiple|pacific|other_race|hispanic") ~ "Race/ethnicity",
      str_detect(group, "income") ~ "Income",
      TRUE ~ "Education"
    ),
    se = sqrt((percent * (1 - percent)) / use), # moe / 1.645,
    moe_95 = se * 1.96,
    moe_90 = se * 1.645
  ) %>%
  filter(!is.na(group))

park_stl_dem <- stl_dem %>%
  mutate(group = case_when(
    group == "white" ~ "White",
    group == "black" ~ "Black",
    group == "american_indian" ~ "American Indian",
    group == "asian" ~ "Asian",
    group == "pacific_islander" ~ "Native Hawaiian and other Pacific Islander",
    group == "other_race" ~ "Some other race",
    group == "multiple_races" ~ "More than one race",
    group == "hispanic" ~ "Hispanic or Latinx",
    group == "income_less_than_25k" ~ "Less than $25,000",
    group == "income_25k_to_40k" ~ "$25,000 - 39,999",
    group == "income_40k_to_60k" ~ "$40,000 - 59,999",
    group == "income_60k_to_75k" ~ "$60,000 - 74/79,999",
    group == "income_75k_to_100k" ~ "$75/80,000 - 99,999",
    group == "income_100k_to_150k" ~ "$100,000 - 149,999",
    group == "income_more_than_150k" ~ "$150,000 or higher",
    group == "hs_edu" ~ "High school",
    group == "associates_lt" ~ "Associate degree or some college",
    group == "bachelors_degree" ~ "4-year degree",
    TRUE ~ "Graduate or professional degree"
  ))


## save
save(park_stl_dem,
  file = file.path(here(), "data-intermediate/visitors/park-stl-dem.rda")
)
