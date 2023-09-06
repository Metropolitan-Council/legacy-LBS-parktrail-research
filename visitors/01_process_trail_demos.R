## This script reads the park demographics from `data-intermediate/visitors` and processes
## the data to align with surveyed demographics (for validation) and with park demographics
## (so that park & trail data can be combined). The results are saved in `data-intermediate`
## before being combined with park data.


##### setup #####
load(file.path(here(), "data-intermediate/visitors/raw-trail-demographics.rda"))


trail_stl_dem1 <- bind_rows(
  summer_trail_demographics %>% mutate(label = "summer21"),
  full_trail_demographics %>% mutate(label = "full21")
)

trail_stl_dem <- trail_stl_dem1 %>%
  ## combine & categories
  transmute(
    unit_id = unit_id,
    label = label,
    day_type = day_type,
    day_part = day_part,
    total_volume = use,
    white = white,
    black = black,
    american_indian = american_indian,
    asian = asian,
    pacific_islander = pacific_islander,
    other_race = other_race,
    multiple_races = multiple_races,
    hispanic = hispanic,
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
  ) %>%
  pivot_longer(
    names_to = "group",
    values_to = "percent",
    -c("unit_id", "label", "day_type", "day_part", "total_volume")
  ) %>%
  # get categories
  mutate(category = case_when(
    str_detect(group, "white|black|american_|asian|pacific_|other_|multiple_|hispanic") ~ "Race/ethnicity",
    str_detect(group, "income") ~ "Income",
    TRUE ~ "Education"
  )) %>%
  # recast race to relative percents
  group_by(label, day_type, day_part, unit_id, category) %>%
  mutate(
    totalp = sum(percent),
    percent = if_else(category == "Race/ethnicity",
      percent / totalp,
      percent
    )
  ) %>%
  # align names with survey/park names
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
  )) %>%
  select(-totalp) %>%
  ungroup() %>%
  group_by(unit_id, label, day_type, day_part, total_volume, percent) %>%
  # add margin of error & id information
  mutate(
    se = sqrt((percent * (1 - percent)) / total_volume),
    moe_95 = se * 1.96,
    moe_90 = se * 1.645
  )


save(trail_stl_dem,
  file = file.path(here(), "data-intermediate/visitors/trail_stl_dem.rda")
)
