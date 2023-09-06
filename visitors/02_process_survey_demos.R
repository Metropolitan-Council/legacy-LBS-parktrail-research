## This script processes demographics from the Met Council's 2021 Visitor Study. Demographics
## were processed by Ellen Esch before being saved in the `data-raw` folder of this repository.
## This script processes the data to be aligned with LBS estimates.
## Data is saved to `data-intermediate` before being combined with LBS data.

load(paste0(here::here(), "/data-raw/intercept-survey/demos_raven.rda"))

survey_dem <- demos_raven %>%
  # rename to match StL
  rename(group = category, category = variable) %>%
  filter(!group %in% c("12-24", "25-44", "45-64", "65+", "NA", "Mean household income")) %>%
  # combine/summarise categories to match StL designations
  mutate(group = case_when(
    group %in% c("$60,000 - 74,999", "$60,000 - 79,999") ~ "$60,000 - 74/79,999",
    group %in% c("$75,000 - 99,999", "$80,000 - 99,999") ~ "$75/80,000 - 99,999",
    group == "Other BIPOC" ~ "Some other race",
    str_detect(group, "Indian") ~ "American Indian",
    str_detect(group, "Asian") ~ "Asian",
    str_detect(group, "Black") ~ "Black",
    str_detect(group, "Hispanic") ~ "Hispanic or Latinx",
    str_detect(group, "Middle Eastern") ~ "White",
    str_detect(group, "Multiple") ~ "More than one race",
    str_detect(group, "Hawaiian") ~ "Native Hawaiian and other Pacific Islander",
    str_detect(group, "BIPOC") ~ "Some other race",
    str_detect(group, "Associate degree") ~ "Associate degree or some college",
    str_detect(group, "High school") ~ "High school",
    TRUE ~ group
  )) %>%
  # recalculate margin of error with summed categories
  group_by(system, category, group) %>%
  summarise(
    percent = sum(Percent / 100),
    totalpersons = sum(Persons),
    se = sqrt((percent * (1 - percent)) / totalpersons),
    moe_95 = se * 1.96,
    moe_90 = se * 1.645,
    .groups = "keep"
  ) %>%
  ungroup() %>%
  # set up for comparison with StL
  mutate(source = "survey") %>%
  separate(system, "Regional Park", into = c("system", "other")) %>%
  separate(system, "Regional Trail", into = c("system", "other")) %>%
  select(-other) %>%
  transmute(
    unit = case_when(
      str_detect(system, "Bush") ~ "Hyland Bush Anderson Lakes",
      str_detect(system, "Como") ~ "Como Zoo and Conservatory",
      str_detect(system, "Rice Creek") ~ "Rice Creek West",
      TRUE ~ str_remove(system, " Park Reserve")
    ),
    unit = trimws(unit),
    use = totalpersons,
    category = case_when(
      category == "income" ~ "Income",
      category == "education" ~ "Education",
      TRUE ~ "Race/ethnicity"
    ),
    group = group,
    se = se,
    moe_95 = moe_95,
    moe_90 = moe_90,
    source = source,
    percent = percent
  )


save(survey_dem,
  file = file.path(here(), "data-intermediate/visitors/survey-demos.rda")
)
