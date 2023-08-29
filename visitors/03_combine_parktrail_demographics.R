## This script simply combines the park demographics processed in `02_process_trail_demos` 
## and `02_process_trail_demos`. This data, along with surveyed demographics, is saved to 
## `data-intermediate/processed`.

##### setup #####
load(file.path(here(), "data-intermediate/demographics/trail-stl-dem.rda"))
load(file.path(here(), "data-intermediate/demographics/park-stl-dem.rda"))
load(file.path(here(), "data-intermediate/demographics/survey-demos.rda"))
load(file.path(here(), "data-intermediate/trails/trail-metadata.rda"))
park_metadata <- readRDS(file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))

##### combine parks, trails, & survey data #####

### parks & trails ###
stl_dem <- park_stl_dem %>%
  left_join(park_metadata) %>%
  bind_rows(
    trail_stl_dem %>%
      left_join(trail_metadata)
  ) %>%
  filter(!is.na(unit_type)) %>%
  # fill trail zone names with unit ids
  mutate(zone_name = ifelse(unit_type == "trail", 
                            unit_id, 
                            zone_name)) %>%
  # make education & income ordered factors
  mutate(group = factor(group, 
                        levels = c("American Indian", "Asian", "Black", 
                                   "Hispanic or Latinx", "More than one race", 
                                   "Native Hawaiian and other Pacific Islander", 
                                   "Some other race", "White", 
                                   "High school", "Associate degree or some college", 
                                   "4-year degree", "Graduate or professional degree", 
                                   "Less than $25,000", "$25,000 - 39,999", 
                                   "$40,000 - 59,999", "$60,000 - 74/79,999", 
                                   "$75/80,000 - 99,999", "$100,000 - 149,999", 
                                   "$150,000 or higher"))) %>%
  filter(!is.na(percent))

all_metadata <- bind_rows(park_metadata, trail_metadata %>% mutate(zone_name = unit_id))
  

### prep to combine with survey ####
unit_to_zone_name <- data.frame(unit = c("Battle Creek",
                                         "Cleary Lake",
                                         "Como Zoo and Conservatory",
                                         "Hyland Bush Anderson Lakes",
                                         "Lake Elmo",
                                         "Lake Minnetonka LRT",
                                         "Lake Minnewashta",
                                         "Lebanon Hills",
                                         "North Mississippi",
                                         "Rice Creek West"),
                                zone_name = c("Battle-Creek_park_Metro-Regional_Ramsey-County",
                                              "Cleary-Lake_park_Metro-Regional_Three-Rivers",
                                              "Como-Zoo-and-Conservatory_park_Metro-Regional_Saint-Paul",
                                              "Hyland-Bush-Anderson-Lakes_park_Metro-Regional_Bloomington",
                                              "Lake-Elmo_park_Metro-Regional_Washington-County",
                                              "Lake-Minnetonka-LRT_trail_Metro-Regional_Three-Rivers",
                                              "Lake-Minnewashta_park_Metro-Regional_Carver-County",
                                              "Lebanon-Hills_park_Metro-Regional_Dakota-County",
                                              "North-Mississippi_park_Metro-Regional_MPRB",
                                              "Rice-Creek-West_trail_Metro-Regional_Anoka-County"), 
                                unit_id = c("Battle-Creek_park_Metro-Regional_Ramsey-County", 
                                            "Cleary-Lake_park_Metro-Regional_Three-Rivers", 
                                            "Como-Zoo-and-Conservatory_park_Metro-Regional_Saint-Paul", 
                                            "Hyland-Bush-Anderson-Lakes_park_Metro-Regional_Bloomington", 
                                            "Lake-Elmo_park_Metro-Regional_Washington-County", 
                                            "Lake-Minnetonka-LRT_trail_Metro-Regional_Three-Rivers", 
                                            "Lake-Minnewashta_park_Metro-Regional_Carver-County", 
                                            "Lebanon-Hills_park_Metro-Regional_Dakota-County", 
                                            "North-Mississippi_park_Metro-Regional_MPRB", 
                                            "Rice-Creek-West_trail_Metro-Regional_Anoka-County"))



### combine ###
combo_stl_surv <- stl_dem %>%
  filter(label == "summer21", 
         str_detect(day_type, "All"), 
         str_detect(day_part, "All")) %>%
  mutate(source = "LBS") %>%
  filter(zone_name %in% unit_to_zone_name$zone_name) %>%
  ungroup() %>%
  full_join(unit_to_zone_name) %>%
  full_join(survey_dem %>% 
              inner_join(unit_to_zone_name, by = "unit") %>%
              mutate(label = "summer21", 
                     day_type = "0: All Days (M-Su)", 
                     day_part = "0: All Day (12am-12am)")) %>%
  filter(group != "NA", !is.na(category)) %>%
  unique()

demo_stats <- combo_stl_surv %>%
  filter(!is.na(source)) %>%
  select(source, unit, category, group, percent, se, moe_90, moe_95) %>%
  pivot_wider(names_from = source, values_from = c(percent, se, moe_90, moe_95)) %>%
  filter(!is.na(percent_survey), !is.na(percent_LBS)) %>%
  mutate(
    z = (percent_survey - percent_LBS) /
      sqrt((se_survey^2) + (se_LBS^2)),
    sig = case_when( # abs(z) > 2.575 ~ "99% CI",
      # abs(z) > 1.96 ~ "95% CI",
      # abs(z) > 1.645 ~  "90% CI",
      z > 1.96 ~ "survey % is sig. higher than streetlight %",
      z < -1.96 ~ "streetlight % is sig. higher than survey %",
      TRUE ~ "ns"
    ),
    abbrsig = case_when(
      abs(z) > 3.291 ~ "***",
      abs(z) > 2.575 ~ "**",
      abs(z) > 1.96 ~ "*",
      abs(z) > 1.645 ~ ".",
      TRUE ~ "ns"
    )
  ) %>%
  select(unit, category, group, sig, abbrsig)

demo_stats %>%
  group_by(sig) %>%
  summarise(count = n())

both <- combo_stl_surv %>%
  inner_join(demo_stats) 

save(stl_dem, survey_dem, both, 
     file = file.path(here(), "data-intermediate/processed/all-processed-demographics.rda"))
