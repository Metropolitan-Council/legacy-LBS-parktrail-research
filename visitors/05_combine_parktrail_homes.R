## This script combines all park and trail home locations (including surveyed data). Data is saved to
## `data-intermediate`; final shapefiles are saved to data-processed.


##### setup #####
load(file.path(here(), "data-intermediate/visitors/park-homes.rda"))
load(file.path(here(), "data-intermediate/visitors/trail-homes.rda"))
load(file.path(here::here(), "data-raw/intercept-survey/raven_home_stl_zips.rda"))

##### combine systems ####
home_bgs <- park_bgs %>%
  left_join(park_metadata) %>%
  bind_rows(
    trail_bgs %>%
      mutate(unit_id = zone_name) %>%
      left_join(trail_metadata)
  ) %>%
  filter(!is.na(unit_type))

home_zips <- park_zips %>%
  left_join(park_metadata) %>%
  bind_rows(
    trail_zips %>%
      mutate(unit_id = zone_name) %>%
      left_join(trail_metadata)
  ) %>%
  filter(!is.na(unit_type))

home_states <- park_states %>%
  left_join(park_metadata) %>%
  bind_rows(
    trail_states %>%
      mutate(unit_id = zone_name) %>%
      left_join(trail_metadata)
  ) %>%
  filter(!is.na(unit_type))


##### incorporate survey ######
park_to_zone <- data.frame(
  zone_name = c(
    "Battle-Creek_park_Metro-Regional_Ramsey-County",
    "Cleary-Lake_park_Metro-Regional_Three-Rivers",
    "Como-Zoo-and-Conservatory_park_Metro-Regional_Saint-Paul",
    "Hyland-Bush-Anderson-Lakes_park_Metro-Regional_Bloomington",
    "Lake-Elmo_park_Metro-Regional_Washington-County",
    "Lake-Minnewashta_park_Metro-Regional_Carver-County",
    "Lebanon-Hills_park_Metro-Regional_Dakota-County",
    "North-Mississippi_park_Metro-Regional_MPRB",
    "Lake-Minnetonka-LRT_trail_Metro-Regional_Three-Rivers",
    "Rice-Creek-West_trail_Metro-Regional_Anoka-County"
  ),
  unit_name = c(
    "Battle Creek", "Cleary Lake",
    "Como Zoo and Conservatory", "Hyland Bush Anderson Lakes",
    "Lake Elmo", "Lake Minnewashta", "Lebanon Hills", "North Mississippi",
    "Lake Minnetonka LRT", "Rice Creek West"
  ),
  PARK = c(
    "Battle Creek Regional Park",
    "Cleary Lake Regional Park (Scott County)",
    "Como Regional Zoo and Conservatory",
    "Bush and Normandale Lakes Regional Park",
    "Lake Elmo Park Reserve",
    "Lake Minnewashta Regional Park",
    "Lebanon Hills Regional Park",
    "North Mississippi Regional Park (MPRB)",
    "Lake Minnetonka LRT Regional Trail",
    "Rice Creek West Regional Trail (Anoka County)"
  )
)

oversample_survey_home <- raven_home_stl_zips %>%
  rename(zip_code = zip) %>%
  left_join(park_to_zone, by = "PARK") %>%
  filter(
    !is.na(zip_code),
    !is.na(unit_name)
  ) %>%
  mutate(source = "Survey") %>%
  select(zone_name, unit_name, zip_code, source, raw_percent = percent_visitors) %>%
  # convert to relative percents
  group_by(zone_name, unit_name) %>%
  mutate(percent = raw_percent / sum(raw_percent)) %>%
  select(-raw_percent)

oversample_stl_home <- home_zips %>%
  ungroup() %>%
  filter(
    label == "summer21",
    str_detect(day_type, "All"),
    str_detect(day_part, "All"),
    zone_name %in% park_to_zone$zone_name
  ) %>%
  left_join(park_to_zone, by = c("zone_name")) %>%
  mutate(source = "LBS") %>%
  select(zone_name, unit_name, zip_code, source, percent_by_zip)

oversample_home <- bind_rows(
  oversample_survey_home %>%
    rename(percent_by_zip = percent),
  oversample_stl_home
) %>%
  filter(percent_by_zip > 0)

oversample_home_long <- oversample_home %>%
  pivot_wider(
    names_from = "source",
    values_from = "percent_by_zip"
  ) %>%
  filter(!is.na(Survey), !is.na(LBS)) %>%
  pivot_longer(
    cols = c("LBS", "Survey"),
    names_to = "source",
    values_to = "percent"
  )

oversample_home_wide <- oversample_home %>%
  pivot_wider(
    names_from = "source",
    values_from = "percent_by_zip"
  ) %>%
  filter(!is.na(Survey), !is.na(LBS))


oversample_zip_count <- oversample_home %>%
  group_by(unit_name, source) %>%
  summarise(count = n_distinct(zip_code), .groups = "keep") %>%
  pivot_wider(names_from = "source", values_from = "count")

##### save everything together #####

save(home_bgs, home_zips, home_states,
  oversample_home_long, oversample_home_wide, oversample_zip_count,
  file = file.path(here(), "data-intermediate/processed/processed-homes.rda")
)

##### save shapefiles for upload #####
country_bgs <- purrr::map_dfr(.x = c(1:n_distinct(home_states$state_name)), .f = function(x) {
  tigris::block_groups(
    state = unique(home_states$state_name)[x],
    year = 2020, cb = TRUE
  )
})

park_bgs <- home_bgs %>%
  filter(
    unit_type == "park",
    str_detect(day_type, "All"),
    str_detect(day_part, "All"),
    label %in% c("year21", "full21")
  ) %>%
  arrange(system, district, unit_id) %>%
  select(shp_id, block_group_id, percent_by_bg) %>%
  transmute(
    shp_id = shp_id,
    percent_by_bg = percent_by_bg,
    bg_id = str_remove_all(block_group_id, "'")
  ) %>%
  pivot_wider(names_from = shp_id, values_from = percent_by_bg) %>%
  left_join(country_bgs %>% select(bg_id = GEOID, geometry)) %>%
  st_as_sf()

write_sf(park_bgs, file.path(here(), "data-processed/home-locations/park-home-block-groups/park-home-block-groups.shp"),
  append = FALSE
)

trail_bgs <- home_bgs %>%
  filter(
    unit_type == "trail",
    str_detect(day_type, "All"),
    str_detect(day_part, "All"),
    label %in% c("year21", "full21")
  ) %>%
  arrange(system, primary_district, unit_id) %>%
  select(shp_id, block_group_id, percent_by_bg) %>%
  transmute(
    shp_id = shp_id,
    percent_by_bg = percent_by_bg,
    bg_id = str_remove_all(block_group_id, "'")
  ) %>%
  pivot_wider(names_from = shp_id, values_from = percent_by_bg) %>%
  left_join(country_bgs %>% select(bg_id = GEOID, geometry)) %>%
  st_as_sf()

write_sf(trail_bgs, file.path(here(), "data-processed/home-locations/trail-home-block-groups/trail-home-block-groups.shp"),
  append = FALSE
)
