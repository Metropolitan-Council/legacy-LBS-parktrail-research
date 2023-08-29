## This script generates some quick analyses and figures for use in the use cases storymap.

# Hourly use at custom zones

## read data from data-raw
### zones are drawn manually, meaning this analysis can't be fetched via the API

# FIX HOURLY PCTS
bc <- read_csv(file.path(here::here(), "data-raw/extra-stl/1414828_rim_battle_creek_hourly_amenities/1414828_rim_battle_creek_hourly_amenities_za_ped.csv")) %>%
  select(zone_name = `Zone Name`, intersection_type = `Intersection Type`, day_type = `Day Type`,
         day_part = `Day Part`, ped_use = `Average Daily Zone Traffic (StL Volume)`) %>%
  filter(!str_detect(day_part, "All")) %>%
  # clean up hourly day parts
  group_by(day_part) %>%
  mutate(hourm = str_remove_all(str_extract(day_part, "\\([A-Za-z0-9]+-"), "\\(|-"),
         ampm = ifelse(str_detect(hourm, "pm"), "pm", "am"),
         hour = as.numeric(str_remove_all(hourm, "[A-Za-z]")),
         hour = ifelse(ampm == "pm", hour + 12, hour),
         time = as.POSIXct(strptime(paste0(hour, ":00"), format = "%H:%M"), format = "%H:%M")) %>%
  # restrict 6am-10pm
  filter(hour %in% c(6:22)) %>%
  select(-c(hourm, ampm, hour)) %>%
  # combine pass-through & non-passthrough data
  group_by(zone_name) %>%
  mutate(zone_name = str_remove(zone_name, "-pt"),
         zone_name = ifelse(str_detect(zone_name, "rec-center"), "parking", zone_name)) %>%
  # get hourly use as %
  group_by(zone_name, time) %>%
  summarise(hourly_ped_use = sum(ped_use)) %>%
  ungroup() %>%
  group_by(zone_name) %>%
  mutate(hourly_pct = hourly_ped_use/sum(hourly_ped_use))


# Hartley Park/Duluth Traverse

## get Hartley Park polygon
hartley <- st_read(paste0(here::here(), "/data-raw/greater-mn/Designated/Designated.shp")) %>%
  mutate(valid = st_is_valid(geometry)) %>%
  filter(str_detect(park_name, "Hartley")) %>%
  st_transform(26915)

upload_zone_set(login_email = login_email,
                zones = hartley %>% rename(name = park_name),
                zone_set_name = "rim_hartley-use-case")

## and Duluth Traverse segment for mapping
hartley_bbox <- st_as_sf(st_as_sfc(st_bbox(hartley))) %>% st_buffer(350)

duluth_segments <- cleaned_trail_segments %>%
  filter(str_detect(zone_name, "Duluth-Traverse"),
         !osm_id %in% c(627622410, 627622404)) %>%
  st_transform(26915)

duluth_clip <- st_intersection(duluth_segments, hartley_bbox) %>%
  summarise(do_union = TRUE)

## read data from data-raw
### gates are drawn manually, meaning this analysis can't be fetched via the API
dt <- read_csv(file.path(here::here(), "data-raw/extra-stl/1414895_rim_hartley_dt_use_case/1414895_rim_hartley_dt_use_case_od_bike.csv")) %>%
  select(origin = `Origin Zone Name`, destination = `Destination Zone Name`, day_type = `Day Type`,
         day_part = `Day Part`, ped_use = `Average Daily O-D Traffic (StL Volume)`, origin_use = `Average Daily Origin Zone Traffic (StL Volume)`,
         destination_use = `Average Daily Destination Zone Traffic (StL Volume)`)

dt %>%
  mutate(entrance_use = 11,
         tag = case_when(
           origin == "entrance" & destination == "exit" ~ "through",
           origin == "entrance" & destination == "Hartley Park" ~ "stop"
         )) %>%
  filter(!is.na(tag)) %>%
  group_by(tag) %>%
  summarise(pct_from_entrance = ped_use/entrance_use) %>%
  bind_rows(data.frame(tag = "exit_elsewhere", pct_from_entrance = (1 - 0.273 - 0.182)))


## get Glacial Lakes polygon
temp_man <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/bdry_dnr_lrs_prk/gpkg_bdry_dnr_lrs_prk.zip", destfile = temp_man)
glacial <- read_sf(unzip(temp_man, "bdry_dnr_lrs_prk.gpkg"), layer = "dnr_management_units_prk") %>%
  st_transform(26915) %>%
  filter(str_detect(UNIT_NAME, "Glacial"))

## create grid
raw_grid <- st_make_grid(
  glacial,
  square = TRUE,
  cellsize = c(150, 150)
)

## intersect with park
grid <- st_intersection(raw_grid, glacial) %>%
  st_as_sf() %>%
  mutate(name = row_number()) %>%
  rename(geometry = x)

## upload & run
# upload_zone_set(login_email = login_email,
#                 zones = grid %>% mutate(is_pass = 1),
#                 zone_set_name = "rim_glacial-grid")
#
# create_streetlight_analysis(login_email = login_email,
#                             analysis_type = "Zone_Activity_Analysis",
#                             analysis_name = "rim_summer21_glacial-grid",
#                             travel_mode_type = "Pedestrian",
#                             output_type = "Volume",
#                             origin_zone_set = "rim_glacial-grid",
#                             day_types = "All Days|17",
#                             day_parts = "All Day|0023",
#                             date_ranges = list(start_date = "06/01/2021", end_date = "08/31/2021"),
#                             tags = list("streetlightR"))

## fetch
glacial_dat <- get_analysis_data(analysis_name = "rim_summer21_glacial-grid", metric = "za_ped")

## join to grid
grid_dat <- grid %>%
  left_join(glacial_dat, by = c("name" = "zone_name")) %>%
  mutate(activity = ifelse(is.na(average_daily_zone_traffic_st_l_volume),
                           0.01, as.numeric(average_daily_zone_traffic_st_l_volume))) %>%
  # calculate percents
  mutate(pct = activity/sum(activity)) %>%
  select(name, geometry, activity)


ggplot() +
  ggspatial::annotation_map_tile(
    type = "cartolight",
    zoom = 15
  ) +
  geom_sf(data = grid_dat, aes(fill = activity), alpha = 0.5) +
  theme_void() +
  scale_fill_viridis_c(trans = "log") +
  theme(legend.position = "none")

ggsave(file.path(here::here(), "figures/storymap/placeholder-glacial.png"), dpi = 300)


save(bc, dt, grid_dat, file = file.path(here::here(), "data-intermediate/documentation/use-case-fig-dat.rda"))
