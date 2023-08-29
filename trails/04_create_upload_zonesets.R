# This script applies the edits to OSM segments from the `03_` Rmds.
# Cleaned OSM segments are uploaded to StreetLight for analyes and
# saved to `data-intermediate` as .RDS object and to `data-processed`
# as shapefiles to be shared publicly. Additional polygon zones
# are created by buffering the original trail shapefiles by 30 meters.
# Polygon zones are used for home location analyses. These zones are also
# saved in `data-intermediate` and `data-processed.`

##### load edits #####
load(file.path(here(), "data-intermediate/trails/dnr-geography-edits.rda"))
load(file.path(here(), "data-intermediate/trails/gmn-geography-edits.rda"))
load(file.path(here(), "data-intermediate/trails/metro-geography-edits.rda"))

##### pull new osm segments #####

### DNR ###
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

# make sure nothing has been duplicated
dnr_new_to_pull <- unique(dnr_new_to_pull)
new_dnr_segments_geog <- purrr::map_dfr(.x = c(1:nrow(dnr_new_to_pull)), .f = function(x) {
  pull <- opq_osm_id(
    id = dnr_new_to_pull[x, ]$osm_id,
    type = "way",
    open_url = FALSE
  ) %>%
    osmdata_sf()

  print(x)

  Sys.sleep(3)

  lines <- pull$osm_lines %>%
    mutate(
      unit_id = dnr_new_to_pull[x, ]$unit_id,
      osm_id = as.character(dnr_new_to_pull[x, ]$osm_id),
      zone_name = paste0(unit_id, "_", osm_id)
    )
})

new_dnr_segments_geog <- new_dnr_segments_geog %>%
  select(unit_id, osm_id, zone_name, geometry)

### Greater MN ###
# make sure nothing has been duplicated
gmn_new_to_pull <- unique(gmn_new_to_pull)
new_gmn_segments_geog <- purrr::map_dfr(.x = c(1:nrow(gmn_new_to_pull)), .f = function(x) {
  pull <- opq_osm_id(
    id = gmn_new_to_pull[x, ]$osm_id,
    type = "way",
    open_url = FALSE
  ) %>%
    osmdata_sf()

  print(x)

  Sys.sleep(3)

  if (str_detect(gmn_new_to_pull[x, ]$unit_id, "Cook-County-Mountain-Bike")) {
    pull$osm_polygons %>%
      st_cast("LINESTRING") %>%
      mutate(
        unit_id = gmn_new_to_pull[x, ]$unit_id,
        osm_id = as.character(osm_id)
      ) %>%
      select(osm_id, unit_id, geometry)
  } else {
    pull$osm_lines %>%
      mutate(
        osm_id = gmn_new_to_pull[x, ]$osm_id,
        unit_id = gmn_new_to_pull[x, ]$unit_id,
        osm_id = as.character(osm_id)
      ) %>%
      select(osm_id, unit_id, geometry)
  }
})

new_gmn_segments_geog <- new_gmn_segments_geog %>%
  select(unit_id, osm_id, geometry) %>%
  mutate(zone_name = paste0(unit_id, "_", osm_id))

##### clean (add/remove) segments #####
# drop all geography to work with just osm_ids & zone_names

### DNR ###
dnr_segments_to_add <- dnr_osm_edits %>%
  filter(action == "add") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(
    !is.na(unit_id),
    osm_id %in% osm_lines$osm_id
  )

dnr_segments_to_remove <- dnr_osm_edits %>%
  filter(action == "remove") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(!is.na(unit_id))

new_dnr_segments <- unique(st_drop_geometry(new_dnr_segments_geog))

dnr_osm <- bike_ped_osm %>%
  filter(str_detect(unit_id, "DNR")) %>%
  st_transform(4326)

dnr_cleaned_segments <-
  # start with initially identified segments
  dnr_osm %>%
  # just work with osm_ids/zone_names, add geometry back in later
  st_drop_geometry() %>%
  select(unit_id, osm_id, zone_name) %>%
  # append manually added segments
  bind_rows(dnr_segments_to_add) %>%
  # append newly pulled segments
  bind_rows(new_dnr_segments) %>%
  # remove manually removed zones
  filter(!zone_name %in% dnr_segments_to_remove$zone_name) %>%
  # correct zones that were added twice
  unique()

## check for unique zones
dnr_check_unique <- dnr_cleaned_segments %>%
  group_by(zone_name) %>%
  summarise(count = n(), .groups = "keep") %>%
  filter(count != 1)

if (nrow(dnr_check_unique) > 0) {
  stop("DNR zone names are not unique")
}

## rejoin to geography
dnr_cleaned_segments <- dnr_cleaned_segments %>%
  left_join(osm_lines %>%
    filter(osm_id %in% dnr_cleaned_segments$osm_id) %>%
    select(osm_id, geometry) %>%
    bind_rows(new_dnr_segments_geog %>% select(osm_id, geometry)) %>%
    unique()) %>%
  st_as_sf()

cat("Found", nrow(dnr_cleaned_segments), "DNR segments")

# confirm nothing wonky with zone names - this should return nothing
dnr_cleaned_segments %>%
  mutate(test = paste0(unit_id, "_", osm_id)) %>%
  filter(test != zone_name)

### Greater MN ###
gmn_segments_to_add <- gmn_osm_edits %>%
  filter(action == "add") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(
    !is.na(unit_id),
    osm_id %in% osm_lines$osm_id
  )

gmn_segments_to_remove <- gmn_osm_edits %>%
  filter(action == "remove") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(!is.na(unit_id))

new_gmn_segments <- unique(st_drop_geometry(new_gmn_segments_geog))

gmn_osm <- bike_ped_osm %>%
  filter(str_detect(unit_id, "Greater-MN")) %>%
  st_transform(4326)

gmn_cleaned_segments <-
  # start with initially identified segments
  gmn_osm %>%
  # just work with osm_ids/zone_names, add geometry back in later
  st_drop_geometry() %>%
  select(unit_id, osm_id, zone_name) %>%
  # append manually added segments
  bind_rows(gmn_segments_to_add) %>%
  # append newly pulled segments
  bind_rows(new_gmn_segments) %>%
  # remove manually removed zones
  filter(!zone_name %in% gmn_segments_to_remove$zone_name) %>%
  # correct zones that were added twice
  unique()

## check for unique zones
gmn_check_unique <- gmn_cleaned_segments %>%
  group_by(zone_name) %>%
  summarise(count = n(), .groups = "keep") %>%
  filter(count != 1)

if (nrow(gmn_check_unique) > 0) {
  stop("gmn zone names are not unique")
}

## rejoin to geography
gmn_cleaned_segments <- gmn_cleaned_segments %>%
  left_join(osm_lines %>%
    filter(osm_id %in% gmn_cleaned_segments$osm_id) %>%
    select(osm_id, geometry) %>%
    bind_rows(new_gmn_segments_geog %>% select(osm_id, geometry)) %>%
    unique()) %>%
  st_as_sf()

cat("Found", nrow(gmn_cleaned_segments), "Greater MN segments")

# confirm nothing wonky with zone names - this should return nothing
gmn_cleaned_segments %>%
  mutate(test = paste0(unit_id, "_", osm_id)) %>%
  filter(test != zone_name)

### Metro Regional ###
metro_segments_to_add <- metro_osm_edits %>%
  filter(action == "add") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(
    !is.na(unit_id),
    osm_id %in% osm_lines$osm_id
  )

metro_segments_to_remove <- metro_osm_edits %>%
  filter(action == "remove") %>%
  select(unit_id, osm_id, zone_name) %>%
  filter(!is.na(unit_id))

metro_osm <- bike_ped_osm %>%
  filter(str_detect(unit_id, "Metro-Regional")) %>%
  st_transform(4326)

metro_cleaned_segments <-
  # start with initially identified segments
  metro_osm %>%
  # just work with osm_ids/zone_names, add geometry back in later
  st_drop_geometry() %>%
  select(unit_id, osm_id, zone_name) %>%
  # append manually added segments
  bind_rows(metro_segments_to_add) %>%
  # remove manually removed zones
  filter(!zone_name %in% metro_segments_to_remove$zone_name) %>%
  # correct zones that were added twice
  unique()

## check for unique zones
metro_check_unique <- metro_cleaned_segments %>%
  group_by(zone_name) %>%
  summarise(count = n(), .groups = "keep") %>%
  filter(count != 1)

if (nrow(metro_check_unique) > 0) {
  stop("Metro zone names are not unique")
}

## rejoin to geography
metro_cleaned_segments <- metro_cleaned_segments %>%
  left_join(osm_lines %>%
    filter(osm_id %in% metro_cleaned_segments$osm_id) %>%
    select(osm_id, geometry) %>%
    unique()) %>%
  st_as_sf()

cat("Found", nrow(metro_cleaned_segments), "Metro segments")

# confirm nothing wonky with zone names - this should return nothing
metro_cleaned_segments %>%
  mutate(test = paste0(unit_id, "_", osm_id)) %>%
  filter(test != zone_name)


##### upload zone sets #####
trail_zone_suffix <- "2023.07.11"

### DNR ###

## segments
dnr_zones <- dnr_cleaned_segments %>%
  transmute(
    name = zone_name,
    is_pass = 1,
    geometry = geometry
  )

upload_zone_set(login_email,
  geom_type = "line",
  zones = dnr_zones,
  zone_set_name = paste0("segments_dnr_", trail_zone_suffix)
)

## buffers (for home locations)
# start with 30 meter buffer and decrease as needed to keep zone under 4km^2
dnr_buffers <- trail_lines %>%
  filter(str_detect(unit_id, "DNR")) %>%
  # discard any trails that were dropped
  filter(unit_id %in% dnr_cleaned_segments$unit_id) %>%
  # buffer
  st_buffer(30) %>%
  # check area & reduce buffer as needed
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -15, 0)) %>% # down to 15
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 10
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 5
  mutate(area_m2 = st_area(geom)) %>%
  filter(as.numeric(area_m2) <= 4e6)

dnr_trail_area_test <- dnr_buffers %>% filter(as.numeric(area_m2) > 4e6)
if (nrow(dnr_trail_area_test) > 0) {
  warning("At least 1 DNR trail area zone is greater than 4km^2")
}

dnr_buffer_zones <- dnr_buffers %>%
  transmute(
    name = unit_id,
    is_pass = 1
  ) %>%
  rename(geometry = geom)

upload_zone_set(login_email,
  zones = dnr_buffer_zones,
  zone_set_name = paste0("buffers_dnr_", trail_zone_suffix)
)

### Greater MN ###
## segments
gmn_zones <- gmn_cleaned_segments %>%
  filter(!str_detect(unit_id, "Zumbro")) %>%
  transmute(
    name = zone_name,
    is_pass = 1,
    geometry = geometry
  )

upload_zone_set(login_email,
  geom_type = "line",
  zones = gmn_zones,
  zone_set_name = paste0("segments_greatermn_", trail_zone_suffix)
)

## buffers (for home locations)
# start with 30 meter buffer and decrease as needed to keep zone under 4km^2
gmn_buffers <- trail_lines %>%
  filter(str_detect(unit_id, "Greater-MN")) %>%
  filter(!str_detect(unit_id, "Zumbro")) %>%
  # discard any trails that were dropped
  filter(unit_id %in% gmn_cleaned_segments$unit_id) %>%
  # buffer
  st_buffer(30) %>%
  # check area & reduce buffer as needed
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -15, 0)) %>% # down to 15
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 10
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 5
  mutate(area_m2 = st_area(geom)) %>%
  filter(as.numeric(area_m2) <= 4e6) # only 2 remaining "trails" are actually minimally developed corridors

gmn_trail_area_test <- gmn_buffers %>% filter(as.numeric(area_m2) > 4e6)
if (nrow(gmn_trail_area_test) > 0) {
  warning("At least 1 gmn trail area zone is greater than 4km^2")
}

gmn_buffer_zones <- gmn_buffers %>%
  transmute(
    name = unit_id,
    is_pass = 1
  ) %>%
  rename(geometry = geom)

upload_zone_set(login_email,
  zones = gmn_buffer_zones,
  zone_set_name = paste0("buffers_gmn_", trail_zone_suffix)
)

### Metro ###

## segments
metro_zones <- metro_cleaned_segments %>%
  transmute(
    name = zone_name,
    is_pass = 1,
    geometry = geometry
  )

upload_zone_set(login_email,
  geom_type = "line",
  zones = metro_zones,
  zone_set_name = paste0("segments_metro_", trail_zone_suffix)
)

## buffers (for home locations)
# start with 30 meter buffer and decrease as needed to keep zone under 4km^2
metro_buffers <- trail_lines %>%
  filter(str_detect(unit_id, "Metro-Regional")) %>%
  # discard any trails that were dropped
  filter(unit_id %in% metro_cleaned_segments$unit_id) %>%
  # buffer
  st_buffer(30) %>%
  # check area & reduce buffer as needed
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -15, 0)) %>% # down to 15
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 10
  mutate(area_m2 = st_area(geom)) %>%
  st_buffer(., ifelse(as.numeric(.$area_m2) > 4e6, -5, 0)) %>% # down to 5
  mutate(area_m2 = st_area(geom)) %>%
  filter(as.numeric(area_m2) <= 4e6)

metro_trail_area_test <- metro_buffers %>% filter(as.numeric(area_m2) > 4e6)
if (nrow(metro_trail_area_test) > 0) {
  warning("At least 1 metro trail area zone is greater than 4km^2")
}

metro_buffer_zones <- metro_buffers %>%
  transmute(
    name = unit_id,
    is_pass = 1
  ) %>%
  rename(geometry = geom)

upload_zone_set(login_email,
  zones = metro_buffer_zones,
  zone_set_name = paste0("buffers_metro_", trail_zone_suffix)
)


##### save geographies #####
cleaned_trail_segments <- bind_rows(dnr_cleaned_segments, gmn_cleaned_segments, metro_cleaned_segments)
trail_buffer_zones <- bind_rows(dnr_buffers, gmn_buffers, metro_buffers)

save(cleaned_trail_segments, trail_buffer_zones,
  file = file.path(here(), "data-intermediate/trails/cleaned-trail-geography.rda")
)

##### save zoneset shapefiles #####
load(file.path(here(), "data-intermediate/trails/trail-metadata.rda"))


dnr_zones %>%
  select(zone_id = name, geometry) %>%
  left_join(trail_segment_metadata %>%
    select(
      zone_id = zone_name,
      unit_id = unit_id,
      osm_id = osm_id,
      name = unit_label,
      type = unit_type_label,
      system = system_label
    )) %>%
  st_write(file.path(here(), "data-processed/zone-sets/dnr-trail-segments/dnr-trail-segments-2023.07.011.shp"),
    append = FALSE
  )

gmn_zones %>%
  filter(!str_detect(name, "Zumbro")) %>%
  select(zone_id = name, geometry) %>%
  left_join(trail_segment_metadata %>%
    select(
      zone_id = zone_name,
      unit_id = unit_id,
      osm_id = osm_id,
      name = unit_label,
      type = unit_type_label,
      system = system_label
    )) %>%
  st_write(file.path(here(), "data-processed/zone-sets/greatermn-trail-segments/greatermn-trail-segments-2023.07.011.shp"),
    append = FALSE
  )

metro_zones %>%
  select(zone_id = name, geometry) %>%
  left_join(trail_segment_metadata %>%
    select(
      zone_id = zone_name,
      unit_id = unit_id,
      osm_id = osm_id,
      name = unit_label,
      type = unit_type_label,
      system = system_label
    )) %>%
  st_write(file.path(here(), "data-processed/zone-sets/metro-trail-segments/metro-trail-segments-2023.07.011.shp"),
    append = FALSE
  )

buffer_zones <- bind_rows(dnr_buffer_zones, gmn_buffer_zones, metro_buffer_zones) %>%
  filter(!str_detect(name, "Zumbro")) %>%
  select(unit_id = name, geometry) %>%
  left_join(trail_metadata %>%
    select(
      unit_id = unit_id,
      name = unit_label,
      type = unit_type_label,
      system = system_label,
      dist = primary_district
    )) %>%
  st_write(file.path(here(), "data-processed/zone-sets/trail-polygons/trail-polygons-2023.07.011.shp"),
    append = FALSE
  )
