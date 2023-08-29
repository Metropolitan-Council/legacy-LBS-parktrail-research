# This script combines the partial zone sets created in the 02_ processing Rmds and prepares them for use with StreetLight.
# Polygons are read from `data-intermediate/parks` and saved to `data-intermediate`.

##### read polygons and perimeters #####

dnr <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/dnr_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)

dnr_perim <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/dnr_perim_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)

gmn <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/gmn_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)

gmn_perim <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/gmn_perim_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)

metro <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/metro_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)

metro_perim <- st_read(paste0(
  here(), "/data-intermediate/parks/shp/metro_perim_",
  park_geom_processing_date, ".shp"
)) %>%
  select(name = zone_name, geometry)


###### combine #####
zones <- bind_rows(dnr, gmn, metro)

perim_zones <- bind_rows(dnr_perim, gmn_perim, metro_perim) %>%
  mutate(is_pass = 1)

### check that perimeters all match polygons
# tedious, but a mismatch will cause problems with demographics/home locations later
# for(i in c(1:nrow(zones))){
#   park_name <- zones[i,]$name
#   img <- ggplot() +
#     geom_sf(data = zones %>% filter(name == park_name), fill = "darkgreen") +
#     geom_sf(data = perim_zones %>% filter(name == park_name), fill = "black", color = "black") +
#     theme_void() +
#     labs(title = zones[i,]$name)
#
#   ggsave(img, file = paste0(here(), "data-temp/perim-checks/", i, ".png"))
# }

##### upload to streetlight #####
upload_zone_set(
  login_email = login_email,
  zones = zones,
  zone_set_name = paste0("legacy_park_polygons_", park_geom_processing_date)
)

upload_zone_set(
  login_email = login_email,
  zones = perim_zones,
  zone_set_name = paste0("legacy_park_perimeters_", park_geom_processing_date)
)

upload_zone_set(
  login_email = login_email,
  zones = perim_zones %>% filter(str_detect(name, "_DNR")),
  zone_set_name = paste0("dnr_park_perimeters_", park_geom_processing_date)
)

upload_zone_set(
  login_email = login_email,
  zones = perim_zones %>% filter(str_detect(name, "_Greater-MN")),
  zone_set_name = paste0("gmn_park_perimeters_", park_geom_processing_date)
)

upload_zone_set(
  login_email = login_email,
  zones = perim_zones %>% filter(str_detect(name, "_Metro-Regional")),
  zone_set_name = paste0("metro_park_perimeters_", park_geom_processing_date)
)

##### add metadata & save as shp #####
park_metadata <- readRDS(file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))

zones_meta <- zones %>%
  rename(zone_id = name) %>%
  # rename columns to have <7 character names
  full_join(park_metadata %>%
    select(
      zone_id = zone_name,
      name = unit_label,
      type = unit_type_label,
      system = system_label,
      dist = district
    )) %>%
  arrange(system, dist, zone_id)

st_write(zones_meta, paste0(
  here(), "/data-processed/zone-sets/park-polygons/park-polygon-zone-set-",
  park_geom_processing_date, ".shp"
), append = FALSE)

perim_zones_meta <- perim_zones %>%
  rename(zone_id = name) %>%
  # rename columns to have <7 character names
  full_join(park_metadata %>%
    select(
      zone_id = zone_name,
      name = unit_label,
      type = unit_type_label,
      system = system_label,
      dist = district
    )) %>%
  arrange(system, dist, zone_id)

st_write(perim_zones_meta, paste0(
  here(), "/data-processed/zone-sets/park-perimeters/park-perim-zone-set-",
  park_geom_processing_date, ".shp"
), append = FALSE)
