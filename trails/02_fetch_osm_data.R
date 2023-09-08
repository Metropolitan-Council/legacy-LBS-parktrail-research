# This script fetches OpenStreetMap (OSM) data corresponding to each trail in the project sample.
# The script uses the cleaned trails from `01_fetch_trail_geography.R` to create a bounding box
# for each trail. The `osmdata` package is used to fetch OSM line segments within that bounding box.
# OSM segments appropriate for biking & walking are identified; all line segments are saved to
# `data-intermediate` in case additional segments need to be added to complete a trail. Unique
# zone names are generated at this step (append osm_id to unit_id).

##### setup geography #####

## buffer trails
# this simplifies geographies when trails "double back"
# these buffered polygons will be used to identify line segments that intersect with a given trail

trail_polygons <- trail_lines %>%
  st_cast("MULTILINESTRING") %>%
  st_cast("LINESTRING") %>%
  # buffer, but don't expand ends of line segments
  st_buffer(15, min = "endCapStyle: FLAT") %>%
  group_by(unit_id) %>%
  summarise(geometry = st_union(geom))

saveRDS(trail_polygons, file.path(here(), "data-intermediate/trails/trail-polygons.RDS"))

## transform for use with osmdata package
trail_polygons <- st_transform(trail_polygons, 4326)


##### pull osm data #####

## setup for pulling data
assign("has_internet_via_proxy", TRUE, environment(curl::has_internet))

tictoc::tic()
osm_data_raw <- purrr::map(.x = c(1:nrow(trail_polygons)), .f = function(x) {
  # sleep to avoid overloding OSM API
  Sys.sleep(10)

  # select trail
  trail <- trail_polygons[x, ]

  # print status message
  cat(paste0("\n", x, ": ", trail$unit_id))

  # create bounding box
  trail_bbox <- trail %>%
    st_bbox()

  # pull data
  dat <- try(
    opq(trail_bbox, timeout = 100) %>%
      add_osm_feature(key = c("highway")) %>%
      osmdata_sf(),
    # print the error if it doesn't work
    silent = FALSE
  )

  # save unit_id
  dat$unit_id <- trail$unit_id

  # return
  dat
})
tictoc::toc()
beepr::beep(sound = 3)

## check length of results - any missed trails?
if (length(osm_data_raw) != nrow(trail_polygons)) {
  stop("Missing OSM data for some trails")
}

##### Extract lines from OSM data #####
osm_lines <- purrr::map_dfr(.x = c(1:length(osm_data_raw)), .f = function(x) {
  # set unit_id
  this_unit_id <- osm_data_raw[[x]]$unit_id

  # extract lines
  tryCatch(
    {
      osm_data_raw[x][[1]]$osm_lines %>%
        mutate(unit_id = this_unit_id) %>%
        select(unit_id, osm_id, name, highway, bicycle, geometry)
    },
    error = function(cond) {
      NULL
    }
  )
})

## check for trails with no lines
st_drop_geometry(trail_polygons) %>%
  filter(!unit_id %in% osm_lines$unit_id)

##### filter to only overlapping bike/ped lines #####
bike_ped_osm_partial <- osm_lines %>%
  mutate(
    keep = case_when(
      highway == "cycleway" ~ "keep",
      highway == "path" ~ "keep",
      highway == "track" ~ "keep",
      highway == "footway" ~ "keep",
      bicycle == "designated" &
        !highway %in% c("trunk", "unclassified") ~ "keep",
      TRUE ~ "remove"
    )
  ) %>%
  filter(keep == "keep") %>%
  rename(osm_unit_id = unit_id) %>%
  st_transform(26915) %>%
  # confirm lines overlap with a trail
  st_intersection(
    trail_polygons %>% st_transform(26915) %>% rename(trail_unit_id = unit_id)
  ) %>%
  # because bounding boxes are large, confirm that osm & trail unit_ids agree
  filter(osm_unit_id == trail_unit_id) %>%
  st_cast("LINESTRING") %>%
  mutate(approx_length = st_length(.)) %>%
  # drop all segments <50m
  filter(approx_length > set_units(50, "m")) %>%
  rename(unit_id = osm_unit_id) %>%
  select(-trail_unit_id)

## join to full extent of osm line
bike_ped_osm <- osm_lines %>%
  right_join(st_drop_geometry(bike_ped_osm_partial)) %>%
  mutate(zone_name = paste0(unit_id, "_", osm_id)) %>%
  select(-c(keep, bicycle))

##### save #####
save(osm_lines, bike_ped_osm,
  file = file.path(here(), "data-intermediate/trails/osm-data.rda")
)
