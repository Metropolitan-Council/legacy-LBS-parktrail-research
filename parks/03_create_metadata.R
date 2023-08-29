# This script uses the processed geographies in `data-intermediate` to create more complete metadata for each park.
# This step is performed before uploading zones to StreetLight or running any analyses so that final checks of zone names
# can be performed. Metadata is saved in `data-intermediate.`

##### basic metadata from raw geographies #####

load(file.path(here(), "data-intermediate/parks/raw-dnr-geography.rda"))
dnr <- do.call(bind_rows, dnr) %>%
  # exclude unit dropped due to area
  filter(!str_detect(zone_name, "Sam-Brown-Memorial"))
dnr_meta1 <- dnr %>%
  st_drop_geometry() %>%
  mutate(
    system = "DNR",
    system_label = "DNR State"
  ) %>%
  filter(!str_detect(zone_name, "Sam-Brown-Memorial"))

load(file.path(here(), "data-intermediate/parks/raw-gmn-geography.rda"))
gmn <- do.call(bind_rows, gmn)
gmn_meta1 <- gmn %>%
  st_drop_geometry() %>%
  mutate(
    system = "Greater MN",
    system_label = "Greater MN Regional"
  )

load(file.path(here(), "data-intermediate/parks/raw-metro-geography.rda"))
metro <- do.call(bind_rows, metro)
metro_meta1 <- metro %>%
  st_drop_geometry() %>%
  mutate(
    system = "Metro",
    system_label = "Metro Regional"
  )

##### combine basic metadata #####
basic_metadata1 <- dnr_meta1 %>%
  st_drop_geometry() %>%
  select(-c(UNIT_NAME, UNIT_TYPE, PGRM_PROJE, GIS_ACRES, LEGISLATIV, Shape_Leng, Shape_Area)) %>%
  bind_rows(
    st_drop_geometry(gmn_meta1) %>% select(-park_name)
  ) %>%
  bind_rows(
    st_drop_geometry(metro_meta1) %>% select(-c(NAME, AGENCY, STATUS, CATEGORY))
  )

##### duplicate metadata for 4 inclusive metro polygons #####
inclusive_metadata <- basic_metadata1 %>%
  filter(str_detect(zone_name, "Minneapolis-Chain|Minnehaha|Nokomis|Phalen")) %>%
  mutate(
    zone_name = str_replace_all(zone_name, "Metro-Regional", "Metro-Regional-Inclusive"),
    system = "Metro Regional Inclusive",
    system_label = "Metro Regional Inclusive",
    extra_id = "Inclusive Boundary",
    long_unit_label = paste0(unit_label, " ", unit_type_label, ", ", short_agency, " (Inclusive Boundary)"),
    long_agency = paste(long_agency, "(Inclusive)"),
    short_agency = paste(short_agency, "(Inclusive)"),
    unit_label = paste0(unit_label, ", ", short_agency),
    primary_county = ifelse(str_detect(zone_name, "Phalen"), "Ramsey", "Hennepin")
  )

##### add inclusive polygons
basic_metadata <- bind_rows(basic_metadata1, inclusive_metadata)

##### fill in all labels
# zone_name: unique id used for analysis

# unit_id: unique id used to identify UNIT - for parks, this is the same as zone_name. The differentiation between zone_name & unit_id is necessary for trails.

# system: indicates "DNR", "Greater MN", or "Metro"

# system_label: indicates "DNR State", "Greater MN Regional", or "Metro Regional"

# extra_id: indicates any "extra" info needed to identify park ("State Wayside" vs "State Park", for example)

# unit_type: always "park"

# unit_type_label: more descriptive than unit_type ("Regional Park" or "Regional Special Recreation Feature", for example)

# unit_label: short name (with spaces), used for most plot labels
# metro parks include agency
# eligible greater minnesota parks are noted
# state waysides and recreation areas are called out

# long_unit_label: longer name (with spaces), used for fact sheet titles
# unit_label plus " Regional Park" or " State Park"

# long_agency: only for metro parks, indicates implementing agency

# short_agency: only for metro parks, shortened agency name for plot labels, etc

labels_metadata <- basic_metadata %>%
  # create long unit labels
  mutate(long_unit_label = case_when(
    system == "Metro" ~ paste0(unit_label, " ", unit_type_label, ", ", short_agency),
    extra_id == "Eligible" ~ paste0(unit_label, " ", unit_type_label, " (Eligible)"),
    system == "Metro Regional Inclusive" ~ long_unit_label,
    unit_label == "Grand Marais Recreation Area (Eligible)" ~ "Grand Marais Regional Recreation Area (Eligible)",
    TRUE ~ paste(unit_label, unit_type_label)
  )) %>%
  # add extra info to unit labels
  mutate(unit_label = case_when(
    system == "Metro" ~ paste0(unit_label, ", ", short_agency),
    extra_id == "Eligible" ~ paste0(unit_label, " (Eligible)"),
    extra_id == "State-Recreation-Area" ~ paste0(unit_label, " (State Recreation Area)"),
    extra_id == "State-Wayside" ~ paste0(unit_label, " (State Wayside)"),
    TRUE ~ unit_label
  )) %>%
  # DNR and Greater MN each have a Lac qui Parle park, make this clear
  mutate(extra_id = case_when(
    zone_name == "Lac-qui-Parle_park_Greater-MN" ~ "Greater MN",
    zone_name == "Lac-qui-Parle_park_DNR" ~ "DNR",
    # and set short_agency to be extra_id for metro parks
    system == "Metro" ~ short_agency,
    TRUE ~ extra_id
  )) %>%
  select(-original_name)

##### add spatial metadata #####

### primary county ###
# get just zone name & geometry
all_parks <- dnr %>%
  select(zone_name, geom) %>%
  rbind(gmn %>%
    select(zone_name, geom = geometry)) %>%
  rbind(metro %>%
    select(zone_name, geom))

counties <- counties(state = "MN") %>%
  select(county = NAME) %>%
  st_transform(26915)

park_county <- counties %>%
  st_intersection(all_parks) %>%
  mutate(total_area_in_county = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  ungroup() %>%
  group_by(zone_name) %>%
  mutate(max = max(total_area_in_county)) %>%
  filter(total_area_in_county == max) %>%
  select(zone_name, primary_county = county) %>%
  rbind(
    inclusive_metadata %>% select(zone_name, primary_county)
  )

### DNR area ###
temp_dd <- tempfile()
download.file(url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/bdry_dnr_parks_and_trails_admin/gpkg_bdry_dnr_parks_and_trails_admin.zip", destfile = temp_dd)
dnr_areas <- read_sf(unzip(temp_dd, "bdry_dnr_parks_and_trails_admin.gpkg"), layer = "dnr_parks_and_trails_areas") %>%
  st_transform(26915)

dnr_area_county <- dnr_areas %>%
  select(ALTD_NAME) %>%
  st_intersection(counties) %>%
  # as above, select the largest region in case of slight overlaps
  mutate(total_area_in_dnr_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(county) %>%
  mutate(max = max(total_area_in_dnr_area)) %>%
  filter(total_area_in_dnr_area == max) %>%
  select(county, dnr_area = ALTD_NAME) %>%
  mutate(dnr_area = paste("DNR Area", dnr_area))

## check there is only 1 area per county
dnr_area_check <- dnr_area_county %>%
  group_by(county) %>%
  summarise(count = n()) %>%
  filter(count > 1)

if (nrow(dnr_area_check > 0)) {
  stop("Issue with DNR area assignments")
}

### Greater MN district ###
gmn_dist_county <- counties %>%
  # st_drop_geometry() %>%
  mutate(gmn_district = case_when(
    # district 2 has to go first to avoid "Lake County" vs "Red Lake County" issue
    str_detect(county, "Itasca|Crow Wing|Cass|Hubbard|Beltrami|Lake of the Woods|Roseau|Kittson|Marshall|Polk|Red Lake|Pennington|Norman|Mahnomen|Clearwater") == TRUE ~ "Greater MN District 2",
    str_detect(county, "Koochiching|St. Louis|Lake|Cook|Carlton|Pine|Aitkin|Kanabec|Mille Lacs") == TRUE ~ "Greater MN District 1",
    str_detect(county, "Clay|Becker|Wilkin|Otter Tail|Wadena|Traverse|Grant|Douglas|Todd|Big Stone|Stevens|Pope|Swift|Kandiyohi|Chippewa|Lac qui Parle|Yellow Medicine|Renville") == TRUE ~ "Greater MN District 3",
    str_detect(county, "Stearns|Benton|Wright|Isanti|Morrison|Meeker|Sherburne|Chisago") == TRUE ~ "Greater MN District 4",
    str_detect(county, "Lincoln|Lyon|Redwood|Brown|Nicollet|Blue Earth|Waseca|Pipestone|Murray|Cottonwood|Watonwan|Sibley|Faribault|Rock|Nobles|Jackson|Martin|McLeod|Le Sueur") == TRUE ~ "Greater MN District 5",
    str_detect(county, "Rice|Steele|Wabasha|Winona|Fillmore|Goodhue|Dodge|Olmsted|Freeborn|Mower|Houston") == TRUE ~ "Greater MN District 6",
    TRUE ~ "Metro"
  ))

county_metadata <- st_drop_geometry(park_county) %>%
  left_join(st_drop_geometry(dnr_area_county) %>% rename(primary_county = county)) %>%
  left_join(st_drop_geometry(gmn_dist_county) %>% rename(primary_county = county))

##### vehicle multipliers #####
multipliers <- basic_metadata %>%
  mutate(
    vehicle_multiplier =
      case_when(
        short_agency == "Anoka County" ~ 2.33,
        short_agency == "Bloomington" ~ 1.71,
        short_agency == "Carver County" ~ 2.72,
        short_agency == "Dakota County" ~ 2.07,
        str_detect(short_agency, "MPRB") ~ 2.16,
        short_agency == "Ramsey County" ~ 1.84,
        str_detect(short_agency, "Saint Paul") ~ 1.96,
        short_agency == "Scott County" ~ 1.89,
        short_agency == "Three Rivers" ~ 2.30,
        short_agency == "Washington County" ~ 2.55,
        system == "DNR" ~ 3.2,
        system == "Greater MN" ~ 2.46
      )
  ) %>%
  select(zone_name, vehicle_multiplier)

##### combine all metadata #####
park_metadata <- labels_metadata %>%
  select(-primary_county) %>% # remove column used for inclusive polygons earlier
  left_join(county_metadata, by = "zone_name") %>%
  left_join(multipliers, by = "zone_name") %>%
  mutate(district = case_when(
    system == "DNR" ~ dnr_area,
    system == "Greater MN" ~ gmn_district,
    str_detect(system, "Metro") ~ short_agency
  )) %>%
  select(-gmn_district, -dnr_area)

## order by system, district, zone name
park_metadata <- park_metadata %>%
  arrange(system, district, zone_name) %>%
  group_by(system) %>%
  # create shapefile id
  mutate(
    sys = case_when(
      system == "DNR" ~ "dnr",
      system == "Greater MN" ~ "gmn",
      system == "Metro" ~ "metro",
      TRUE ~ "metroi"
    ),
    shp_id = paste0("p_", sys, "_", row_number())
  ) %>%
  ungroup() %>%
  select(-sys)

## check for any NAs (extra_id, short_agency, and long_agency will all have some NAs)
# sapply(park_metadata, function(x) sum(is.na(x)))

## check for any duplicate zone names
# park_metadata %>%
#   group_by(zone_name) %>% summarise(count = n()) %>% filter(count > 1)

## check for any unexpected duplicate unit labels
# park_metadata %>%
#   group_by(unit_label) %>% summarise(count = n()) %>% filter(count > 1)

## the final output will only include a few key columns
# keep all columns for use in future code
# this is a preview of the "final" metadata:
park_metadata %>%
  select(system_label, district, unit_label, zone_name, vehicle_multiplier, shp_id) # %>% View()

## create unit_id to match trail metadata; for parks unit_id = zone_name
park_metadata <- park_metadata %>%
  mutate(unit_id = zone_name)

##### save #####
saveRDS(park_metadata, file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))
saveRDS(park_metadata, file.path(here(), "data-intermediate/parks/all-parks-metadata.RDS"))
