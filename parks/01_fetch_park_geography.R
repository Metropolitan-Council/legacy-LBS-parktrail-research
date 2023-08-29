# This script downloads original, unedited shapefiles for each park in the sample.
# Unique zone names are established in this script. This script additionally downloads
# shapefiles of water, golf courses, and roadways, which are used to edit park polygons.
# All geographies are saved in the `data-intermediate` folder of this repository.
# DNR and Metropolitan Regional files are downloaded from the MN Geospatial Commons.
# Greater MN files were obtained via personal communication with Renee Mattson in June 2021;
# these files are available in the `data-raw` folder of this repository.

##### global features #####
## some files may take longer to download
options(timeout = max(300, getOption("timeout")))

### water ###
temp_water <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/water_dnr_hydrography/gpkg_water_dnr_hydrography.zip", destfile = temp_water)
water <- read_sf(unzip(temp_water, "water_dnr_hydrography.gpkg"), layer = "dnr_hydro_features_all") %>%
  st_transform(26915)

fs::file_delete("water_dnr_hydrography.gpkg")

### mndot roads ###
temp_roads <- tempfile()
temp2 <- tempfile()

download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dot/trans_roads_centerlines/shp_trans_roads_centerlines.zip", destfile = temp_roads)

unzip(zipfile = temp_roads, exdir = temp2)
# list.files(temp2) # list of all possible layers to download
mndot <- sf::read_sf(paste0(temp2, pattern = "/MnDOT_Roadway_Routes_in_Minnesota.shp")) %>%
  st_zm() %>%
  st_transform(26915)


##### DNR parks #####
temp_dnr <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/bdry_dnr_lrs_prk/gpkg_bdry_dnr_lrs_prk.zip", destfile = temp_dnr)
dnr_management_boundaries <- read_sf(unzip(temp_dnr, "bdry_dnr_lrs_prk.gpkg"), layer = "dnr_management_units_prk") %>%
  st_transform(26915) %>%
  # exclude islands
  filter(UNIT_NAME != "Garden Island", 
         UNIT_NAME != "Saint Croix Islands") %>%
  mutate(original_name = UNIT_NAME,
         unit_label = str_replace_all(UNIT_NAME, "St\\.", "Saint"),
         zone_name = paste0(str_replace_all(unit_label, " ", "-"), "_park_DNR"),
         zone_name = str_remove_all(zone_name, "\\."),
         zone_name = str_replace_all(zone_name, "\\/", "-"),
         zone_name = str_replace_all(zone_name, "\\'", "-")) %>%
  mutate(extra_id = ifelse(UNIT_TYPE == "State Park", NA, str_replace_all(UNIT_TYPE, " ", "-")),
         unit_type_label = UNIT_TYPE,
         zone_name = ifelse(is.na(extra_id), zone_name, paste0(zone_name, "_", extra_id)), 
         unit_type = "park")

fs::file_delete("bdry_dnr_lrs_prk.gpkg")

# confirm unique zones, contains _park_, contains _DNR_, no punctuation (. , / \ ')
dnr_unique <- nrow(dnr_management_boundaries) == length(unique(dnr_management_boundaries$zone_name))
if(dnr_unique == FALSE){
  stop("DNR zone names are not unique")
}
if(isTRUE(any(str_detect(dnr_management_boundaries$zone_name, "\\.|'|/|'")))){
  stop("DNR zone names contain punctuation")
}
if(all(str_detect(dnr_management_boundaries$zone_name, "_park_")) == FALSE){
  stop("DNR zone names missing park tag")
}
if(all(str_detect(dnr_management_boundaries$zone_name, "_DNR")) == FALSE){
  stop("DNR zone names missing DNR tag")
}

## get water & roads that intersect with parks
dnr_water <- water %>%
  st_join(st_buffer(dnr_management_boundaries, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

dnr_mndot_roads <- mndot %>%
  st_join(st_buffer(dnr_management_boundaries, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

## split geographies into lists for easy processing
dnr_water <- dnr_water %>%
  split(f = dnr_water[["zone_name"]])

dnr_mndot_roads <- dnr_mndot_roads %>%
  split(f = dnr_mndot_roads[["zone_name"]])

dnr <- dnr_management_boundaries %>%
  split(f = dnr_management_boundaries[["zone_name"]])

## save
save(dnr, dnr_water, dnr_mndot_roads,
     file = file.path(here(), "data-intermediate/parks/raw-dnr-geography.rda"))

##### Greater MN parks #####
gmn_raw <- st_read(paste0(here(), "/data-raw/Greater-MN/Designated/Designated.shp")) %>%
  mutate(valid = st_is_valid(geometry),
         park_name = ifelse(is.na(park_name), "Robbins Island Park", park_name))

## correct invalid "bowtie" geometry
bluffs <- gmn_raw %>%
  filter(park_name == "Bluffs Traverse") %>%
  st_cast("POINT") %>% filter(row_number() != 34) %>%
  st_as_sf() %>%
  mutate(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
  st_cast("MULTIPOLYGON") %>%
  st_transform(26915) %>%
  unique()

gmn_valid <- gmn_raw %>%
  filter(park_name != "Bluffs Traverse") %>%
  rbind(bluffs) %>%
  st_make_valid()

gmn_valid <- gmn_valid %>%
  group_by(park_name) %>%
  summarise(do_union = TRUE)

## read parks that were eligible as of June 2021
gmn_raw_2 <- st_read(paste0(here(), "/data-raw/Greater-MN/Qualified_For_Designation/Qualified for Designation.shp")) %>%
  st_make_valid() %>%
  mutate(extra_id = "Eligible") %>%
  select(extra_id, park_name, geometry)

## combine and tag eligible parks
gmn <- gmn_valid %>%
  bind_rows(gmn_raw_2) %>%
  mutate(original_name = park_name,
         unit_label = str_remove_all(park_name, " County Park| Park & Campground| County park| Recreation Area| Recreational Area| Park & Nature Center| Sports Park| Park Reserve| Park| park"),
         unit_label = ifelse(str_detect(unit_label, "Grand Marais"), "Grand Marais Recreation Area", unit_label), # one exception, "Grand Marais" doesn't make sense alone as a park name
         unit_label = str_replace_all(unit_label, "&", "and"),
         unit_type = "park",
         zone_name = paste0(str_replace_all(unit_label, " ", "-"), "_park_Greater-MN"),
         zone_name = ifelse(is.na(extra_id), zone_name, paste0(zone_name, "_", extra_id)),
         zone_name = str_replace_all(zone_name, "\\/|---", "-"),
         zone_name = str_remove_all(zone_name, "\\.|\\(|\\)"), # remove punctuation
         unit_type_label = case_when(
           str_detect(original_name, "Recreation Area|Recreational Area") ~ "Regional Recreation Area",
           str_detect(original_name, "Park Reserve") ~ "Regional Park Reserve",
           unit_label == "Grand Marais Recreation Area" ~ "", # so we don't duplicate "Recreation Area" in plot labels later
           TRUE ~ "Regional Park"
         )
  )

## checks
gmn_unique <- nrow(gmn) == length(unique(gmn$park_name))
if(gmn_unique == FALSE){
  stop("Greater MN zone names are not unique")
}
if(isTRUE(any(str_detect(gmn$zone_name, "\\.|'|/|'")))){
  stop("Greater MN zone names contain punctuation")
}
if(all(str_detect(gmn$zone_name, "_park_")) == FALSE){
  stop("Greater MN zone names missing park tag")
}
if(all(str_detect(gmn$zone_name, "_Greater-MN")) == FALSE){
  stop("Greater MN zone names missing Greater MN tag")
}

gmn_water <- water %>%
  st_join(st_buffer(gmn, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

gmn_mndot_roads <- mndot %>%
  st_join(st_buffer(gmn, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

## split for easy processing
gmn_water <- gmn_water %>%
  split(f = gmn_water[["zone_name"]])

gmn_mndot_roads <- gmn_mndot_roads %>%
  split(f = gmn_mndot_roads[["zone_name"]])

gmn <- gmn %>%
  split(f = gmn[["zone_name"]])

## save
save(gmn, gmn_water, gmn_mndot_roads,
     file = file.path(here(), "data-intermediate/parks/raw-gmn-geography.rda"))


##### Metro parks #####
temp_mc <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_parks_regional/gpkg_plan_parks_regional.zip",
              destfile = temp_mc)

mc_raw <- sf::read_sf(unzip(temp_mc, "plan_parks_regional.gpkg")) %>%
  mutate(AGENCY = ifelse(AGENCY == "St. Paul Parks and Recreation",
                         "Saint Paul Parks and Recreation", AGENCY),
         CATEGORY = ifelse(CATEGORY == "Regional park", "Regional Park", CATEGORY),
         # two parks need to have their agency names corrected
         AGENCY = case_when(
           str_detect(PARKNAME, "Lilydale") ~ "Saint Paul Parks and Recreation",
           str_detect(PARKNAME, "Theodore Wirth") ~ "Minneapolis Park and Recreation Board",
           TRUE ~ AGENCY)) %>%
  filter(STATUS == "Existing") %>%
  group_by(PARKNAME, AGENCY, STATUS, CATEGORY) %>%
  rename(NAME = PARKNAME) %>%
  summarize(do_union = TRUE) %>%
  ungroup() %>%
  st_transform(26915) %>%
  st_as_sf()

### corrections to Hyland-Bush-Anderson Lakes
## this park has two implementing agencies, but the geographies are mis-assigned in the original shapefile
## here, re-assign each parcel to the correct implementing agency

hyland_raw <- sf::read_sf(unzip(temp_mc, "plan_parks_regional.gpkg")) %>%
  filter(str_detect(PARKNAME, "Hyland")) %>%
  filter(STATUS == "Existing") %>%
  rename(NAME = PARKNAME) %>%
  st_transform(26915) %>%
  st_as_sf()

fs::file_delete("plan_parks_regional.gpkg")

## select individual parcels to be re-assigned
tierneys_woods <- c("053-1911621220001", "053-1811621330001", "053-1811621340001", "053-1911621210042",
                    "053-1811621310001", "053-1811621310001", "053-1811621420053", "053-1811621420001",
                    "053-1811621420002", "053-1811621130063", "053-1811621120017", "053-1811621110021")

hyland_unit <- c("053-1711621430012", "053-1711621430012", "053-2911621210002", "053-2911621320003",
                 "Water", "053-3211621210007", "053-2011621420002", "053-2011621130006")

hyland_split <- hyland_raw %>%
  mutate(AGENCY = ifelse(PIN_1 %in% c(tierneys_woods, hyland_unit), "Three Rivers Park District", "Bloomington Parks and Recreation"))

hyland_split <- hyland_split %>%
  group_by(NAME, AGENCY, STATUS, CATEGORY) %>%
  summarise(do_union = TRUE)

## check 
# mapview(hyland_split, zcol = "AGENCY")

### Como edits ###
## here, combine the zoo and conservatory into one unit
mc_raw <- mc_raw %>%
  filter(!str_detect(NAME, "Hyland")) %>%
  bind_rows(hyland_split) %>%
  mutate(NAME = ifelse(NAME %in% c("Como (Marjorie McNeely Conservatory)", "Como (Como Zoo)"),
                       "Como Zoo and Conservatory", NAME)) %>%
  group_by(NAME, AGENCY, STATUS, CATEGORY) %>%
  summarise(do_union = TRUE) %>%
  ungroup()

### combine metro parks ###
metro <- mc_raw %>%
  # get shorter agency name for labeling purposes
  mutate(short_agency = case_when(
    AGENCY == "Anoka County Parks and Recreation" ~ "Anoka County",
    AGENCY == "Bloomington Parks and Recreation" ~ "Bloomington",
    AGENCY == "Carver County Parks and Recreation" ~ "Carver County",
    AGENCY == "Dakota County Parks" ~ "Dakota County",
    AGENCY == "Minneapolis Park and Recreation Board" ~ "MPRB",
    AGENCY == "Ramsey County Parks and Recreation" ~ "Ramsey County",
    AGENCY == "Saint Paul Parks and Recreation" ~ "Saint Paul",
    AGENCY == "Scott County Parks" ~ "Scott County",
    AGENCY == "Three Rivers Park District" ~ "Three Rivers",
    AGENCY == "Washington County Parks" ~ "Washington County",
  ),
  long_agency = AGENCY,
  original_name = NAME, 
  unit_label = NAME, 
  unit_type = "park") %>%
  mutate(zone_name = str_replace_all(paste0(NAME, "_park_Metro-Regional_", short_agency), " ", "-"),
         zone_name = str_remove_all(zone_name, "\\."),
         zone_name = str_replace_all(zone_name, "\\/", "-"),
         zone_name = str_replace_all(zone_name, "St-", "Saint-"),
         zone_name = str_replace(zone_name, "Anoka-Co-Riverfront-Island", "Anoka-Riverfront-and-Island"),
         unit_type_label = case_when(
           CATEGORY == "Park Reserve" ~ "Regional Park Reserve",
           CATEGORY == "Special Recreation Feature" ~ "Regional Special Recreation Feature",
           TRUE ~ "Regional Park"
         )) %>%
  # exclude islands
  filter(!str_detect(zone_name, "Lake-Minnetonka-Islands")) 

## checks
metro_unique <- nrow(metro) == length(unique(metro$zone_name))
if(metro_unique == FALSE){
  stop("Metro zone names are not unique")
}
if(isTRUE(any(str_detect(metro$zone_name, "\\.|'|/|'")))){
  stop("Metro zone names contain punctuation")
}
if(all(str_detect(metro$zone_name, "_park_")) == FALSE){
  stop("Metro zone names missing park tag")
}
if(all(str_detect(metro$zone_name, "_Metro-Regional_")) == FALSE){
  stop("Metro zone names missing Metro and/or agency tag")
}

### metro golf courses ###
## the metro region's traditional use estimates do not account for golf courses
## here, remove golf courses from parks where applicable

temp_golf <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/plan_generl_lnduse2020/gpkg_plan_generl_lnduse2020.zip", destfile = temp_golf)
golf <- read_sf(unzip(temp_golf, "plan_generl_lnduse2020.gpkg")) %>%
  st_transform(26915) %>%
  filter(DESC2020 == "Golf Course")

fs::file_delete("plan_generl_lnduse2020.gpkg")
metro <- metro %>%
  st_erase(golf) %>%
  arrange(zone_name)

metro_water <- water %>%
  st_join(st_buffer(metro, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

metro_mndot_roads <- mndot %>%
  st_join(st_buffer(metro, 30), join = st_intersects) %>%
  filter(!is.na(zone_name))

## split for easy processing
metro_water <- metro_water %>%
  split(f = metro_water[["zone_name"]])

metro_mndot_roads <- metro_mndot_roads %>%
  split(f = metro_mndot_roads[["zone_name"]])

metro <- metro %>%
  split(f = metro[["zone_name"]])

## save
save(metro, metro_water, metro_mndot_roads,
     file = file.path(here(), "data-intermediate/parks/raw-metro-geography.rda"))
