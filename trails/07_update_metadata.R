# This script generates complete metadata for trails and trail segments. 
# Columns match park metadata where possible. Data is saved to `data-intermediate`.

##### setup #####
# load basic metadata
load(file.path(here(), "data-intermediate/trails/trail-meta1.rda"))

trail_meta1 <- bind_rows(dnr_meta1, gmn_meta1, metro_meta1)

# load geography
load(file.path(here(), "data-intermediate/trails/cleaned-trail-geography.rda"))


##### segment lengths #####
seg_lengths <- cleaned_trail_segments %>%
  mutate(approx_length_meters = st_length(.), 
         approx_length_feet = set_units(approx_length_meters, "ft"), 
         approx_length_miles = set_units(approx_length_meters, "mi")) %>%
  st_drop_geometry()

##### trail lengths #####
trail_lengths <- trail_lines %>%
  mutate(approx_length_meters = st_length(.), 
         approx_length_feet = set_units(approx_length_meters, "ft"), 
         approx_length_miles = set_units(approx_length_meters, "mi")) %>%
  st_drop_geometry()

##### geographic metadata #####

### primary county ### 
counties <- counties(state = "MN") %>%
  select(county = NAME) %>%
  st_transform(26915)

trail_county <- counties %>%
  st_intersection(trail_lines) %>%
  mutate(total_length_in_county = as.numeric(st_length(.))) %>%
  st_drop_geometry() %>%
  ungroup() %>% 
  group_by(unit_id) %>%
  mutate(max = max(total_length_in_county), 
         max_flag = ifelse(total_length_in_county == max, TRUE, FALSE), 
         primary_county = ifelse(max_flag == TRUE, county, NA), 
         all_counties = paste(county, collapse = ", ")) %>%
  fill(primary_county, .direction = "downup") %>%
  select(unit_id, primary_county, all_counties) %>%
  unique()

### DNR area ###
temp_dd <- tempfile()
download.file(url = "https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/bdry_dnr_parks_and_trails_admin/gpkg_bdry_dnr_parks_and_trails_admin.zip", destfile = temp_dd)
dnr_areas <- read_sf(unzip(temp_dd, "bdry_dnr_parks_and_trails_admin.gpkg"), layer = "dnr_parks_and_trails_areas") %>%
  st_transform(26915)

trail_dnr_area <- dnr_areas %>%
  st_intersection(trail_lines) %>%
  mutate(total_length_in_area = as.numeric(st_length(.))) %>%
  st_drop_geometry() %>%
  ungroup() %>% 
  group_by(unit_id) %>%
  mutate(max = max(total_length_in_area), 
         max_flag = ifelse(total_length_in_area == max, TRUE, FALSE), 
         primary_dnr_area = ifelse(max_flag == TRUE, ALTD_NAME, NA), 
         all_dnr_areas = paste(ALTD_NAME, collapse = ", ")) %>%
  fill(primary_dnr_area, .direction = "downup") %>%
  select(unit_id, primary_dnr_area, all_dnr_areas) %>%
  unique()

### Greater MN district ###
gmn_dist <- counties %>%
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
  )) %>%
  group_by(gmn_district) %>%
  summarise(do_union = TRUE)

trail_gmn_dist <- gmn_dist %>%
  st_intersection(trail_lines) %>%
  mutate(total_length_in_area = as.numeric(st_length(.))) %>%
  st_drop_geometry() %>%
  ungroup() %>% 
  group_by(unit_id) %>%
  mutate(max = max(total_length_in_area), 
         max_flag = ifelse(total_length_in_area == max, TRUE, FALSE), 
         primary_gmn_district = ifelse(max_flag == TRUE, gmn_district, NA), 
         all_gmn_districts = paste(gmn_district, collapse = ", ")) %>%
  fill(primary_gmn_district, .direction = "downup") %>%
  select(unit_id, primary_gmn_district, all_gmn_districts) %>%
  unique()

spatial_metadata <- trail_county %>%
  left_join(trail_dnr_area) %>%
  left_join(trail_gmn_dist)

##### fill in all labels #####
# zone_name: unique id used for analysis - zone_names identify SEGMENTS. 

# unit_id: unique id used to identify UNIT - for trails, this represents the entire trail. Only segments have zone_names. For parks, zone_name == unit_id.

# system: indicates "DNR", "Greater MN", or "Metro"

# system_label: indicates "DNR State", "Greater MN Regional", or "Metro Regional"

# extra_id: indicates any "extra" info needed to identify trail (for example, different sections of the Mesabi trail. Metro )

# unit_type: always "trail"

# unit_type_label: more descriptive than unit_type ("Regional Trail" or "State Trail", for example)

# unit_label: short name (with spaces), used for most plot labels
# metro trails include agency

# long_unit_label: longer name (with spaces), used for fact sheet titles
# unit_label plus " Regional Trail" or " State Trail"

# long_agency: only for metro parks, indicates implementing agency

# short_agency: only for metro parks, shortened agency name for plot labels, etc

label_metadata <- trail_meta1 %>%
  rename(extra_id = section) %>%
  select(-unit_name)

##### trail/unit metadata #####
## trail level metadata
# columns will match park metadata 
trail_metadata <- label_metadata %>%
  left_join(spatial_metadata) %>%
  left_join(trail_lengths %>% select(unit_id, approx_length_meters, approx_length_feet, approx_length_miles)) %>%
  mutate(primary_district = case_when(
    system == "DNR" ~ primary_dnr_area, 
    system == "Greater MN" ~ primary_gmn_district, 
    TRUE ~ short_agency
  )) %>%
  select(-primary_dnr_area, -primary_gmn_district) 

## order by system, district, unit_id
trail_metadata <- trail_metadata %>%
  arrange(system, primary_district, unit_id) %>%
  # create shapefile id
  group_by(system) %>% 
  mutate(sys = case_when(
    system == "DNR" ~ "dnr", 
    system == "Greater MN" ~ "gmn", 
    TRUE ~ "metro"
  ), 
  shp_id = paste0("t_", sys, "_", row_number())) %>%
  ungroup() %>% select(-sys)

## check for any NAs (extra_id, short_agency, and long_agency will all have some NAs)
sapply(trail_metadata, function(x) sum(is.na(x)))

## check for any duplicate zone names
trail_metadata %>%
  group_by(unit_id) %>% summarise(count = n()) %>% filter(count > 1)

## check for any unexpected duplicate unit labels
trail_metadata %>%
  group_by(unit_label) %>% summarise(count = n()) %>% filter(count > 1)

# the final output will only include a few key columns
# keep all columns for use in future code 
# this is a preview of the "final" metadata:
trail_metadata %>% 
  select(system_label, primary_district, unit_label, unit_id, shp_id) #%>% View()

##### zone/segment metadata #####
## additional information about individual segments
# segment-level metadata won't include county or district, that information will be aggregated at trail-level only

# confirm all unit_ids are accounted for
cleaned_trail_segments %>%
  filter(!unit_id %in% trail_metadata$unit_id)

# these are the units that got skipped due to poor OSM coverage
trail_metadata %>%
  filter(!unit_id %in% cleaned_trail_segments$unit_id)

# remove dropped units from trail-level metadata
trail_metadata <- trail_metadata %>%
  filter(unit_id %in% cleaned_trail_segments$unit_id)

trail_segment_metadata <- cleaned_trail_segments %>%
  st_drop_geometry() %>%
  left_join(seg_lengths) %>%
  left_join(label_metadata)

sapply(trail_segment_metadata, function(x) sum(is.na(x)))

##### save #####
save(trail_metadata, trail_segment_metadata, 
     file = file.path(here(), "data-intermediate/trails/trail-metadata.rda"))

all_trail_metadata <- trail_metadata
all_segment_metadata <- trail_segment_metadata
save(all_trail_metadata, all_segment_metadata, 
     file = file.path(here(), "data-intermediate/trails/all-trail-metadata.rda"))
