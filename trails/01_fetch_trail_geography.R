# This script downloads original, unedited shapefiles for each trail in the sample. 
# Unique trail/unit names are established in this script (unique zone names will include
# OSM segment IDs and will be generated once OSM data is retrieved). All geographies are
# saved in the `data-intermediate` folder. DNR State and Metropolitan Regional files are 
# downloaded from the MN Geospatial Commons.  Greater MN files were obtained via personal
# communication with Renee Mattson in June 2021;  these files are available in the `data-raw` 
# folder of this repository.


##### DNR trails #####
temp_dnr <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_dnr/trans_state_trails_minnesota/gpkg_trans_state_trails_minnesota.zip", destfile = temp_dnr)
dnr <- read_sf(unzip(temp_dnr, "trans_state_trails_minnesota.gpkg")) %>%
  st_make_valid() %>%
  st_transform(26915)

fs::file_delete("trans_state_trails_minnesota.gpkg")

dnr_trails <- dnr %>%
  # combine all sections of a trail
  group_by(trail_name) %>%
  summarise(do_union = TRUE) %>%
  mutate(original_name = trail_name, 
         unit_name = str_remove_all(trail_name, " State Trail"), 
         unit_name = str_remove_all(unit_name, " MS84.029"), 
         unit_type = "trail",
         unit_type_label = "State Trail",
         system = "DNR", 
         system_label = "DNR State", 
         unit_label = unit_name, 
         long_unit_label = paste(unit_label, unit_type_label), 
         # create unique ID for unit/trail (these are NOT zone names for use with StL)
         unit_id = str_remove_all(unit_name, ",|'|\\."), 
         unit_id = str_replace_all(unit_id, "/| ", "-"), 
         unit_id = paste(unit_id, unit_type, system, sep = "_")) %>%
  select(-trail_name)

dnr_meta1 <- dnr_trails %>%
  st_drop_geometry() %>%
  select(unit_name, unit_type,  unit_type_label, system, system_label, 
         unit_label, long_unit_label, unit_id) %>%
  unique() 

dnr_trails <- dnr_trails %>%
  # combine all sections of a trail 
  group_by(unit_id) %>%
  summarise(do_union = TRUE) %>%
  left_join(dnr_meta1)

## check for duplicate unit_ids
dnr_dups <- dnr_trails %>%
  group_by(unit_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

if(nrow(dnr_dups) > 0){
  stop("Duplicate dnr unit_ids")
}

##### Greater MN trails #####
gmn <- st_read(file.path(here::here(), "data-raw/Greater-MN/GreaterMN_trails_2022_11_23/record_set1.shp")) %>% #qualified
  transmute(trail_name = trai_name, geometry = geometry) %>%
  bind_rows(st_read(file.path(here::here(), "data-raw/Greater-MN/GreaterMN_trails_2022_11_23/inputLayers.shp")) %>%
              select(trail_name) %>%
              filter(trail_name != "<NA>")) %>%
  # remove non-trails
  filter(!str_detect(trail_name, "Duluth Traverse Trail Center")) %>%
  st_transform(26915)

gmn_trails <- gmn %>%
  mutate(original_name = trail_name, 
         # correct some typos
         trail_name = str_replace_all(str_trim(trail_name), "Galcial", "Glacial"), 
         # label trails with multiple sections
         overall_trail_name = case_when(
           str_detect(trail_name, "Cook County Mountain Bike") ~ "Cook County Mountain Bike",
           str_detect(trail_name, "Aitkin County Northwoods ATV Trail") ~ "Aitkin County Northwoods ATV",
           str_detect(trail_name, "Mesabi") ~ "Mesabi",
           str_detect(trail_name, "Soo Line") ~ "Soo Line",
           str_detect(trail_name, "Greater Mankato River Valley") ~ "Greater Mankato River Valley Trail System",
           TRUE ~ str_replace_all(trail_name, ' Trail| Trai| Trail System| System| Regional Trail', "")
         )) %>%
  # get labels for the subsections of those trails
  group_by(trail_name) %>%
  mutate(section = case_when(
    # determined based on map
    trail_name == "Cook County Mountain Bike Trail System" ~ "Pincushion",
    trail_name == "Cook County Mountain Bike System" ~ "Britton Peak",
    # Aitkin County ATV
    str_detect(trail_name, "Aitkin County Northwoods ATV Trail") ~ str_trim(str_split(trail_name, "-")[[1]][2]), 
    # Mesabi
    str_detect(trail_name, "Mesabi") ~ str_trim(str_split(trail_name, "-")[[1]][3]),  
    # Greater Mankato River Valley
    str_detect(trail_name, "Greater Mankato River Valley") ~ str_trim(str_split(trail_name, "-")[[1]][1]), 
    # Soo Line
    str_detect(trail_name, "Soo Line") ~ str_remove(str_remove(trail_name, "Soo Line "), "Trail - "), 
    # others
    TRUE ~ NA
  ), 
  # one quirk with Greater Mankato
  section = ifelse(str_detect(section, "South Route Trail "), "South Route", section)) %>%
  ungroup() %>% 
  # now, match names to DNR trails above
  mutate(unit_name = ifelse(is.na(section), 
                            # no section
                            overall_trail_name, 
                            # section
                            paste0(overall_trail_name, ", ", section)), 
         unit_name = str_remove_all(unit_name, " Connector and Loop Trail| Connector Trail| Trail System| Trail Segment| Trail"), 
         # a few specific fixes
         unit_name = case_when(
           unit_name == "Otter Tail County - Perham to Pelican Rapids" ~ "Otter Tail County, Perham to Pelican Rapids", 
           unit_name == "Lake Vermillion - Cook County spur" ~ "Lake Vermillion, Cook County", 
           unit_name == "Waabisheshikana \"The Martin\"" ~ "Waabisheshikana 'The Marten'", 
           TRUE ~ unit_name
         ), 
         unit_type = "trail",
         unit_type_label = "Regional Trail",
         system = "Greater MN", 
         system_label = "Greater MN Regional", 
         unit_label = unit_name, 
         long_unit_label = ifelse(str_detect(unit_label, ", "), 
                                  # if there's a comma, insert "Regional Trail" before comma
                                  paste(str_replace_all(unit_label, ", ", " Regional Trail, "), "Section"), 
                                  # otherwise, add "Regional Trail" at the end
                                  paste(unit_label, unit_type_label)), 
         # create unique ID for unit/trail (these are NOT zone names for use with StL)
         unit_id = str_remove_all(unit_name, ",|'|\\.|:"), 
         unit_id = str_replace_all(unit_id, "/| ", "-"), 
         unit_id = paste(unit_id, unit_type, str_replace(system, " ", "-"), sep = "_"), 
         unit_id = ifelse(str_detect(unit_id, "Waabisheshikana"), "Waabisheshikana-The-Marten_trail_Greater-MN", unit_id)) 


gmn_meta1 <- gmn_trails %>%
  st_drop_geometry() %>%
  select(unit_name, section, unit_type,  unit_type_label, system, 
         system_label, unit_label, long_unit_label, unit_id) %>%
  unique() 

gmn_trails <- gmn_trails %>%
  # combine all sections of a trail 
  group_by(unit_id) %>%
  summarise(do_union = TRUE) %>%
  rename(geom = geometry) %>%
  left_join(gmn_meta1)

## check for duplicate unit_ids
gmn_dups <- gmn_trails %>%
  group_by(unit_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

if(nrow(gmn_dups) > 0){
  stop("Duplicate gmn unit_ids")
}

##### Metro regional trails #####
temp_mc <- tempfile()
download.file("https://resources.gisdata.mn.gov/pub/gdrs/data/pub/us_mn_state_metc/trans_regional_trails_exst_plan/gpkg_trans_regional_trails_exst_plan.zip", destfile = temp_mc)
metro <- read_sf(unzip(temp_mc, "trans_regional_trails_exst_plan.gpkg")) %>%
  filter(STATUS == "Existing (Open to Public)") %>%
  st_make_valid() %>%
  st_transform(26915)

fs::file_delete("trans_regional_trails_exst_plan.gpkg")

metro_trails <- metro %>%
  filter(NAME != "River Crossing") %>%
  mutate(original_name = NAME, 
         short_agency = case_when(
           str_detect(Agency, "Anoka") ~ "Anoka County",
           str_detect(Agency, "Bloomington") ~ "Bloomington",
           str_detect(Agency, "Carver") ~ "Carver County",
           str_detect(Agency, "Dakota") ~ "Dakota County",
           str_detect(Agency, "Minneapolis") ~ "MPRB",
           str_detect(Agency, "Ramsey") ~ "Ramsey County",
           str_detect(Agency, "Scott") ~ "Scott County",
           str_detect(Agency, "Paul") ~ "Saint Paul",
           str_detect(Agency, "Three Rivers") ~ "Three Rivers",
           str_detect(Agency, "Washington") ~ "Washington County",
           str_detect(Agency, "Wright") ~ "Three Rivers"
         ),
         # some corrections - make consistent with parks
         long_agency = case_when(
           short_agency == "Anoka County" ~ "Anoka County Parks and Recreation", 
           short_agency == "Bloomington" ~ "Bloomington Parks and Recreation", 
           short_agency == "Carver County" ~ "Carver County Parks and Recreation", 
           short_agency == "Dakota County" ~ "Dakota County Parks", 
           short_agency == "MPRB" ~ "Minneapolis Park and Recreation Board", 
           short_agency == "Ramsey County" ~ "Ramsey County Parks and Recreation", 
           short_agency == "Scott County" ~ "Scott County Parks", 
           short_agency == "Saint Paul" ~ "Saint Paul Parks and Recreation", 
           short_agency == "Three Rivers" ~ "Three Rivers Park District",
           short_agency == "Washington County" ~ "Washington County Parks", 
           TRUE ~ NA
         ), 
         unit_name = paste0(Label, ", ", short_agency)) %>%
  ungroup() %>%
  mutate(
    # some typos
    unit_name = str_replace_all(unit_name, "  ", " "), 
    section = short_agency, 
    unit_type = "trail", 
    unit_type_label = "Regional Trail", 
    system = "Metro", 
    system_label = "Metro Regional", 
    unit_label = unit_name, 
    long_unit_label = str_replace_all(unit_label, ", ", " Regional Trail, "), 
    # need Label for unit_id
    Label = str_replace_all(Label, "  ", " "),
    Label = str_remove_all(Label, ",|'|\\.|\\(|\\)"),
    Label = str_replace_all(Label, "/| ", "-"), 
    Label = str_replace_all(Label, "---", "-"), 
    unit_id = paste(Label, unit_type, 
                    str_replace_all(system_label, " ", "-"), 
                    str_replace_all(short_agency, " ", "-"), sep = "_")
  )


metro_meta1 <- metro_trails %>%
  st_drop_geometry() %>%
  select(short_agency, long_agency, unit_name, section, unit_type, unit_type_label, 
         system, system_label, unit_label, long_unit_label, unit_id) %>%
  unique() 

metro_trails <- metro_trails %>%
  # combine all sections of a trail (within an agency)
  group_by(unit_id) %>%
  summarise(do_union = TRUE) %>%
  left_join(metro_meta1)

## check for duplicate unit_ids
metro_dups <- metro_trails %>%
  group_by(unit_id) %>%
  summarise(count = n()) %>%
  filter(count > 1)

if(nrow(metro_dups) > 0){
  stop("Duplicate metro unit_ids")
}

##### save #####
trail_lines <- bind_rows(dnr_trails, gmn_trails, metro_trails)

saveRDS(trail_lines, file.path(here(), "data-intermediate/trails/trail-lines.RDS"))
save(dnr_meta1, gmn_meta1, metro_meta1, 
     file = file.path(here(), "data-intermediate/trails/trail-meta1.rda"))
