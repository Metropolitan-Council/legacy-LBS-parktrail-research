## This script creates several table-based appendices documenting trails which were excluded
## from analyses, dropped for coverage issues, etc. 

##### trails excluded from all analyses #####
load(file.path(here(), "data-intermediate/trails/all-trail-metadata.rda"))
load(file.path(here(), "data-intermediate/processed/trail-volume.rda"))

dropped_pre_LBS <- data.frame(
  system = c("DNR State", rep("Greater MN Regional", 3), rep("Metro Regional", 3)), 
  unit_name = c("OHV access to Taconite via USFS Rd 11404", 
                "Houston County trail", 
                "Red Lake River Corridor", 
                "Zumbro River", 
                "Mississippi Gorge/West River Parkway, Saint Paul", 
                "Northeast Diagonal, MPRB/Three Rivers", 
                "Rice Creek North Extension, Ramsey County")
)

dropped_entirely <- all_trail_metadata %>%
  filter(!unit_id %in% c(annual_trail_volume$unit_id)) %>%
  select(system = system_label, 
         unit_name = unit_label) 

load(file.path(here(), "data-intermediate/trails/raw-monthly-segments.rda"))
raw_monthly_segments$zone_name %>% n_distinct()

dropped_segments <- all_segment_metadata %>%
  filter(!zone_name %in% raw_monthly_segments$zone_name) %>%
  group_by(system, unit_label) %>%
  summarise(dropped_segments = n()) %>%
  mutate(reason = "No LBS data returned")

dropped_for_length <- all_segment_metadata %>%
  filter(as.numeric(approx_length_feet) < 50) %>%
  group_by(system, unit_label) %>%
  summarise(dropped_segments = n()) %>%
  mutate(reason = "Less than 50ft")

save(dropped_pre_LBS, dropped_entirely, dropped_segments, dropped_for_length, 
     file = file.path(here(), "data-intermediate/documentation/trail-appendices.rda"))
