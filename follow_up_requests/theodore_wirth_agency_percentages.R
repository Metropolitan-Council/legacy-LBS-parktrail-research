## source basic utility functions, colors for plotting, etc
source(file.path(here::here(), "R/_load_packages.R"))
source(file.path(here(), "R/_utility_functions.R"))
source(file.path(here(), "R/_global_parameters.R"))
source(file.path(here(), "R/_streetlight_credentials_parameters.R"))
source(file.path(here(), "R/_set_aesthetics.R"))


meta <- readxl::read_xlsx("data-processed/unit_metadata_2023.07.xlsx")

theo <- meta %>%
  filter(unit_name == "Theodore Wirth, MPRB")

home_bg <- sf::read_sf("data-processed/home-locations/park-home-block-groups/park-home-block-groups.shp") %>%
  st_transform(4269)

theo_home_bg <- home_bg %>%
  select(bg_id,
         !!theo$shp_id) %>%
  filter(!is.na(p_metro_25))


mn_geo <- tigris::states() %>%
  filter(NAME == "Minnesota") %>%
  st_transform(4269)

minneapolis_geo <- tigris::places("MN") %>%
  filter(NAME == "Minneapolis") %>%
  st_transform(4269)

metro_geo <- tigris::counties("MN") %>%
  st_transform(4269) %>%
  filter(NAME %in% c("Anoka",
                     "Carver",
                     "Dakota",
                     "Hennepin",
                     "Ramsey",
                     "Scott",
                     "Washington")
  ) %>%
  st_union()

gmn_geo <- st_difference(mn_geo, st_union(st_combine(metro_geo)), dimension = "polygon") %>%
  st_make_valid()

## each implementing agency boundary
ia_geo <- read_sf("/Volumes/shared/CommDev/Research/Public/GIS/Parks/Park_Operating_Agencies.shp")  %>%
  st_transform(4269)

## suburban Hennepin County (all Hennepin Co minus Minneapolis, but including Bloomington)
hennepin_geo <- tigris::counties(state = "MN") %>%
  st_transform(4269) %>%
  filter(NAME == "Hennepin")
suburban_hennepin_geo <- st_difference(hennepin_geo, st_union(st_combine(minneapolis_geo)), dimension = "polygon") %>%
  st_make_valid()

#### block groups don't nest perfectly
# get proportion of bg in each region to make sure things sum to 100%
mn_bgs <- tigris::block_groups("MN", cb = TRUE) %>%
  mutate(bg_area = st_area(.))

mn_bg_area <- mn_bgs %>%
  st_drop_geometry() %>%
  select(GEOID, bg_area)

out_of_state_bgs <- theo_home_bg %>%
  filter(!bg_id %in% mn_bgs$GEOID) %>%
  select(GEOID = bg_id) %>%
  mutate(region = "Out of state") %>%
  st_drop_geometry() %>%
  mutate(bg_in_region = 1)

in_state_bgs <- mn_bgs %>%
  select(GEOID) %>%
  mutate(region = "Minnesota") %>%
  st_drop_geometry() %>%
  mutate(bg_in_region = 1)

metro_bgs <- st_intersection(mn_bgs, metro_geo) %>%
  st_make_valid() %>%
  mutate(bg_in_region = as.numeric(st_area(.)/bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "7-County Metropolitan Region") %>%
  st_drop_geometry()

gmn_bgs <- st_intersection(mn_bgs, gmn_geo) %>%
  st_make_valid() %>%
  mutate(bg_in_region = as.numeric(st_area(.)/bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Greater Minnesota (outside of 7-County Metro)") %>%
  st_drop_geometry()

minneapolis_bgs <- st_intersection(mn_bgs, minneapolis_geo) %>%
  st_make_valid() %>%
  mutate(bg_in_region = as.numeric(st_area(.)/bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Minneapolis") %>%
  st_drop_geometry()

suburban_hennepin_bgs <- st_intersection(mn_bgs, suburban_hennepin_geo) %>%
  st_make_valid() %>%
  mutate(bg_in_region = as.numeric(st_area(.)/bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Suburban Hennepin County (Hennepin County minus Minneapolis, including Bloomington)") %>%
  st_drop_geometry()

ia_bgs <- st_intersection(mn_bgs, ia_geo) %>%
  st_make_valid() %>%
  mutate(bg_in_region = as.numeric(st_area(.)/bg_area)) %>%
  select(region = Map_Label3, GEOID, bg_in_region) %>%
  st_drop_geometry()

##### combine
all_bgs <- bind_rows(out_of_state_bgs,
                     in_state_bgs,
                     metro_bgs,
                     gmn_bgs,
                     minneapolis_bgs,
                     suburban_hennepin_bgs,
                     ia_bgs) %>%
  left_join(theo_home_bg %>%
              st_drop_geometry() %>%
              rename(GEOID = bg_id)) %>%
  filter(!is.na(p_metro_25)) %>%
  replace_na(list(bg_in_region = 0)) %>%
  mutate(pct_visits_from_region = bg_in_region * p_metro_25) %>%
  group_by(region) %>%
  summarise(visitation_from_region = sum(pct_visits_from_region))

load(file.path(here(), "data-intermediate/processed/park-volume.rda"))
theo_21_visitation <- annual_volume %>%
  filter(year == 2021, zone_name == "Theodore-Wirth_park_Metro-Regional_MPRB")



results <- all_bgs %>%
  transmute(Region = region,
            `Estimated visits` = round(visitation_from_region * theo_21_visitation$annual_total),
            `Percent of visits` = 100 * visitation_from_region) %>%
  mutate(Region = factor(Region, levels = c("Minnesota", "Out of state",
                                            "7-County Metropolitan Region", "Greater Minnesota (outside of 7-County Metro)",
                                            "Suburban Hennepin County (Hennepin County minus Minneapolis, including Bloomington)",
                                            "Minneapolis",
                                            sort(ia_geo$Map_Label3)))) %>%
  arrange(Region)


metadata_sheet <- tibble(
  "Metadata" = c(
    "This dataset contains information about visitation to Theodore Wirth Regional Park as estimated by location based services data (LBS). LBS data was provided by StreetLight Data, Inc and accessed in July 2023. LBS data is an aggregated and anonymized data source providing information about when and where people travel. Users are advised that LBS data provided by StreetLight uses a sample based approach, and thus all numbers provided are estimates. More information about StreetLight's methodology can be found on their website [https://www.streetlightdata.com/].",
    "The data in this spreadsheet is derived from StreetLight Data, Inc's inferred home locations. StreetLight Data infers home locations based on where the devices (i.e., smartphones with opt-in GPS services enabled) in their sample 'spend the night' most often. Nighttime hours are defined as 8pm - 7am daily and home locations are restricted to residential census blocks.",
    "A StreetLight analysis for the entire calendar year 2021 (Jan 1, 2021 - Dec 31, 2021) was run to produce the estimates in this spreadsheet. Home location data was validated against intercept survey data collected during the summer of 2021.",
    "Please note that StreetLight Data cannot identify unique visitors to a given park. Therefore, estimates in this dataset represent total visits, not visitors, to Theodore Wirth Regional Park. In other words, 100 visits from a given region could be equivalent to 100 individuals visiting 1 time each, or 1 individual visiting 100 separate times. Estimates in this sheet were produced by aggregating results for census block groups into the regions of interest.",
    "This work was funded with Legacy Partnership Research Funds from the State of Minnesota Park and Trails Legacy Fund. More information about the data and results from this project can be found at [https://minnesota-parks-and-trails-metrocouncil.hub.arcgis.com].",
    "Region - A region of visitor home locations.",
    "Estimated visits - Total estimated visits from a given home location region during 2021.",
    "Percent of visits - Estimated percent of visits from a given home location region during 2021."
  )
)

wb <- createWorkbook()

addWorksheet(wb, "metadata")
writeData(wb, sheet = 1, x = metadata_sheet)
addStyle(wb,
         sheet = "metadata",
         style = openxlsx::createStyle(wrapText = TRUE),
         rows = 2:nrow(metadata_sheet),
         cols = 1,
         gridExpand = F,
         stack = TRUE
)
setColWidths(wb, sheet = 1, cols = 1)

addWorksheet(wb, "results")
writeData(wb, sheet = 2, x = results)
setColWidths(wb, sheet = 2, cols = c(1:ncol(results)), widths = "auto")

saveWorkbook(wb, file.path(here::here(), "follow_up_requests/theodore_wirth_visitor_home_locations.xlsx"),
             overwrite = TRUE
)

# theo_mpls <- st_intersection(theo_home_bg, minneapolis)
# theo_metro <- st_intersection(theo_home_bg, metro)
# theo_mn <- st_intersection(theo_home_bg, mn)
# theo_suburb_henn <- st_intersection(theo_home_bg, suburban_hennepin)
# theo_ia <- st_intersection(theo_home_bg, ia)

# # about 43% from block groups in Minneapolis
# sum(theo_mpls$p_metro_25)
#
# # and 52% from suburban Hennepin
# sum(theo_suburb_henn$p_metro_25)
#
# # about 97% in metro (~3% Greater MN)
# sum(theo_metro$p_metro_25)
#
# # and 98.9% in state
# sum(theo_mn$p_metro_25)

# theo_ia_pcts <- theo_ia %>%
#   group_by(Map_Label) %>%
#   summarise(pct = sum(p_metro_25))
#
# sum(theo_ia_pcts$pct)

## block groups don't nest perfectly into agency boundaries
# first, intersect block groups with agencies to get % of bg per agency
# bgs <- tigris::block_groups("MN", year = 2020)
# bg_areas <- bgs %>%
#   select(GEOID) %>%
#   mutate(bg_area = st_area(.)) %>%
#   st_drop_geometry()
# bg_ia <- st_intersection(bgs, ia)
#
# bg_ia_prop <- bg_ia %>%
#   select(GEOID, Map_Label) %>%
#   mutate(area = st_area(.)) %>%
#   left_join(bg_areas) %>%
#   mutate(prop_bg_in_ia = area/bg_area)
#
# theo_bg_ia <- bg_ia_prop %>%
#   full_join(theo_home_bg %>%
#               st_drop_geometry() %>%
#               rename(GEOID = bg_id)) %>%
#   filter(!is.na(p_metro_25)) %>%
#   mutate(visits_in_ia = prop_bg_in_ia * p_metro_25) %>%
#   group_by(Map_Label) %>%
#   summarise(pct = sum(visits_in_ia)) %>%
#   filter(!is.na(Map_Label))
#
# sum(theo_bg_ia$pct)
#
#
# ## combine
#
# instate <- theo_mn %>%
#   st_drop_geometry() %>%
#   mutate(region = "Minnesota") %>%
#   group_by(region) %>%
#   summarise(percent_visitation = sum(p_metro_25))
#
# out_of_state <- data.frame(region = "Out of state",
#                            percent_visitation = 1 - instate$percent_visitation)
#
# minneapolis <- theo_mpls %>%
#   st_drop_geometry() %>%
#   mutate(region = "Minneapolis") %>%
#   group_by(region) %>%
#   summarise(percent_visitation = sum(p_metro_25))
#
# suburban_hennepin <- theo_suburb_henn %>%
#   st_drop_geometry() %>%
#   mutate(region = "Suburban Hennepin County (Hennepin County minus Minneapolis)") %>%
#   group_by(region) %>%
#   summarise(percent_vistation = sum(p_metro_25))
#
# greater_mn <- st_difference(theo_home_bg, metro)
#
# metro <- theo_metro %>%
#   st_drop_geometry() %>%
#   mutate(region = "7-County Metropolitan Area") %>%
#   group_by(region) %>%
#   summarise(percent_visitation = sum(p_metro_25))
#
#
# ias <- theo_bg_ia %>%
#   st_drop_geometry() %>%
#   rename(region = Map_Label) %>%
#   group_by(region) %>%
#   summarise(percent_visitation = sum(pct))
#
# sum(ias$percent_visitation)
# metro$percent_visitation
