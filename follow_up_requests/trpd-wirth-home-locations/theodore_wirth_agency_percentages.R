##### setup #####

## source basic utility functions, colors for plotting, etc
source(file.path(here::here(), "R/_load_packages.R"))
source(file.path(here(), "R/_utility_functions.R"))
source(file.path(here(), "R/_global_parameters.R"))
source(file.path(here(), "R/_streetlight_credentials_parameters.R"))
source(file.path(here(), "R/_set_aesthetics.R"))

## read data
meta <- readxl::read_xlsx("data-processed/unit_metadata_2023.07.xlsx")

theo <- meta %>%
  filter(unit_name == "Theodore Wirth, MPRB")
# theodore wirth's shp_id = "p_metro_25"

home_bg <- sf::read_sf("data-processed/home-locations/park-home-block-groups/park-home-block-groups.shp") %>%
  st_transform(4269)

theo_home_bg <- home_bg %>%
  select(
    bg_id,
    !!theo$shp_id
  ) %>%
  filter(!is.na(p_metro_25))

##### get geographies ####
metro_counties <- tigris::counties("MN") %>%
  st_transform(4269) %>%
  filter(NAME %in% c(
    "Anoka",
    "Carver",
    "Dakota",
    "Hennepin",
    "Ramsey",
    "Scott",
    "Washington"
  ))


## state
mn_geo <- tigris::states() %>%
  filter(NAME == "Minnesota") %>%
  st_transform(4269)

## minneapolis
minneapolis_geo <- tigris::places("MN") %>%
  filter(NAME == "Minneapolis") %>%
  st_transform(4269)

## 7-county metro
metro_geo <- metro_counties %>%
  st_union()

## greater mn (state minus 7-county metro)
gmn_geo <- st_difference(mn_geo, st_union(st_combine(metro_geo)), dimension = "polygon") %>%
  st_make_valid()

## each implementing agency boundary
ia_cities <- tigris::places("MN") %>%
  filter(NAME %in% c("Minneapolis", "St. Paul", "Bloomington")) %>%
  st_transform(4269) %>%
  mutate(agency_name = case_when(
    NAME == "Minneapolis" ~ "Minneapolis Park & Rec Board",
    NAME == "Bloomington" ~ "Bloomington Parks and Recreation",
    TRUE ~ "St. Paul Parks and Recreation"
  ))

trpd <- metro_counties %>%
  filter(NAME == "Hennepin") %>%
  st_difference(st_union(ia_cities), dimension = "polygon") %>%
  st_make_valid() %>%
  smoothr::drop_crumbs(1000) %>%
  mutate(agency_name = "Three Rivers Park District")

ramsey <- metro_counties %>%
  filter(NAME == "Ramsey") %>%
  st_difference(st_union(ia_cities), dimension = "polygon") %>%
  st_make_valid() %>%
  smoothr::drop_crumbs(1000) %>%
  mutate(agency_name = "Ramsey County Parks and Recreation")


ia_geo <- metro_counties %>%
  filter(!NAME %in% c("Hennepin", "Ramsey")) %>%
  mutate(agency_name = case_when(
    NAME == "Anoka" ~ "Anoka County Parks",
    NAME == "Carver" ~ "Carver County Parks and Recreation",
    NAME == "Dakota" ~ "Dakota County Parks",
    NAME == "Scott" ~ "Scott County Parks And Trails",
    TRUE ~ "Washington County Parks"
  )) %>%
  bind_rows(trpd, ramsey, ia_cities) %>%
  select(agency_name, geometry)

## suburban Hennepin County (all of Hennepin minus Minneapolis, but including Bloomington)
hennepin_geo <- tigris::counties(state = "MN") %>%
  st_transform(4269) %>%
  filter(NAME == "Hennepin")
suburban_hennepin_geo <- st_difference(hennepin_geo, st_union(st_combine(minneapolis_geo)), dimension = "polygon") %>%
  st_make_valid()

##### assign block groups to geographies #####
## block groups don't nest perfectly, so get proportion of bg in each geography to make sure things sum to 100%

## block groups DO nest into the state perfectly
mn_bgs <- tigris::block_groups("MN", cb = TRUE) %>%
  mutate(bg_area = st_area(.))

## get area of each block group
mn_bg_area <- mn_bgs %>%
  st_drop_geometry() %>%
  select(GEOID, bg_area)

## all non-mn block groups
out_of_state_bgs <- theo_home_bg %>%
  filter(!bg_id %in% mn_bgs$GEOID) %>%
  select(GEOID = bg_id) %>%
  mutate(region = "Out of state") %>%
  st_drop_geometry() %>%
  mutate(bg_in_region = 1)

## in-state block groups
in_state_bgs <- mn_bgs %>%
  select(GEOID) %>%
  mutate(region = "Minnesota") %>%
  st_drop_geometry() %>%
  mutate(bg_in_region = 1)

## 7-county metro block groups
metro_bgs <- st_intersection(mn_bgs, metro_geo) %>%
  st_make_valid() %>%
  # calculate percent of block group that falls within region
  mutate(bg_in_region = as.numeric(st_area(.) / bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "7-County Metropolitan Region") %>%
  st_drop_geometry()

## greater mn block groups
gmn_bgs <- st_intersection(mn_bgs, gmn_geo) %>%
  st_make_valid() %>%
  # calculate percent of block group that falls within region
  mutate(bg_in_region = as.numeric(st_area(.) / bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Greater Minnesota (outside of 7-County Metro)") %>%
  st_drop_geometry()

## minneapolis block groups
minneapolis_bgs <- st_intersection(mn_bgs, minneapolis_geo) %>%
  st_make_valid() %>%
  # calculate percent of block group that falls within region
  mutate(bg_in_region = as.numeric(st_area(.) / bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Minneapolis") %>%
  st_drop_geometry()

## suburban hennepin block groups
suburban_hennepin_bgs <- st_intersection(mn_bgs, suburban_hennepin_geo) %>%
  st_make_valid() %>%
  # calculate percent of block group that falls within region
  mutate(bg_in_region = as.numeric(st_area(.) / bg_area)) %>%
  select(GEOID, bg_in_region) %>%
  mutate(region = "Suburban Hennepin County (Hennepin County minus Minneapolis, including Bloomington)") %>%
  st_drop_geometry()

## implementing agency block groups (this is where finding the proportions has the biggest impact)
ia_bgs <- st_intersection(mn_bgs, ia_geo) %>%
  st_make_valid() %>%
  # calculate percent of block group that falls within region
  mutate(bg_in_region = as.numeric(st_area(.) / bg_area)) %>%
  select(region = agency_name, GEOID, bg_in_region) %>%
  st_drop_geometry()

##### combine results #####
all_bgs <- bind_rows(
  out_of_state_bgs,
  in_state_bgs,
  metro_bgs,
  gmn_bgs,
  minneapolis_bgs,
  suburban_hennepin_bgs,
  ia_bgs
) %>%
  left_join(theo_home_bg %>%
    st_drop_geometry() %>%
    rename(GEOID = bg_id)) %>%
  filter(!is.na(p_metro_25)) %>%
  replace_na(list(bg_in_region = 0)) %>%
  # scale visits from block group by percent of block group in region
  mutate(pct_visits_from_region = bg_in_region * p_metro_25) %>%
  group_by(region) %>%
  # summarise total % visitation from region
  summarise(visitation_from_region = sum(pct_visits_from_region))

##### distribute estimated annual visits based on % from home location #####
## we did not publicly share estimated counts by home location, so this is the most reproducible approach
load(file.path(here(), "data-intermediate/processed/park-volume.rda"))
theo_21_visitation <- annual_volume %>%
  filter(year == 2021, zone_name == "Theodore-Wirth_park_Metro-Regional_MPRB")

## format results
results <- all_bgs %>%
  transmute(
    Region = region,
    # distribute annual visits by % visitation from region
    `Estimated visits` = round(visitation_from_region * theo_21_visitation$annual_total),
    `Percent of visits` = 100 * visitation_from_region
  ) %>%
  mutate(Region = factor(Region, levels = c(
    "Minnesota", "Out of state",
    "7-County Metropolitan Region", "Greater Minnesota (outside of 7-County Metro)",
    "Suburban Hennepin County (Hennepin County minus Minneapolis, including Bloomington)",
    "Minneapolis",
    sort(ia_geo$agency_name)
  ))) %>%
  arrange(Region)

##### confirm regions sum correctly #####
## in state + out of state = 100
sum(filter(results, Region %in% c("Minnesota", "Out of state"))$`Percent of visits`)

## metro + greater mn = in state
sum(filter(results, Region %in% c("Greater Minnesota (outside of 7-County Metro)", "7-County Metropolitan Region"))$`Percent of visits`) # 98.845
filter(results, Region == "Minnesota")$`Percent of visits` # 98.846

## all implementing agencies == metro
sum(filter(results, Region %in% ia_geo$agency_name)$`Percent of visits`) # 95.684
filter(results, Region == "7-County Metropolitan Region")$`Percent of visits` # 95.686

##### create output excel workbook ######
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

saveWorkbook(wb, file.path(here::here(), "follow_up_requests/trpd-wirth-home-locations/theodore_wirth_visitor_home_locations.xlsx"),
  overwrite = TRUE
)
