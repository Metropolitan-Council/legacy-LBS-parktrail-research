## This script takes the cleaned park use estimates `data-intermediate/processed/trail-volume.rda` to
## create the final excel workbook to be published on the project site. The workbook is saved to `data-processed`.

load(file.path(here(), "data-intermediate/processed/trail-volume.rda"))
load(file.path(here(), "data-intermediate/trails/trail-metadata.rda"))

metadata_sheet <- tibble(
  "Metadata" = c(
    "This dataset contains information about visitation to Minnesota's state and regional trails as estimated via location based services data (LBS). LBS data was provided by StreetLight Data, Inc. and accessed in July 2023. LBS data is an aggregated and anonymized data source providing information about when and where people travel. Users are advised that LBS data provided by StreetLight uses a sample based approach, and thus all numbers provided are estimates. More information about StreetLight's methodology can be found on their website [https://www.streetlightdata.com/].",
    "For each trail, monthly-level analyses were run, and the data has been aggregated into different time periods within this excel sheet for ease of use. These visitation estimates can be used to understand temporal visitation dynamics within and across trails. More information about the data and results from this project can be found at [https://minnesota-parks-and-trails-metrocouncil.hub.arcgis.com].",
    "This project was funded with Legacy Partnership Research Funds from the State of Minnesota Park and Trails Legacy Fund.",
    "",
    "annual_summary sheet: this sheet contains data aggregated at the trail level by year from January 1 - December 31 for years 2019, 2020, and 2021.",
    "system - indicates if the unit belongs to the DNR State, Greater MN Regional, or Metro Regional system",
    "unit_type - indicates if the unit is a park or trail",
    "unit_mgmt - indicates the unit's management structure; either DNR area for state trails, Greater MN District, or Metropolitan Regional Implementing Agency",
    "unit_id - unique identifier for each unit (concatenated unit name, type, system, mgmt)",
    "unit_label - indicates the name of the unit",
    "year - year of estimate",
    "shp_id - unique ID used to identify unit in accompanying shapefiles of visitor home locations",
    "approx_length_meters - approximate length of trail segment in meters",
    "approx_length_miles - approximate length of trail segment in miles",
    "total_miles_traveled - total estimated miles of use on the trail (bike_miles_traveled + ped_miles_traveled)",
    "bike_miles_traveled - total estimated miles of bicycle use on the trail (bike_counter_estimate x approx_length_miles)",
    "ped_miles_traveled - total estimated miles of pedestrian use on the trail (ped_counter_estimate x approx_length_miles)",
    "total_counter_estimate - total estimated trips taken, analogous measurement to a trail counter estimate (bike_counter_estimate + ped_counter_estimate)",
    "bike_counter_estimate - estimated bike trips taken, analogous measurement to a trail counter estimate for bikes (monthly_summary sheets reflects the trail's highest monthly_segment value; annual_summary sheet sums the monthly_summary values)",
    "ped_counter_estimate - estimated pedestrial trips taken, analogous measurement to a trail counter estimate for pedestrians (monthly_summary sheets reflects the trail's highest monthly_segment value; annual_summary sheet sums the monthly_summary values)",
    "modeshare_bike_miles - the percent of miles traveled by bicycles (bike_miles_traveled / total_miles_traveled)",
    "modeshare_ped_miles - the percent of miles traveled by pedestrians (ped_miles_traveled / total_miles_traveled)",
    "",
    "monthly_summary sheet: this sheet contains data aggregated at the trail level by month from January 1, 2019 - April 30, 2023",
    "month - month of estimate",
    "all other columns are as in annual_summary sheet",
    "",
    "monthly_segment_data sheet: this sheet contains data at the trail segment level by month from January 1, 2019 - April 25, 2023",
    "osm_id - ID identifying the OpenStreetMap (OSM) segment geography",
    "segment_id -  unique identifier for each trail segment (concatenated unit name, type, system, mgmt, osm_id)",
    "approx_segment_length_meters - length of individual trail segment in meters",
    "approx_segment_length_miles - length of individual trail segment in miles",
    "all other columns are as in monthly_summary sheet",
    "",
    "hour_summary sheet: this sheet contains trail-level summarized hourly visitation data for the summer of 2021 (June 1 - August 31, 2021). All modes of travel are combined in this summary.",
    "day_type - indicates weekday, weekend, or overall average",
    "5:00_percent - estimated percentage of daily use that occurs during the 5:00am-6:00am hour. Additional columns following this naming convention contain estimates for each hour of the day. Hours are denoted using a 24-hour clock. Empty or NA values indicate that there was insufficient data coverage to calculate hourly use estimates."
  )
)

# TRAIL LEVEL
annual_summary <- annual_trail_volume %>%
  filter(year != 2022, str_detect(day_type, "All")) %>%
  arrange(system, primary_district, unit_id) %>%
  ungroup() %>%
  # arrange(unit_label) %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = primary_district,
    unit_id = unit_id,
    unit_name = unit_label,
    year = year,
    shp_id = shp_id,
    approx_trail_length_meters = as.numeric(approx_length_meters),
    approx_trail_length_miles = as.numeric(approx_length_miles),
    total_miles_traveled = miles_traveled,
    bike_miles_traveled = bike_miles_traveled,
    ped_miles_traveled = ped_miles_traveled,
    total_counter_estimate = counter_estimate,
    bike_counter_estimate = bike_counter_estimate,
    ped_counter_estimate = ped_counter_estimate,
    modeshare_bike_miles = modeshare_bike_miles,
    modeshare_ped_miles = modeshare_ped_miles
  )

monthly_summary <- monthly_trail_volume %>%
  filter(str_detect(day_type, "All")) %>%
  ungroup() %>%
  arrange(system, primary_district, unit_id, year) %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = primary_district,
    unit_id = unit_id,
    unit_name = unit_label,
    year = year,
    month = month,
    shp_id = shp_id,
    approx_trail_length_meters = approx_length_meters,
    approx_trail_length_miles = approx_length_miles,
    total_miles_traveled = miles_traveled,
    bike_miles_traveled = bike_miles_traveled,
    ped_miles_traveled = ped_miles_traveled,
    total_counter_estimate = counter_estimate,
    bike_counter_estimate = bike_counter_estimate,
    ped_counter_estimate = ped_counter_estimate,
    modeshare_bike_miles = modeshare_bike_miles,
    modeshare_ped_miles = modeshare_ped_miles
  )

# SEGMENT LEVEL
segment_data <- monthly_segment_volume %>%
  left_join(trail_metadata %>% select(unit_id, primary_district, shp_id)) %>%
  filter(str_detect(day_type, "All")) %>%
  arrange(system, primary_district, unit_id, osm_id, year, month) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = primary_district,
    unit_id = unit_id,
    unit_name = unit_label,
    osm_id = osm_id,
    segment_id = zone_name,
    year = year,
    month = month,
    shp_id = shp_id,
    approx_segment_length_meters = as.numeric(approx_length_meters),
    approx_segment_length_miles = as.numeric(approx_length_miles),
    total_miles_traveled = miles_traveled,
    bike_miles_traveled = bike_miles_traveled,
    ped_miles_traveled = ped_miles_traveled,
    total_counter_estimate = counter_estimate,
    bike_counter_estimate = bike_counter_estimate,
    ped_counter_estimate = ped_counter_estimate,
    modeshare_bike_miles = modeshare_bike_miles,
    modeshare_ped_miles = modeshare_ped_miles
  ) %>%
  group_by(unit_id) %>%
  mutate(
    approx_trail_length_meters = sum(approx_segment_length_meters),
    approx_trail_length_miles = sum(approx_segment_length_miles)
  )

hour_summary <- hourly_trails %>%
  select(unit_id, day_type, day_part, sumtotal) %>%
  left_join(trail_metadata) %>%
  filter(!is.na(system)) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = primary_district,
    unit_id = unit_id,
    unit_name = unit_label,
    shp_id = shp_id,
    day_type = day_type,
    time = paste0(str_replace_all(word(day_part, sep = " ", 2), "_", ":"), "_percent"),
    hourly_visitors_total = sumtotal
  ) %>%
  arrange(system, unit_mgmt, unit_id) %>%
  group_by(day_type, unit_id) %>%
  mutate(
    hourly_percent = hourly_visitors_total / sum(hourly_visitors_total),
    hourly_percent = ifelse(is.nan(hourly_percent), NA, hourly_percent)
  ) %>%
  select(-hourly_visitors_total) %>%
  group_by(day_type) %>%
  pivot_wider(names_from = time, values_from = hourly_percent) %>%
  mutate(day_type = case_when(
    str_detect(day_type, "All") ~ "Average",
    str_detect(day_type, "Weekday") ~ "Weekday",
    TRUE ~ "Weekend"
  ))

wb <- createWorkbook()

# metadata
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
setColWidths(wb, sheet = 1, cols = 1, widths = 100)

# annual summary
addWorksheet(wb, "annual_summary")
writeData(wb, sheet = 2, x = annual_summary)
setColWidths(wb, sheet = 2, cols = c(1:ncol(annual_summary)), widths = "auto")

# monthly summary
addWorksheet(wb, "monthly_summary")
writeData(wb, sheet = 3, monthly_summary)
setColWidths(wb, sheet = 3, cols = c(1:ncol(monthly_summary)), widths = "auto")

# monthly segment
addWorksheet(wb, "monthly_segment_data")
writeData(wb, sheet = 4, segment_data)
setColWidths(wb, sheet = 4, cols = c(1:ncol(segment_data)), widths = "auto")

# hourly
addWorksheet(wb, "hourly_summary")
writeData(wb, sheet = 5, x = hour_summary)
setColWidths(wb, sheet = 5, cols = c(1:ncol(hour_summary)), widths = "auto")


saveWorkbook(wb, file.path(here(), "data-processed/LBS_trail_results_2023.07.xlsx"),
  overwrite = TRUE
)
