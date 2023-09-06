## This script takes the cleaned park use estimates `data-intermediate/processed/park-volume.rda` to
## create the final excel workbook to be published on the project site. The workbook is saved to `data-processed`.

load(file.path(here(), "data-intermediate/processed/hourly-parks.rda"))
load(file.path(here(), "data-intermediate/processed/park-volume.rda"))


metadata_sheet <- tibble("Metadata" = c(
  "This dataset contains information about visitation to Minnesota's state and regional parks as estimated via location based services data (LBS). LBS data was provided by StreetLight Data, Inc. and accessed in July 2023. LBS data is an aggregated and anonymized data source providing information about when and where people travel. Users are advised that LBS data provided by StreetLight uses a sample based approach, and thus all numbers provided are estimates. More information about StreetLight's methodology can be found on their website [https://www.streetlightdata.com/].",
  "Weekly-level analyses were run for each park and the data has been aggregated into different time periods within this excel sheet for ease of use. These visitation estimates can be used to understand temporal visitation dynamics within and across parks. Persons arriving via vehicle, by bike, and on foot (pedestrian) are included in these estimates. Person per vehicle (PPV) multipliers were necessary to convert vehicle counts into meaningful visitor counts. PPV multipliers used for Metro Regional Parks were specific to the park implementing agency and reported in the 2021 Parks and Trails Visitor Study [https://metrocouncil.org/Parks/Research/Visitor-Study.aspx]. The PPV multiplier used for DNR State Parks was 3.2 as reported in the 2007 visitor survey [https://files.dnr.state.mn.us/aboutdnr/reports/parks/2017_state_parks_visitor_survey.pdf]. The PPV mulitplier used for Greater MN Regional Parks was 2.46 - the average of suburban Metro Regional implementing agencies and the State Park mulipliers. More information about the data and results from this project can be found at [https://minnesota-parks-and-trails-metrocouncil.hub.arcgis.com].",
  "This project was funded with Legacy Partnership Research Funds from the State of Minnesota Park and Trails Legacy Fund.",
  "",
  "Sheet and column descriptions:",
  "",
  "annual_summary sheet: this sheet contains data aggregated by year from January 1 - December 31 for years 2019, 2020, and 2021.",
  "system - indicates if the unit belongs to the DNR State, a Greater MN Regional, or Metro Regional system",
  "unit_type - indicates if the unit is a park or trail",
  "unit_mgmt - indicates the unit's management structure; either DNR area for state parks, Greater MN District, or Metropolitan Regional Implementing Agency",
  "unit_name - indicates the name of the unit",
  "unit_id - unique identifier for each unit (concatenated unit name, type, system, mgmt)",
  "zone_id - unique identifier for each zone used for LBS analyses. For parks, zone_id is identical to unit_id",
  "year - year of estimate",
  "shp_id - unique ID used to identify unit in accompanying shapefiles of visitor home locations",
  "annual_vehicle_count - estimated number of vehicles entering the unit",
  "vehicle_multiplier - person per vehicle multiplier",
  "annual_visitors_vehicle - estimated number of visitors arriving via vechicle (vehicle count * vehicle multiplier)",
  "annual_visitors_bike - estimated number of visitors arriving via bike",
  "annual_visitors_pedestrian - estimated number of visitors arriving via foot",
  "annual_visitors_total - total estimated number of visitors (visitors_vehicle + visitors_bike + visitors_pedestrian)",
  "annual_share_vehicle - the percent of total visitors arriving via vehicle (visitors_vehicle / visitors_total)",
  "annual_share_bike - the percent of total visitors arriving via bike (visitors_bike / visitors_total)",
  "annual_share_pedestrian - the percent of total visitors arriving via foot (visitors_pedestrian / visitors_total)",
  "",
  "monthly_summary sheet: this sheet contains data aggregated by month from January 2019 to April 2022",
  "month - month of estimate",
  "all other columns are as in annual_summary sheet",
  "",
  "weekly_data sheet: this sheet contains data aggregated by week from January 1, 2019 to April 25, 2022",
  "week_start - starting date of the week of estimate",
  "week_end - ending date of the week of estimate",
  "all other columns are as in annual_summary sheet",
  "",
  "hour_summary sheet: this sheet contains summarized hourly visitation data for the summer of 2021 (June 1 - August 31, 2021). All modes of travel are combined in this summary. Vehicle multipliers were used when calculating hourly totals and percentages.",
  "day_type - indicates weekday, weekend, or overall average",
  "5:00_percent - estimated percentage of daily visitation that occurs during the 5:00am-6:00am hour. Additional columns following this naming convention contain estimates for each hour of the day. Hours are denoted using a 24-hour clock."
))

annual_summary <- annual_volume %>%
  full_join(park_metadata) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = district,
    unit_name = unit_label,
    unit_id = unit_id,
    zone_id = zone_name,
    year = year,
    shp_id = shp_id,
    annual_vehicle_count = annual_vehicle_visitors,
    vehicle_multiplier = vehicle_multiplier,
    annual_visitors_vehicle = NA,
    annual_visitors_bike = annual_bike,
    annual_visitors_pedestrian = annual_ped,
    annual_visitors_total = NA,
    annual_share_vehicle = NA,
    annual_share_bike = NA,
    annual_share_pedestrian = NA
  ) %>%
  arrange(system, unit_mgmt, zone_id)


monthly_summary <- monthly_volume %>%
  full_join(park_metadata) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = district,
    unit_name = unit_label,
    unit_id = zone_name,
    zone_id = zone_name,
    year = year,
    shp_id = shp_id,
    month = month,
    monthly_vehicle_count = monthly_vehicle_visitors,
    vehicle_multiplier = vehicle_multiplier,
    monthly_visitors_vehicle = NA,
    monthly_visitors_bike = monthly_bike,
    monthly_visitors_pedestrian = monthly_ped,
    monthly_visitors_total = NA,
    monthly_share_vehicle = NA,
    monthly_share_bike = NA,
    monthly_share_pedestrian = NA
  ) %>%
  arrange(system, unit_mgmt, zone_id)

weekly_data <- weekly_volume %>%
  full_join(park_metadata) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = district,
    unit_name = unit_label,
    unit_id = zone_name,
    zone_id = zone_name,
    year = year,
    shp_id = shp_id,
    week_start = start_date,
    week_end = end_date,
    weekly_vehicle_count = weekly_vehicle_visitors,
    vehicle_multiplier = vehicle_multiplier,
    weekly_visitors_vehicle = NA,
    weekly_visitors_bike = weekly_bike,
    weekly_visitors_pedestrian = weekly_ped,
    weekly_visitors_total = NA,
    weekly_share_vehicle = NA,
    weekly_share_bike = NA,
    weekly_share_pedestrian = NA
  ) %>%
  arrange(system, unit_mgmt, zone_id)

hour_summary <- hourly_parks %>%
  select(zone_name, day_type, day_part, total) %>%
  left_join(park_metadata) %>%
  filter(!is.na(system)) %>%
  ungroup() %>%
  transmute(
    system = system_label,
    unit_type = unit_type,
    unit_mgmt = district,
    unit_name = unit_label,
    unit_id = zone_name,
    zone_id = zone_name,
    shp_id = shp_id,
    day_type = day_type,
    time = paste0(word(day_part, sep = " ", 2), "_percent"),
    hourly_visitors_total = total
  ) %>%
  arrange(system, unit_mgmt, zone_id) %>%
  group_by(day_type, unit_id) %>%
  mutate(hourly_percent = hourly_visitors_total / sum(hourly_visitors_total)) %>%
  select(-hourly_visitors_total) %>%
  group_by(day_type) %>%
  pivot_wider(names_from = time, values_from = hourly_percent) %>%
  mutate(day_type = case_when(
    str_detect(day_type, "All") ~ "Average",
    str_detect(day_type, "Weekday") ~ "Weekday",
    TRUE ~ "Weekend"
  ))


## Create a new workbook
wb <- createWorkbook()

## function formulas
alphabet <- c("abcdefghijklmnop")
park_vm_calc <- function(.df, .df2) {
  # visitors_vehicle
  writeFormula(wb,
    sheet = .df,
    x = paste(
      paste0(
        "(",
        str_sub(alphabet, which(str_detect(colnames(.df2), "vehicle_count")), which(str_detect(colnames(.df2), "vehicle_count"))),
        1:nrow(.df2) + 1L,
        " * ",
        str_sub(alphabet, which(str_detect(colnames(.df2), "vehicle_multiplier")), which(str_detect(colnames(.df2), "vehicle_multiplier"))),
        1:nrow(.df2) + 1L, ")"
      ),
      sep = " + "
    ),
    startCol = which(str_detect(colnames(.df2), "visitors_vehicle")),
    startRow = 2,
    array = F
  )

  # visitors_total
  writeFormula(wb,
    sheet = .df,
    x = paste(
      paste0(
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_vehicle")), which(str_detect(colnames(.df2), "visitors_vehicle"))),
        1:nrow(.df2) + 1L
      ),
      paste0(
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_bike")), which(str_detect(colnames(.df2), "visitors_bike"))),
        1:nrow(.df2) + 1L
      ),
      paste0(
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_pedestrian")), which(str_detect(colnames(.df2), "visitors_pedestrian"))),
        1:nrow(.df2) + 1L
      ),
      sep = " + "
    ),
    startCol = which(str_detect(colnames(.df2), "visitors_total")),
    startRow = 2,
    array = F
  )
  # share_vehicle
  writeFormula(wb,
    sheet = .df,
    x = paste(
      paste0(
        "(",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_vehicle")), which(str_detect(colnames(.df2), "visitors_vehicle"))),
        1:nrow(.df2) + 1L,
        "/ ",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_total")), which(str_detect(colnames(.df2), "visitors_total"))),
        1:nrow(.df2) + 1L, ")"
      ),
      sep = " + "
    ),
    startCol = which(str_detect(colnames(.df2), "share_vehicle")),
    startRow = 2,
    array = F
  )

  # share_bike
  writeFormula(wb,
    sheet = .df,
    x = paste(
      paste0(
        "(",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_bike")), which(str_detect(colnames(.df2), "visitors_bike"))),
        1:nrow(.df2) + 1L,
        "/ ",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_total")), which(str_detect(colnames(.df2), "visitors_total"))),
        1:nrow(.df2) + 1L, ")"
      ),
      sep = " + "
    ),
    startCol = which(str_detect(colnames(.df2), "share_bike")),
    startRow = 2,
    array = F
  )

  # share_pedestrian
  writeFormula(wb,
    sheet = .df,
    x = paste(
      paste0(
        "(",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_pedestrian")), which(str_detect(colnames(.df2), "visitors_pedestrian"))),
        1:nrow(.df2) + 1L,
        "/ ",
        str_sub(alphabet, which(str_detect(colnames(.df2), "visitors_total")), which(str_detect(colnames(.df2), "visitors_total"))),
        1:nrow(.df2) + 1L, ")"
      ),
      sep = " + "
    ),
    startCol = which(str_detect(colnames(.df2), "share_pedestrian")),
    startRow = 2,
    array = F
  )
  setColWidths(wb, sheet = 2, cols = c(1:ncol(.df2)), widths = "auto")
}

## Add and construct metadata
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

## Add and construct annual summary
addWorksheet(wb, "annual_summary")
writeData(wb, sheet = 2, x = annual_summary)
park_vm_calc("annual_summary", annual_summary)
setColWidths(wb, sheet = 2, cols = c(1:ncol(annual_summary)), widths = "auto")

# monthly
addWorksheet(wb, "monthly_summary")
writeData(wb, sheet = 3, x = monthly_summary)
park_vm_calc("monthly_summary", monthly_summary)
setColWidths(wb, sheet = 3, cols = c(1:ncol(monthly_summary)), widths = "auto")

# weekly
addWorksheet(wb, "weekly_data")
writeData(wb, sheet = 4, x = weekly_data)
park_vm_calc("weekly_data", weekly_data)
setColWidths(wb, sheet = 4, cols = c(1:ncol(weekly_data)), widths = "auto")

# hourly
addWorksheet(wb, "hourly_summary")
writeData(wb, sheet = 5, x = hour_summary)
setColWidths(wb, sheet = 5, cols = c(1:ncol(hour_summary)), widths = "auto")


## Save workbook
saveWorkbook(wb, file.path(here(), "data-processed/LBS_park_results_2023.07.xlsx"),
  overwrite = TRUE
)
