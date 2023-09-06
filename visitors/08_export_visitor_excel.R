## This script takes the cleaned visitor attribute estimates (home locations & demographics) to
## create the final excel workbook to be published on the project site. The workbook is saved to `data-processed`.

load(file.path(here(), "data-intermediate/processed/all-processed-demographics.rda"))
load(file.path(here(), "data-intermediate/processed/processed-homes.rda"))

metadata_sheet <- tibble(
  "Metadata" = c(
    "This dataset contains information about visitors to Minnesota's state and regional parks trails as inferred via location based services data (LBS). LBS data was provided by StreetLight Data, Inc. and accessed in July 2023. LBS data is an aggregated and anonymized data source providing information about when and where people travel. Users are advised that LBS data provided by StreetLight uses a sample based approach, and thus all numbers provided are estimates. More information about StreetLight's methodology can be found on their website [https://www.streetlightdata.com/].",
    "For each park and trail, LBS analyses were run for the calendar year 2021 (January 1 - December 31, 2021). Demographic data is inferred based on inferred visitor home locations and 2020 US Census data. Data was validated against intercept survey data conducted during the same period. Users are advised that LBS-based demographic data is inferred and that local, unit-level context should be considered when analyzing demographic data. More information about the data and results from this project can be found at [https://minnesota-parks-and-trails-metrocouncil.hub.arcgis.com]",
    "This project was funded with Legacy Partnership Research Funds from the State of Minnesota Park and Trails Legacy Fund.",
    "",
    "inferred_demographics sheet: This sheet contains data on inferred visitor race and ethnicity characteristics during 2021. Race and ethnicity categories are defined by the US Census; definitions provided below. For parks, vehicle multipliers were used when calculating percentages.",
    "system - indicates if the unit belongs to the DNR State, Greater MN Regional, or Metro Regional system",
    "unit_type - indicates if the unit is a park or trail",
    "unit_mgmt - indicates the unit's management structure; either DNR area for state parks, Greater MN District, or Metropolitan Regional Implementing Agency",
    "unit_name - indicates the name of the unit",
    "unit_id - unique identifier for each unit (concatenated unit name, type, system, mgmt)",
    "year - year of estimate",
    "shp_id - unique ID used to identify unit in accompanying shapefiles of visitor home locations",
    "",
    "race and ethnicity groups (US Census Bureau definitions)",
    "american_indian - the percent of visitors inferred to be American Indian or Alaska Native (A person having origins in any of the original peoples of North and South America (including Central America) and who maintains tribal affiliation or community attachment)",
    "asian - the percent of visitors inferred to be Asian (A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent including, for example, Cambodia, China, India, Japan, Korea, Malaysia, Pakistan, the Philippine Islands, Thailand, and Vietnam)",
    "black - the percent of visitors inferred to be Black or African American (A person having origins in any of the Black racial groups of Africa)",
    "hispanic_or_latinx - the percent of visitors inferred to be Hispanic or Latino (A person of Cuban, Mexican, Puerto Rican, South or Central American, or other Spanish culture or origin regardless of race)",
    "multiple_races - the percent of visitors inferred to be two or more races (A person who provided multiple responses on the census questionairre)",
    "native_hawaiian_and_pacific_islander - the percent of visitors inferred to be Native Hawaiian or Other Pacific Islander (A person having origins in any of the original peoples of Hawaii, Guam, Samoa, or other Pacific Islands)",
    "some_other_race - the percent of visitors inferred to be Some Other Race (A person who self-reported a race not listed on the census questionairre)",
    "white - the percent of visitors inferred to be White (A person having origins in any of the original peoples of Europe, the Middle East, or North Africa)",
    "",
    "educational attainment groups",
    "high_school - the percent of visitors inferred to be high school graduates, current high school students, or to have no high school completion",
    "some_college - the percent of visitors inferred to have an associate degree, some post-secondary education, or to be a current college student,",
    "degree_4_year - the percent of visitors inferred to have a 4-year degree",
    "degree_graduate_professional - the percent of visitors inferred to have a graduate or professional degree",
    "",
    "income groups",
    "inc_less_25k - the percent of visitors inferred to have a household income of less than $25,000",
    "inc_25k_to_40k - the percent of visitors inferred to have a household income of $25,000 - 39,999",
    "inc_40k_to_60k - the percent of visitors inferred to have a household income of $40,000 - 59,999",
    "inc_60k_to_75k - the percent of visitors inferred to have a household income of $60,000 - 74,999",
    "inc_75k_to_100k - the percent of visitors inferred to have a household income of $75,000 - 99,999",
    "inc_100k_to_150k - the percent of visitors inferred to have a household income of $100,000 - 149,999",
    "inc_greater_150k - the percent of visitors inferred to have a household income of $150,000 or higher",
    "",
    "home_location_summary sheet: This sheet contains summarized home location data for each park and trail in the project sample. Data is reported at the state level for the entire 2021 calendar year. For parks, vehicle multipliers were used when calculating percentages. States are ordered by total percent of visitors.",
    "home_state - name of inferred home state",
    "percent_visitors - inferred percent of visitors to given park or trail with a home location in home_state",
    "all other columns are as in inferred_demographics sheet"
  )
)


inferred_demographics <- stl_dem %>%
  filter(
    label %in% c("year21", "full21"),
    str_detect(day_type, "All")
  ) %>%
  ungroup() %>%
  select(
    system = system_label,
    unit_type,
    district, primary_district,
    unit_name = unit_label,
    unit_id, shp_id,
    group, percent
  ) %>%
  # group_by(system, unit_type, district, unit_name, unit_id, shp_id)
  tidyr::pivot_wider(values_from = "percent", names_from = "group") %>%
  ungroup() %>%
  transmute(
    system = system,
    unit_type = unit_type,
    unit_mgmt = ifelse(unit_type == "park", district, primary_district),
    unit_name = unit_name,
    unit_id = unit_id,
    shp_id = shp_id,
    american_indian = `American Indian`,
    asian = Asian,
    black = Black,
    hispanic_or_latinx = `Hispanic or Latinx`,
    multiple_races = `More than one race`,
    native_hawaiian_and_pacific_islander = `Native Hawaiian and other Pacific Islander`,
    some_other_race = `Some other race`,
    white = White,
    high_school = `High school`,
    some_college = `Associate degree or some college`,
    degree_4_year = `4-year degree`,
    degree_graduate_professional = `Graduate or professional degree`,
    inc_less_25k = `Less than $25,000`,
    inc_25k_to_40k = `$25,000 - 39,999`,
    inc_40k_to_60k = `$40,000 - 59,999`,
    inc_60k_to_75k = `$60,000 - 74/79,999`,
    inc_75k_to_100k = `$75/80,000 - 99,999`,
    inc_100k_to_150k = `$100,000 - 149,999`,
    inc_greater_150k = `$150,000 or higher`
  ) %>%
  arrange(unit_type, system, unit_mgmt, unit_name)


home_location_summary <- home_states %>%
  filter(
    label %in% c("year21", "full21"),
    str_detect(day_type, "All")
  ) %>%
  transmute(
    system = system,
    unit_type = unit_type,
    unit_mgmt = ifelse(unit_type == "park", district, primary_district),
    unit_name = unit_label,
    unit_id = unit_id,
    shp_id = shp_id,
    home_state = state_name,
    percent_visitors = percent_by_state
  ) %>%
  arrange(unit_type, system, unit_mgmt, unit_name, -percent_visitors)

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

addWorksheet(wb, "inferred_demographics")
writeData(wb, sheet = 2, x = inferred_demographics)
setColWidths(wb, sheet = 2, cols = c(1:ncol(inferred_demographics)), widths = "auto")

addWorksheet(wb, "home_location_summary")
writeData(wb, sheet = 3, x = home_location_summary)
setColWidths(wb, sheet = 3, cols = c(1:ncol(home_location_summary)), widths = "auto")

saveWorkbook(wb, file.path(here::here(), "data-processed/LBS_visitor_attribute_results_2023.07.xlsx"),
  overwrite = TRUE
)


##### export just metadata #####
## create a separate sheet containing shp_id and other metadata for use with home location shapefiles

unit_metadata <- home_states %>%
  filter(
    label %in% c("year21", "full21"),
    str_detect(day_type, "All")
  ) %>%
  transmute(
    system = system,
    unit_type = unit_type,
    unit_mgmt = ifelse(unit_type == "park", district, primary_district),
    unit_name = unit_label,
    unit_id = unit_id,
    shp_id = shp_id
  ) %>%
  unique() %>%
  arrange(unit_type, system, unit_mgmt, unit_name)

wb2 <- createWorkbook()

addWorksheet(wb2, "unit_metadata")
writeData(wb2, sheet = 1, x = unit_metadata)
addStyle(wb2,
  sheet = "unit_metadata",
  style = openxlsx::createStyle(wrapText = TRUE),
  rows = 2:nrow(unit_metadata),
  cols = 1,
  gridExpand = F,
  stack = TRUE
)
setColWidths(wb2, sheet = 1, cols = 1)

saveWorkbook(wb2, file.path(here::here(), "data-processed/unit_metadata_2023.07.xlsx"),
  overwrite = TRUE
)
