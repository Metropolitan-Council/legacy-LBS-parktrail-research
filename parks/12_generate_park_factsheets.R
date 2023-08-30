## This script uses several functions (`R/_park_factsheet_functions.R`) to generate a 1-page PDF
## factsheet for each park in the project sample.

### load functions ###
source(file.path(here::here(), "R/_park_factsheet_functions.R"))
source(file.path(here::here(), "R/_load_packages.R"))
source(file.path(here::here(), "R/_set_aesthetics.R"))


### load data needed for functions ###
## hourly
load(file.path(here(), "data-intermediate/processed/hourly-parks.rda"))
park_metadata <- readRDS(file.path(here(), "data-intermediate/parks/parks-metadata.RDS"))


## demographics
load(file.path(here(), "data-intermediate/visitors/census-comp-pops.rda"))
load(file.path(here(), "data-intermediate/processed/all-processed-demographics.rda"))

# annual_volume, monthly_volume, season_volume, and weekly_volume
load(file.path(here::here(), "data-intermediate/processed/park-volume.rda"))

state_demos <- data.frame(bind_rows(state_race, state_income, state_education))
greater_mn_demos <- data.frame(bind_rows(greater_mn_race, greater_mn_income, greater_mn_education))
metro_demos <- data.frame(bind_rows(metro_race, metro_income, metro_education))

## home locations
load(file.path(here(), "data-intermediate/processed/processed-homes.rda"))

## park locations
locations <- read_sf(file.path(here(), "data-processed/zone-sets/park-polygons/park-polygon-zone-set-2023.07.02.shp"))

## other geographic data
mnoutline <- tigris::states(cb = T, progress_bar = F) %>%
  filter(NAME == "Minnesota")

options(tigris_use_cache = TRUE)
zips <- tigris::zctas(state = "MN", year = 2010)
bgs <- tigris::block_groups(state = "MN", cb = TRUE)
data(fips_codes)
state_abbr <- fips_codes %>%
  select(state_code, state_name) %>%
  unique() %>%
  as_tibble()

## fonts
sysfonts::font_add(family = "Avenir", regular = "/System/Library/Fonts/Avenir.ttc")
showtext::showtext_auto()

## FUNCTION TO CREATE UNIT FIG
create_unit_fig <- function(name) {
  dat_annual <- annual_volume %>%
    filter(zone_name == name)
  dat_monthly <- monthly_volume %>%
    filter(zone_name == name)
  dat_weekly <- weekly_volume %>%
    filter(zone_name == name)
  dat_hourly <- hourly_parks %>%
    filter(zone_name == name)
  meta <- park_metadata %>%
    filter(zone_name == name)
  demo <- stl_dem %>%
    filter(zone_name == name & str_detect(day_type, "All"))


  # title
  title <- textbox_grob(paste(meta$long_unit_label),
    gp = gpar(fontfamily = "Avenir", fontsize = 40, fontface = "bold", lineheight = 0.6),
    margin = unit(c(1, 1, 2, 2), "cm")
  )

  # paragraph directly under title with basic info
  explain_text <- paste(
    "This factsheet summarizes park-level data for", meta$unit_label, paste0(meta$unit_type_label, "."),
    "Use estimates were derived using location-based services (LBS) data and represent the number of visitors arriving to the park by vehicle, bike, or foot.",
    "A vehicle multiplier of", meta$vehicle_multiplier, "was used to convert vehicle counts to visitor counts for all",
    ifelse(meta$system == "Metro Regional", meta$long_agency, meta$system_label), "parks.",
    # "Over the study period, visitation may have been affected by the COVID-19 pandemic, poor air quality caused by wildfires in 2021, or park-specific events.",
    ifelse(meta$system == "Metro Regional Inclusive", 'This "inclusive" park boundary contains roads which would typically be excluded from analysis and may be helpful for interpreting use at parks with significant activity on parkways.', ""),
    "\n\nThis research project was funded with Legacy Partnership Research Funds from the State of Minnesota Parks and Trails Legacy Fund. LBS data was obtained from StreetLight Data, Inc. and was accessed in July 2023."
  ) # More information is available on the <a href = 'https://minnesota-parks-and-trails-metrocouncil.hub.arcgis.com/'> interactive project website</a>.
  # "\n\nLBS data was obtained from StreetLight Data, Inc. and was accessed in July 2023. This project was funded with Legacy Partnership Research Funds from the State of Minnesota Parks and Trails Legacy Fund. Interactive versions of these figures are available on the final project website.")

  explainer <- textbox_grob(explain_text,
    use_markdown = T,
    gp = gpar(fontfamily = "Avenir", fontsize = 14, lineheight = 1),
    margin = unit(c(0.5, 1, 1, 0), "cm")
  )
  # explainer <- ggpubr::text_grob(explain_text)

  # annual bar chart
  print("bars")
  bars <- dat_annual %>%
    ungroup() %>%
    annual_bar_chart_fxn() + # annotate(geom = "text", x= 1000, y = 8000000, label = "<a href = 'http://google.com'>test hyperlink</a>") +
    theme(plot.margin = unit(c(1, 0, 0, 0), "cm"))

  annual_table <- dat_annual %>%
    ungroup() %>%
    annual_table_fxn()

  # map of location (with inset of state)
  print("location map")
  loc_map <- location_map_fxn(name)

  # weekly timeseries
  print("weekly")
  week_ts <- dat_weekly %>%
    weekly_timeseries_plot_fxn()

  # mode share
  print("season modeshare")

  season_mode <- dat_monthly %>%
    seasonal_mode_share_fxn()

  # hourly
  print("hourly")
  hour_ts <- dat_hourly %>%
    hourly_timeseries_plot_fxn()

  # home location map (with zoomed in inset)
  print("home map")
  home_map <- home_map_fxn(name)

  # demographics
  print("demos")
  # demo_title <- demo_table_fxn(demo)$demo_title
  # demo_explainer <- demo_table_fxn(demo)$demo_explainer
  # demo_tab <- demo_table_fxn(demo)$demo_tab

  demo_plot <- demo_table_fxn(demo)
  print("demos done")
  ## PUT IT ALL TOGETHER

  annfigs <- plot_grid(bars, plot_grid(nullGrob(), annual_table, nullGrob(), nrow = 3, ncol = 1, rel_heights = c(.3, 1, .3)), rel_widths = c(1, 1))

  top <- plot_grid(title,
    plot_grid(
      nullGrob(),
      plot_grid(explainer, annfigs, ncol = 1, rel_heights = c(2, 3.5)),
      loc_map,
      nullGrob(),
      nrow = 1, rel_widths = c(0.5, 5, 3, 0.2)
    ),
    ncol = 1, rel_heights = c(1, 5)
  )

  middle <- plot_grid(nullGrob(),
    season_mode, hour_ts,
    nullGrob(),
    nrow = 1, rel_widths = c(0.05, 1, 1, 0.1)
  )

  bottom <- plot_grid(nullGrob(),
    home_map, nullGrob(), demo_plot,
    nullGrob(),
    nrow = 1, rel_widths = c(0.1, 1, 0.1, 1, 0.1)
  )

  figure <- plot_grid(nullGrob(), top,
    plot_grid(nullGrob(), week_ts, nullGrob(), nrow = 1, rel_widths = c(0.05, 1, 0.05)),
    middle, bottom, nullGrob(),
    ncol = 1, rel_heights = c(0.2, 8, 4, 5.5, 7, 0.2)
  )

  if(!file.exists(paste0(here::here(), "/figures/factsheets/", meta$system,
                         "-parks/"))){
    dir.create(paste0(here::here(), "/figures/factsheets/",
                      meta$system,
                      "-parks/"),
               recursive = TRUE)
  }
  if (meta$system == "Metro Regional") {
    save_plot(figure,
      filename = paste0(here::here(), "/figures/factsheets/", meta$system, "-parks/", stringr::str_replace(meta$unit_label, "/", "-"), "_", meta$short_agency, ".pdf"),
      base_height = 23, base_width = 17
    )
  } else {
    save_plot(figure,
      filename = paste0(here::here(), "/figures/factsheets/", meta$system, "-parks/", stringr::str_replace(meta$unit_label, "/", "-"), ".pdf"),
      base_height = 23, base_width = 17
    )
  }
}



##### generate figures #####
purrr::map(.x = c(1:nrow(park_metadata)), .f = function(x) {
  park_name <- park_metadata[x, ]$zone_name
  print(park_name)
  create_unit_fig(park_name)
})
