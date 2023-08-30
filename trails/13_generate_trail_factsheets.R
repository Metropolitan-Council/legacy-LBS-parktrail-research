## This script uses several function (`R/_trail_factsheet_functions.R`) to generate a 1-page PDF
## factsheet for each trail in the project sample.

### load functions ###
source(file.path(here(), "R/_trail_factsheet_functions.R"))
source(file.path(here::here(), "R/_load_packages.R"))
source(file.path(here::here(), "R/_set_aesthetics.R"))

## load data needed for functions ###
load(file.path(here(), "data-intermediate/processed/trail-volume.rda"))
load(file.path(here(), "data-intermediate/trails/trail-metadata.rda"))

## demographics
load(file.path(here(), "data-intermediate/visitors/census-comp-pops.rda"))
load(file.path(here(), "data-intermediate/processed/all-processed-demographics.rda"))

state_demos <- data.frame(bind_rows(state_race, state_income, state_education))
greater_mn_demos <- data.frame(bind_rows(greater_mn_race, greater_mn_income, greater_mn_education))
metro_demos <- data.frame(bind_rows(metro_race, metro_income, metro_education))

stl_dem <- stl_dem %>%
  mutate(group = case_when(
    group == "$60,000 - 74/79,999" ~ "$60,000 - 74,999",
    group == "$75/80,000 - 99,999" ~ "$75,000 - 99,999",
    TRUE ~ group
  ))

## home locations
load(file.path(here(), "data-intermediate/processed/processed-homes.rda"))

## segments
load(file.path(here(), "data-intermediate/trails/cleaned-trail-geography.rda"))

## segments for mapping
ann_seg_vol <- monthly_segment_volume %>%
  filter(
    str_detect(day_type, "All"),
    !str_detect(day_part, "Average"),
    year == 2021
  ) %>%
  group_by(zone_name, osm_id, system, system_label, unit_label, short_agency) %>%
  summarise(total_vol = sum(counter_estimate), .groups = "keep")

cleaned_trail_segments_mapping <- cleaned_trail_segments %>%
  left_join(ann_seg_vol)

## other geographic data
trail_polygons <- readRDS(file.path(here(), "data-intermediate/trails/trail-polygons.RDS"))
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
sysfonts::font_add(family = "Avenir", regular = avenir_location())
showtext::showtext_auto()


# annual_trail_volume %>%
#   filter(str_detect(day_type, "All")) %>%
#   filter(unit_id == "Blue-Mounds_trail_Greater-MN") %>%
#   ungroup() %>%
#   annual_bar_chart_fxn()
#
# annual_trail_volume %>%
#   filter(str_detect(day_type, "All")) %>%
#   filter(str_detect(unit_id, "Blue-M")) %>%
#   ungroup() %>%
#   annual_table_fxn()
#
location_map_fxn("Blue-Ox_trail_DNR")
#
# monthly_trail_volume %>%
#   filter(str_detect(day_type, "All")) %>%
#   filter(str_detect(unit_id, "Blue-Mounds")) %>%
#   ungroup() %>%
#   monthly_timeseries_plot_fxn()
#
# monthly_trail_volume %>%
#   filter(str_detect(day_type, "All")) %>%
#   filter(str_detect(unit_id, "Blue-Mounds")) %>%
#   ungroup() %>%
#   seasonal_mode_share_fxn()
#
# hourly_trails %>%
#   filter(str_detect(unit_id, "Blue-Mounds")) %>%
#   hourly_timeseries_plot_fxn()
#
# stl_dem %>%
#   filter(str_detect(zone_name, "Blue-Mounds")) %>%
#   demo_table_fxn()
#
# home_map_fxn("Blue-Mounds_trail_Greater-MN")


create_unit_fig <- function(name) {
  dat_annual <- annual_trail_volume %>%
    filter(
      unit_id == name,
      str_detect(day_type, "All")
    )

  dat_monthly <- monthly_trail_volume %>%
    filter(
      unit_id == name,
      str_detect(day_type, "All")
    )

  dat_hourly <- hourly_trails %>%
    filter(unit_id == name)

  meta <- trail_metadata %>%
    filter(unit_id == name)

  demo <- stl_dem %>%
    filter(unit_id == name & str_detect(day_type, "All"))


  # title
  title <- textbox_grob(paste(meta$long_unit_label),
                        gp = gpar(fontfamily = "Avenir",
                                  fontsize = 40, fontface = "bold", lineheight = 0.6),
                        margin = unit(c(1, 1, 2, 2), "cm")
  )

  # paragraph directly under title with basic info
  explain_text <- paste(
    "This factsheet summarizes trail-level data for", meta$unit_label, paste0(meta$unit_type_label, "."),
    "Use estimates were derived using location-based services (LBS) data and represent the number miles traveled on trails by bicyclists and pedestrians.",

    # "Over the study period, visitation may have been affected by the COVID-19 pandemic, poor air quality caused by wildfires in 2021, or park-specific events.",
    ifelse(meta$system == "Metro Regional Inclusive", 'This "inclusive" park boundary contains roads which would typically be excluded from analysis and may be helpful for interpreting use at parks with significant activity on parkways.', ""),
    "\n\nThis research project was funded with Legacy Partnership Research Funds from the State of Minnesota Parks and Trails Legacy Fund. LBS data was obtained from StreetLight Data, Inc. and was accessed in May 2023."
  )

  explainer <- textbox_grob(explain_text,
                            use_markdown = T,
                            gp = gpar(fontfamily = "Avenir",
                                      fontsize = 14, lineheight = 1),
                            margin = unit(c(0.5, 1, 1, 0), "cm")
  )

  # annual bar chart
  cli::cli_progress_message("bars")
  bars <- dat_annual %>%
    filter(str_detect(day_type, "All")) %>%
    ungroup() %>%
    annual_bar_chart_fxn() +
    theme(plot.margin = unit(c(1, 0, 0, 0), "cm"))

  annual_table <- dat_annual %>%
    filter(str_detect(day_type, "All")) %>%
    ungroup() %>%
    annual_table_fxn()

  # map of location (with inset of state)
  cli::cli_progress_message("location map")
  location_map_text <- paste(
    meta$unit_label, "is part of the",
    meta$system_label, "Trail system",
    paste0("(", meta$district, ")."),
    "Park boundaries were accessed via",
    ifelse(meta$system == "Greater MN",
           "personal communication in July, 2021",
           "the Minnesota Geospatial Commons in May 2023"),
    "and may have been edited to improve LBS data performance."
  )

  loc_map <- location_map_fxn(name)

  # weekly timeseries
  cli::cli_progress_message("monthly")
  month_ts <- dat_monthly %>%
    monthly_timeseries_plot_fxn()

  # mode share
  cli::cli_progress_message("season modeshare")
  season_mode <- dat_monthly %>%
    seasonal_mode_share_fxn()

  # hourly
  cli::cli_progress_message("hourly")
  hour_ts <- dat_hourly %>%
    hourly_timeseries_plot_fxn()

  # home location map
  cli::cli_progress_message("home map")
  home_map <- home_map_fxn(name)

  # demographics
  print("demos")

  demo_plot <- demo_table_fxn(demo)
  cli::cli_progress_message("demos done")
  ## PUT IT ALL TOGETHER

  annfigs <- plot_grid(bars,
                       plot_grid(nullGrob(),
                                 annual_table,
                                 nullGrob(), nrow = 3, ncol = 1,
                                 rel_heights = c(.3, 1, .3)), rel_widths = c(1, 1))

  top <- plot_grid(title,
                   plot_grid(
                     nullGrob(),
                     plot_grid(explainer, annfigs, ncol = 1,
                               rel_heights = c(2, 3.5)),
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
                      plot_grid(nullGrob(), month_ts, nullGrob(), nrow = 1,
                                rel_widths = c(0.05, 1, 0.05)),
                      middle, bottom, nullGrob(),
                      ncol = 1, rel_heights = c(0.2, 8, 4, 5.5, 7, 0.2)
  )


  # create folder if it doesn't already exist
  if(!file.exists(paste0(here::here(), "/figures/factsheets/", meta$system,
                         "-trails/"))){
    dir.create(paste0(here::here(), "/figures/factsheets/",
                      meta$system,
                      "-trails/"),
               recursive = TRUE)
  }

  if (meta$system == "Metro Regional") {
    save_plot(figure,
              filename = paste0(here::here(), "/figures/factsheets/", meta$system,
                                "-trails/", stringr::str_replace(meta$unit_id, "/", "-"),
                                "_", meta$short_agency, ".pdf"),
              base_height = 23, base_width = 17
    )
  } else {
    save_plot(figure,
              filename = paste0(here::here(), "/figures/factsheets/", meta$system,
                                "-trails/", stringr::str_replace(meta$unit_id, "/", "-"), ".pdf"),
              base_height = 23, base_width = 17
    )
  }

  dev.off()
}

##### generate figures #####
purrr::map(.x = c(1:nrow(trail_metadata)), .f = function(x) {
  trail_name <- trail_metadata[x, ]$unit_id
  cli::cli_alert_info(trail_name)
  create_unit_fig(trail_name)
})
