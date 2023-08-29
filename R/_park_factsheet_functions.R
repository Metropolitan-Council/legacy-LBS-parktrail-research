annual_bar_chart_fxn <- function(x) {
  x %>%
    filter(year != 2022) %>%
    ungroup() %>%
    select(year, annual_bike, annual_ped, annual_vehicle_visitors, annual_total) %>%
    mutate(
      annual_active = annual_bike + annual_ped,
      save_active = annual_active,
      save_vehicle = annual_vehicle_visitors,
      save_total = annual_total
    ) %>%
    rename(
      `Bicycle and pedestrian` = annual_active,
      `Vehicle` = annual_vehicle_visitors
    ) %>%
    pivot_longer(
      cols = c(`Bicycle and pedestrian`, `Vehicle`),
      names_to = "Mode",
      values_to = "Visits"
    ) %>%
    ggplot(aes(x = year, y = Visits, fill = Mode)) +
    geom_col(color = "black") +
    legacy_theme +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
    scale_x_continuous(expand = c(0, 0.1), labels = c("2019", "2020", "2021"), breaks = c(2019, 2020, 2021)) +
    labs(
      x = "", y = "",
      title = "Total annual visits"
    ) +
    scale_fill_manual(values = c(
      "Bicycle and pedestrian" = legacy_green,
      "Vehicle" = legacy_blue
    )) +
    guides(guide = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom") +
    geom_label(aes(
      label = prettyNum(save_vehicle, big.mark = ","),
      y = save_vehicle * 0.5
    ), fill = "white", color = "grey40", size = 3) +
    geom_label(aes(
      label = prettyNum(save_active, big.mark = ","),
      y = save_vehicle + save_active * 0.5
    ), fill = "white", color = "grey20", size = 3) +
    geom_label(
      aes(
        label = prettyNum(save_total, big.mark = ","),
        y = save_total
      ),
      fill = "white", color = "black", size = 5,
      position = position_dodge(width = 0.9), vjust = -0.25
    )
}


annual_table_fxn <- function(x) {
  ann_dat <- x %>%
    filter(year != 2022) %>%
    ungroup() %>%
    select(year, annual_bike, annual_ped, annual_vehicle_visitors, annual_total) %>%
    transmute(
      `Year` = as.character(year),
      `Total visits` = annual_total,
      `Vehicle visitors` = annual_vehicle_visitors,
      `Bicyclists` = annual_bike,
      `Pedestrians` = annual_ped
    )

  ann_tab <- ann_dat %>%
    regulartable() %>%
    theme_vanilla() %>%
    font(part = "all", fontname = "Avenir") %>%
    fontsize(part = "all", size = 14) %>%
    gen_grob()

  ann_plot <- ggplot() +
    geom_point() +
    theme_void() +
    draw_grob(ann_tab) +
    legacy_theme +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank()
    )

  print(ann_plot)
}


location_map_fxn <- function(name) {
  meta <- park_metadata %>%
    filter(zone_name == name)

  park_location <- locations %>%
    filter(zone_id == meta$zone_name) %>%
    smoothr::smooth(method = "chaikin") %>%
    st_transform(4326)

  location_map_text <- paste(
    meta$unit_label, "is part of the", str_remove(meta$system_label, " \\(Inclusive Boundaries\\)"), "Park system. The park is in",
    meta$primary_county, "County", paste0("(", str_remove(meta$district, " \\(Inclusive Boundaries\\)"), ")."),
    "Park boundaries were accessed via", ifelse(meta$system == "Greater MN", "personal communication in July, 2021", "the Minnesota Geospatial Commons in July 2023"),
    "and may have been edited to improve LBS data performance."
  )

  location_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") %>%
    leaflet::addPolygons(
      data = park_location,
      fillOpacity = 1,
      opacity = 1,
      weight = 0,
      fillColor = park_map_color, color = park_map_color
    )

  (location_map %>%
    mapview::mapshot(
      vwidth = 800 / 1.2, vheight = 900 / 1.2,
      file = file.path(getwd(), "data-temp/mapexport", paste0(name, ".png"))
    ))

  loc_file <- file.path(getwd(), "data-temp/mapexport", paste0(name, ".png"))

  loc_map <- ggdraw() +
    draw_image(
      loc_file,
      scale = 1
    )

  loc_map2 <- plot_grid(loc_map) %>%
    add_sub(paste("\n", strwrap(location_map_text, width = 60), collapse = "\n"),
      x = 0, hjust = 0,
      fontfamily = "Avenir", lineheight = 0.5, size = 12
    ) %>%
    ggdraw()

  return(loc_map2)
}

weekly_timeseries_plot_fxn <- function(x) {
  meta <- park_metadata %>%
    filter(zone_name == unique(x$zone_name))
  x %>%
    ggplot(aes(x = start_date, y = weekly_total)) +
    geom_line(col = "black") +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
    scale_x_date(
      date_breaks = "3 months", date_labels = "%b '%y", expand = c(0, 0),
      limits = c(as.Date("2019-01-01"), as.Date("2022-04-26"))
    ) +
    legacy_theme +
    labs(
      x = "", y = "", title = "Weekly visits",
      subtitle = paste0("Each point in this time series indicates estimated weekly visits to ", meta$unit_label, ".")
    ) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linetype = 3),
      plot.margin = unit(c(0.5, 1, 0.5, 0.2), "cm")
    )
}

seasonal_mode_share_fxn <- function(x) {
  meta <- park_metadata %>%
    filter(zone_name == unique(x$zone_name))

  modeshare_text <- "Mode share shows the percent of visitors arriving by vehicle, bicycle, or on foot (pedestrian). Points show the monthly averages across the study period."

  x %>%
    group_by(month) %>%
    mutate(
      vehicle_mode_share = mean(vehicle_mode_share),
      bike_mode_share = mean(bike_mode_share),
      ped_mode_share = mean(ped_mode_share)
    ) %>%
    mutate(
      save_veh = vehicle_mode_share,
      save_bike = bike_mode_share,
      save_ped = ped_mode_share
    ) %>%
    rename(
      Vehicle = vehicle_mode_share,
      Bicycle = bike_mode_share,
      Pedestrian = ped_mode_share
    ) %>%
    pivot_longer(
      cols = c("Vehicle", "Bicycle", "Pedestrian"),
      names_to = "Mode",
      values_to = "Share"
    ) %>%
    mutate(Mode = factor(Mode, levels = c("Vehicle", "Pedestrian", "Bicycle"), ordered = TRUE)) %>%
    ggplot(aes(x = month, y = Share, col = forcats::fct_rev(Mode), pch = forcats::fct_rev(Mode), group = forcats::fct_rev(Mode))) +
    geom_point(size = 4) +
    geom_line(lwd = 1) +
    legacy_theme +
    scale_y_continuous(label = scales::label_percent(), expand = expansion(mult = c(0, 0.025))) +
    labs(
      x = "", fill = "Mode", y = "", title = "Mode share",
      subtitle = paste(strwrap(modeshare_text, width = 80), collapse = "\n"),
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_line(linetype = 3),
      panel.grid.major.x = element_line(linetype = 3),
      legend.key.size = unit(0.5, "cm"),
      plot.margin = unit(c(1, 1, 1, 0.2), "cm")
    ) +
    scale_color_manual(values = c(
      "Vehicle" = legacy_blue,
      "Pedestrian" = legacy_green,
      "Bicycle" = legacy_orange
    )) +
    guides(color = guide_legend(override.aes = list(linetype = 0))) +
    labs(color = "Mode", shape = "Mode")
}

hourly_timeseries_plot_fxn <- function(x) {
  hour_text <- "Hourly use during summer 2021 shows patterns of when visitors use trails on weekdays and weekends."

  x %>%
    mutate(day_type = str_remove_all(day_type, "1: Average |2: Average | Day")) %>%
    filter(!str_detect(day_type, "All")) %>%
    group_by(day_type, time) %>%
    summarise(total = sum(total), .groups = "keep") %>%
    ungroup() %>%
    group_by(day_type) %>%
    mutate(percent = total / sum(total)) %>%
    mutate(
      time2 = strftime(time, format = "%H:%M"),
      label = if_else(time2 == "23:00", day_type, NA_character_)
    ) %>%
    ggplot(aes(x = time, y = percent, color = day_type, group = day_type, pch = day_type)) +
    geom_point(size = 4) +
    geom_line(lwd = 1) +
    legacy_theme +
    scale_color_manual(values = c(
      "Weekday (M-F)" = legacy_blue,
      "Weekend (Sa-Su)" = legacy_green
    )) +
    scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0.01)) +
    scale_x_datetime(date_breaks = "4 hours", date_labels = "%l%p", expand = expansion(mult = c(0.01, 0.01))) +
    labs(
      x = "", color = "", shape = "", y = "", title = "Hourly use",
      subtitle = paste(strwrap(hour_text, width = 80), collapse = "\n")
    ) +
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_line(linetype = 3),
      panel.grid.major.x = element_line(linetype = 3),
      legend.key.size = unit(0.5, "cm"),
      plot.margin = unit(c(1, 0.2, 1, 1), "cm")
    ) +
    guides(color = guide_legend(override.aes = list(linetype = 0)))
}


demo_table_fxn <- function(x) {
  meta <- park_metadata %>%
    filter(zone_name == x$zone_name[1])

  if (x$system[1] == "DNR") {
    comp <- state_demos
  } else if (x$system[1] == "Greater MN") {
    comp <- greater_mn_demos
  } else {
    comp <- metro_demos
  }

  x <- x %>%
    filter(str_detect(day_type, "All")) %>%
    select(zone_name, unit_label, category, group, label, percent) %>%
    pivot_wider(names_from = label, values_from = percent) %>%
    rename(
      full_park_pct = year21
    ) %>%
    mutate(group = case_when(
      group == "$60,000 - 74/79,999" ~ "$60,000 - 74,999",
      group == "$75/80,000 - 99,999" ~ "$75,000 - 99,999",
      TRUE ~ group
    ))

  demo_tab <- comp %>%
    full_join(x)

  col_name <- ifelse(meta$system == "DNR", "State Average", ifelse(meta$system == "Greater MN", "Greater Minnesota Average", "Metro Region Average"))

  demo_tab <- demo_tab %>%
    mutate(
      full_park_pct = scales::percent(full_park_pct, accuracy = .1),
      percent = scales::percent(percent, accuracy = .1)
    ) %>%
    select(
      category, group,
      full_park_pct, percent
    ) %>%
    regulartable() %>%
    set_header_labels(values = list(
      category = "Category", group = "Census Group", # summer_park_pct = "Park Estimate (summer 2021)",
      full_park_pct = "Park Estimate (entire 2021)", percent = col_name
    )) %>%
    merge_v(j = 1) %>%
    theme_vanilla() %>%
    font(part = "all", fontname = "Avenir") %>%
    fontsize(part = "all", size = 14) %>%
    hline(i = c(8, 15), border = officer::fp_border(color = "grey40", width = 2)) %>%
    bold(j = 1, bold = TRUE) %>%
    vline(j = 1, part = "all") %>%
    gen_grob()

  demo_explain <- paste(
    "Visitor demographics are inferred based on home locations and data from the 2020 US Census. The table summarizes inferred demographic attributes of visitors to",
    unique(x$unit_label), "during 2021 (LBS data). 2020 US Census data for",
    ifelse(meta$system == "DNR", "the state", ifelse(meta$system == "Greater MN", "Greater Minnesota (the state minus the 7-county metro area)", "the 7-county metro area")), "is provided for context. Please note that local and/or historical context is crucial when interpreting park-level demographic data."
  )

  demo_explainer <- textbox_grob(demo_explain,
    gp = gpar(fontfamily = "Avenir", fontsize = 14, lineheight = 1),
    margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
  )

  demo_title <- textbox_grob("Visitor Demographics",
    gp = gpar(fontfamily = "Avenir", fontsize = 22),
    margin = unit(c(0.1, 0.1, 0, 0.1), "cm")
  )

  demo_plot <- ggplot() +
    geom_point() +
    theme_void() +
    draw_grob(demo_tab) +
    labs(
      title = "Visitor Demographics",
      subtitle = paste(str_wrap(demo_explain, width = 80), collapse = "\n")
    ) +
    legacy_theme +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank()
    )


  print(demo_plot)
}


home_map_fxn <- function(name) {
  meta <- park_metadata %>%
    filter(zone_name == name)
  park_location <- locations %>%
    filter(name == meta$zone_name)


  home <- home_bgs %>%
    filter(
      zone_name == name,
      label == "year21"
    ) %>%
    mutate(
      block_group_id = str_remove_all(block_group_id, "'"),
      primary_state = substr(block_group_id, 1, 2)
    ) %>%
    left_join(bgs, by = c("block_group_id" = "GEOID")) %>%
    st_as_sf()

  stat <- home %>%
    st_drop_geometry() %>%
    group_by(primary_state) %>%
    summarise(total = sum(total_by_bg, na.rm = TRUE), .groups = "keep") %>%
    mutate(pct = total / sum(total)) %>%
    left_join(state_abbr, by = c("primary_state" = "state_code"))

  home_text <- paste(
    "Inferred visitor home locations are reported at the block group level. Darker colors indicate more visitors from a given block group. During 2021, approximately",
    scales::percent(filter(stat, state_name == "Minnesota")$pct), "of visitors to", meta$unit_label, "lived inside of Minnesota while",
    scales::percent(1 - filter(stat, state_name == "Minnesota")$pct), "of visitors lived outside of Minnesota."
  )

  home_map <- home %>%
    ggplot() +
    geom_sf(
      data = mnoutline,
      fill = "grey0",
      color = "black", alpha = 0.8
    ) +
    geom_sf(aes(fill = percent_by_bg), linewidth = 0, color = "transparent") +
    geom_sf(
      data = st_centroid(park_location),
      fill = "#bf812d",
      color = "black",
      shape = 21, size = 2
    ) +
    theme_void() +
    scale_fill_gradientn(
      trans = "log",
      name = "Percent",
      colours = c("#f7fcf5", "#74c476", "#00441b"),
      labels = scales::label_percent()
    ) +
    theme(legend.position = c(.8, .4)) +
    labs(
      title = "Visitor home locations",
      subtitle = paste(strwrap(home_text, width = 80), collapse = "\n")
    ) + # ,
    theme(
      title = element_text(
        size = 22
      ),
      plot.margin = unit(c(0, 0, 0.5, -6), "cm"),
      plot.caption = element_text(size = 12, color = "grey30"),
      plot.title = element_text(size = 22, hjust = 0),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      plot.subtitle = element_text(size = 14, hjust = 0)
    )
  print(home_map)
}
