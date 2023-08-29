annual_bar_chart_fxn <- function(x){
  x %>%
    ungroup() %>%
    filter(year %in% c(2019, 2020, 2021)) %>%
    select(year, bike_miles_traveled, ped_miles_traveled, miles_traveled) %>%
    mutate(save_bike = round(bike_miles_traveled, 0),
           save_ped = round(ped_miles_traveled, 0),
           save_total = round(miles_traveled, 0)) %>%
    rename(Pedestrian = ped_miles_traveled,
           Bicycle = bike_miles_traveled) %>%
    pivot_longer(cols = c(Pedestrian, Bicycle),
                 names_to = "Mode",
                 values_to = "Miles") %>%
    ggplot(aes(x = year, y = Miles, fill = Mode)) +
    geom_col(color = "black") +
    legacy_theme +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
    scale_x_continuous(expand = c(0, 0.1), labels = c("2019","2020", "2021"), breaks = c(2019, 2020, 2021)) +
    labs(x = "", y = "",
         title = "Total annual miles of use"
    ) +
    scale_fill_manual(values = c("Pedestrian" = legacy_green, 
                                 "Bicycle" = legacy_blue)) + 
    guides(guide = guide_legend(reverse = TRUE)) +
    # legacy_theme +
    theme(legend.position = "bottom") +
    
    geom_label(aes(label = prettyNum(format(save_ped, scientific = F), big.mark = ",", digits = 0),
                   y = save_ped * 0.5), fill = "white", color = "grey20", size = 3) +
    geom_label(aes(label = prettyNum(format(save_bike, scientific = F), big.mark = ",", digits = 0),
                   y = save_ped + (save_bike * 0.5)), fill = "white", color = "grey20", size = 3) +
    geom_label(aes(label = prettyNum(format(save_total, scientific = F), big.mark = ",", digits = 0),
                   y = save_total), fill = "white", color = "black", size = 5,
               position = position_dodge(width = 0.9), vjust = -0.25)
  
}

annual_table_fxn <- function(x){
  ann_dat <- x %>%
    ungroup() %>%
    filter(year %in% c(2019, 2020, 2021)) %>%
    mutate(year = as.character(year)) %>%
    mutate_if(is.numeric, round, 0) %>%
    mutate_if(is.numeric, prettyNum, big.mark = ",") %>%
    select(year, bike_miles_traveled, ped_miles_traveled, miles_traveled,
           counter_estimate, bike_counter_estimate, ped_counter_estimate) %>%
    transmute(`Year` = year,
              `Total miles traveled` = (miles_traveled),
              `Bicycle miles traveled` = (bike_miles_traveled),
              `Pedestrian miles traveled` = (ped_miles_traveled),
              `Total counter estimate` = (counter_estimate),
              `Bicycle counter estimate` = (bike_counter_estimate),
              `Pedestrian counter estimate` = (ped_counter_estimate))
  
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
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank())
  
  print(ann_plot)
  
}

location_map_fxn <- function(name){
  meta <- trail_metadata %>%
    filter(unit_id == name)
  
  trail <- trail_polygons %>%
    filter(unit_id == name) %>%
    st_transform(4326)
  
  trail_vol <- cleaned_trail_segments_mapping %>%
    filter(unit_id == name) %>%
    st_transform(4326)
  
  location_map_text <- paste(meta$unit_label, "is part of the", meta$system_label, "Trail system", paste0("(", meta$primary_district, ")."),
                             "Park boundaries were accessed via", ifelse(meta$system == "Greater MN", "personal communication in July, 2021", "the Minnesota Geospatial Commons in July 2023"),
                             "and may have been edited to improve LBS data performance.")

  location_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles("Esri.WorldGrayCanvas") %>% 
    leaflet::addPolygons(data = trail,
                         fillOpacity = 1,
                         opacity = 1,
                         weight = 5,
                         fillColor = "white", color = "white") %>%
    leaflet::addPolylines(data = trail_vol,
                          color = ~colorNumeric(
                            palette = "viridis", 
                            domain = trail_vol$total_vol)(trail_vol$total_vol),
                          opacity = 1,
                          labelOptions = leaflet::labelOptions(noHide = F, textsize = "12px")
    ) 
  
  (location_map  %>%
      mapview::mapshot(
        vwidth = 800/1.2, vheight = 900/1.2,
        file = file.path(getwd(), "data-temp/mapexport", paste0(name, ".png"))))
  
  logo_file <- file.path(getwd(), "data-temp/mapexport", paste0(name, ".png"))
  
  legend <- get_legend(cleaned_trail_segments_mapping %>% filter(unit_id == name) %>%
                         ggplot() +
                         geom_sf(aes(fill = total_vol, color = total_vol), linewidth = 3) +
                         scale_color_viridis_b(labels = scales::label_comma(), 
                                               na.value = "#808080") +
                         scale_fill_viridis_b(labels = scales::label_comma(), 
                                              na.value = "#808080") +
                         labs(fill = "2021 counter\nestimate",
                              color = "2021 counter\nestimate")+
                         theme(text = element_text(family = "Avenir")))
  
  loc_map <- ggdraw() +
    draw_image(
      logo_file, scale = 1
    ) +
    draw_plot(ggplot() + theme_void() +
                labs(
                  fill = "2021 counter estimate", color = "2021 counter estimate") +
                scale_x_continuous(expand=c(0,0)) +
                scale_y_continuous(expand=c(0,0)) +
                theme(
                  text = element_text(family = "Avenir"),
                  axis.title.y = element_blank()
                  ,plot.margin = unit(c(t= 0, r = 0, b = 0, l = 2), "cm")
                ))
  locmap2 <- plot_grid(loc_map, legend, rel_widths = c(1, .25)) %>%
    add_sub(paste("\n", strwrap(location_map_text, width = 60), collapse = "\n"), x = 0, hjust = 0,
            fontfamily = "Avenir", lineheight = 0.5, size = 12) %>%
    ggdraw()
  
  return(locmap2)
}

monthly_timeseries_plot_fxn <- function(x){
  meta <- trail_metadata %>%
    filter(unit_id == unique(x$unit_id))
  x %>%
    filter(str_detect(day_type, "All")) %>%
    mutate(start_date = lubridate::mdy(start_date)) %>%
    ggplot(aes(x = start_date, y = miles_traveled)) +
    geom_line(col = "black") +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b '%y", expand = c(0, 0),
                 limits = c(as.Date("2019-01-01"), as.Date("2022-04-26"))) +
    legacy_theme +
    labs(x = "", y = "", title = "Monthly use",
         subtitle = paste0("Each point in this time series indicates estimated monthly miles traveled on ", meta$unit_label, ".")) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(linetype = 3),
          plot.margin = unit(c(0.5, 1, 0.5, 0.2), "cm"))
}

seasonal_mode_share_fxn <- function(x){
  meta <- trail_metadata %>%
    filter(unit_id == unique(x$unit_id))
  
  modeshare_text <- "Mode share shows the percent of trail use occurring by bicyclists and pedestrians. Points show the monthly averages across the study period."
  
  x %>%
    filter(str_detect(day_type, "All"),
           !is.na(month)) %>%
    group_by(month) %>%
    mutate(Bicycle = mean(modeshare_bike_miles),
           Pedestrian = mean(modeshare_ped_miles)) %>%
    pivot_longer(cols = c("Bicycle", "Pedestrian"),
                 names_to = "Mode",
                 values_to = "Share") %>%
    mutate(Mode = factor(Mode, levels = c("Pedestrian", "Bicycle"))) %>%
    ggplot(aes(x = month, y = Share, col = Mode, pch = Mode, group = Mode)) +
    geom_point(size = 4) +
    geom_line(lwd = 1) + #1.5) +
    legacy_theme +
    scale_y_continuous(label = scales::label_percent(), expand = expansion(mult = c(0, 0.025))) +
    scale_color_manual(values = c("Pedestrian" = legacy_green,
                                  "Bicycle" = legacy_blue)) +
    guides(color = guide_legend(override.aes = list(linetype = 0))) +
    labs(title = "Mode share", x = "", y = "", #color = "Mode",
         subtitle = paste(strwrap(modeshare_text, width = 80), collapse = "\n")) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_line(linetype = 3),
          panel.grid.major.x = element_line(linetype = 3),
          legend.key.size = unit(0.5, 'cm'),
          plot.margin = unit(c(1, 1, 1, 0.2), "cm")) +
    NULL
}

hourly_timeseries_plot_fxn <- function(x){
  x <-  x %>%
    group_by(unit_id) %>%
    mutate(system = word(unit_id, "_", start = 3, end = 3)) %>%
    ungroup()
  
  hour_text <- "Hourly use during summer 2021 shows patterns of when visitors use trails on weekdays and weekends."
  
  x %>%
    mutate(day_type = str_remove_all(day_type, "1: Average |2: Average | Day")) %>%
    filter(!str_detect(day_type, "All")) %>%
    group_by(day_type, time) %>%
    summarise(total = sum(sumtotal)) %>%
    ungroup() %>%
    group_by(day_type) %>%
    mutate(percent = total / sum(total)) %>%
    mutate(time2 = strftime(time, format = "%H:%M"),
           label = if_else(time2 == "23:00", day_type, NA_character_)) %>%
    ggplot(aes(x = time, y = percent, color = day_type, group = day_type, pch = day_type)) +
    geom_point(size = 4) +
    geom_line(lwd = 1)+
    legacy_theme +
    scale_color_manual(values = c("Weekday (M-F)" = legacy_blue,  
                                  "Weekend (Sa-Su)" = legacy_green))+
    scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0.01)) +
    scale_x_datetime(date_breaks = "4 hours", date_labels = "%l%p", expand = expansion(mult = c(0.01, 0.01))) +
    labs(x = "", color = "", shape = "", y = "", title = "Hourly use",
         subtitle = paste(strwrap(hour_text, width = 80), collapse = "\n")
    ) +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_line(linetype = 3),
          panel.grid.major.x = element_line(linetype = 3),
          legend.key.size = unit(0.5, 'cm'),
          plot.margin = unit(c(1, 0.2, 1, 1), "cm")) +
    guides(color = guide_legend(override.aes = list(linetype = 0)))
}


home_map_fxn <- function(name){
  meta <- trail_metadata %>%
    filter(unit_id == name)
  
  trail <- trail_polygons %>%
    filter(unit_id == name) %>%
    st_transform(26915)
  
  home <- home_bgs %>%
    filter(zone_name == name, 
           label == "year21") %>%
    mutate(block_group_id = str_remove_all(block_group_id, "'")) %>%
    left_join(bgs, by = c("block_group_id" = "GEOID")) %>%
    st_as_sf()
  
  stat <- home_states %>% 
    st_drop_geometry() %>%
    filter(zone_name == name, 
           label == "year21", 
           str_detect(day_type, "All")) %>%
    select(zone_name, state_name, pct = percent_by_state)
  
  
  home_text <- ifelse(nrow(home) > 0,
                           paste("Inferred visitor home locations are reported at the block group level. Darker colors indicate more visitors from a given block group. During 2021, approximately",
                           scales::percent(filter(stat, state_name == "Minnesota")$pct), "of visitors to", meta$unit_label,  "lived inside of Minnesota while",
                           scales::percent(1 - filter(stat, state_name == "Minnesota")$pct), "of visitors lived outside of Minnesota."),
                           paste0("Visitation to was not sufficiently large to infer aggregated home locations for 2021."))
  
  home_map <- home %>%
    ggplot() +
    geom_sf(data = mnoutline,
            fill = "grey0",
            color = "black", alpha = 0.8) +
    geom_sf(aes(fill = percent_by_bg), linewidth = 0, color = "transparent") +
    geom_sf(data = trail,
            fill = "#bf812d",
            color = "#bf812d",
            shape = 21, size = 2) +
    theme_void() +
    scale_fill_gradientn(trans = "log",
                         name = "Percent",
                         colours = c("#f7fcf5", "#74c476", "#00441b"),
                         labels = scales::label_percent()
    ) +
    theme(legend.position = c(.8,.4)) +
    labs(title = "Visitor home locations",
         subtitle = paste(strwrap(home_text, width = 80), collapse = "\n")) + #,
    theme(title = element_text(
      size = 22), 
      plot.margin = unit(c(0, 0, 0.5, -6), "cm"),
      plot.caption = element_text(size = 12, color = "grey30"),
      plot.title = element_text(size = 22, hjust = 0), 
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      plot.subtitle = element_text(size = 14, hjust = 0))
  
  print(home_map)
}

demo_table_fxn <- function(x){
  meta <- trail_metadata %>%
    filter(unit_id == x$unit_id[1])
  
  if(x$system[1] == "DNR"){
    comp <- state_demos
  } else if(x$system[1] == "Greater MN"){
    comp <- greater_mn_demos
  } else{
    comp <- metro_demos
  }
  
  x2 <- x %>%
    filter(str_detect(day_type, "All"),
           label == "full21") %>%
    rename(trail_pct = percent)
  
  demo_tab1 <- comp %>%
    full_join(x2 %>% select(zone_name, unit_id, category, group, trail_pct))
  
  col_name <- ifelse(x$system[1] == "DNR", "State Average", ifelse(x$system[1] == "Greater MN", "Greater Minnesota Average", "Metro Region Average"))
  
  demo_tab <- demo_tab1 %>%
    mutate(trail_pct = scales::percent(trail_pct, accuracy = .1),
           percent = scales::percent(percent, accuracy = .1)) %>%
    select(category, group, trail_pct, percent) %>%
    regulartable() %>%
    set_header_labels(values = list(category = "Category", group = "Census Group",
                                    trail_pct = "Trail Estimate (entire 2021)", percent = col_name)) %>%
    merge_v(j = 1) %>%
    autofit() %>%
    theme_vanilla() %>%
    font(part = "all", fontname = "Avenir") %>%
    fontsize(part = "all", size = 14) %>%
    align(j = 1, align = c("right"), part = "all") %>%
    hline(i = c(8, 15), border = officer::fp_border(color = "grey40", width = 2)) %>%
    bold(j = 1, bold = TRUE) %>%
    width(width = c(.5, 1.5, 1, 1)) %>%
    vline(j = 1, part = "all") %>%
    gen_grob()
  
  demo_explain <- paste("Visitor demographics are inferred based on home locations and data from the 2020 US Census. The table summarizes inferred demographic attributes of visitors to",
                        unique(x$unit_label), "during 2021 (LBS data). 2020 US Census data for",
                        ifelse(meta$system == "DNR", "the state", ifelse(meta$system == "Greater MN", "Greater Minnesota (the state minus the 7-county metro area)", "the 7-county metro area")), "is provided for context. Please note that local and/or historical context is crucial when interpreting trail-level demographic data.")
  
  demo_explainer <- textbox_grob(demo_explain,
                                 gp = gpar(fontfamily = "Avenir", fontsize = 14, lineheight = 1),
                                 margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
  
  demo_title <- textbox_grob("Visitor Demographics",
                             gp = gpar(fontfamily = "Avenir", fontsize = 22),
                             margin =  unit(c(0.1, 0.1, 0, 0.1), "cm"))
  
  demo_plot <- ggplot() +
    geom_point() +
    theme_void() +
    draw_grob(demo_tab) +
    labs(title = "Visitor Demographics",
         subtitle = paste(str_wrap(demo_explain, width = 80), collapse = "\n")) +
    legacy_theme +
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank())
  
  
  print(demo_plot)
}
