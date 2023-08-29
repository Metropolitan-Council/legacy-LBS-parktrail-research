## This script runs additional analyses on several Greater MN trails/trail segments at which
## mountain biking studies were conducted during the summer of 2021. The data created in 
## this script is used for data validation and is saved in `data-intermediate`

gmn_zone_suffix <- "2023.04.05"

if(run_greatermn_validation == TRUE){
  
  # the cook county zone set required manual editing so requires a different suffix
  cook_zone_suffix <- "feb" 
  
  create_streetlight_analysis(login_email = login_email,
                              analysis_type = "Zone_Activity_Analysis",
                              analysis_name = paste0("cookco_bike_sum21_", gmn_zone_suffix),
                              date_ranges = list(start_date = "06/01/2021", end_date = "08/31/2021"),
                              travel_mode_type = "Bicycle",
                              output_type = "Volume",
                              origin_zone_set = paste0("cook_co_mb_validation_", cook_zone_suffix),
                              tags = list("StreetlightR"))
  
  Sys.sleep(6)
  create_streetlight_analysis(login_email = login_email,
                              analysis_type = "Zone_Activity_Analysis",
                              analysis_name = paste0("cookco_ped_sum21_", gmn_zone_suffix),
                              date_ranges = list(start_date = "06/01/2021", end_date = "08/31/2021"),
                              travel_mode_type = "Pedestrian",
                              output_type = "Volume",
                              origin_zone_set = paste0("cook_co_mb_validation_", cook_zone_suffix),
                              tags = list("StreetlightR"))
  
  Sys.sleep(6)
  upload_zone_set(login_email = login_email,
                  geom_type = "line",
                  zones = dt_zones,
                  zone_set_name = paste0("dt_osm_validation_", gmn_zone_suffix))
  
  Sys.sleep(6)
  create_streetlight_analysis(login_email = login_email,
                              analysis_type = "Zone_Activity_Analysis",
                              analysis_name = paste0("dt_bikes_sum21_", gmn_zone_suffix),
                              date_ranges = list(start_date = "06/01/2021", end_date = "08/31/2021"),
                              travel_mode_type = "Bicycle",
                              output_type = "Volume",
                              origin_zone_set = paste0("dt_osm_validation_", gmn_zone_suffix),
                              tags = list("StreetlightR"))
  
  Sys.sleep(6)
  create_streetlight_analysis(login_email = login_email,
                              analysis_type = "Zone_Activity_Analysis",
                              analysis_name = paste0("dt_ped_sum21_", gmn_zone_suffix),
                              date_ranges = list(start_date = "06/01/2021", end_date = "08/31/2021"),
                              travel_mode_type = "Pedestrian",
                              output_type = "Volume",
                              origin_zone_set = paste0("dt_osm_validation_", gmn_zone_suffix),
                              tags = list("StreetlightR"))
}

if(fetch_greatermn_validation == TRUE){
  cp <- get_analysis_data(analysis_name = paste0("cookco_ped_sum21_", gmn_zone_suffix),
                          metric = "za_ped") %>%
    mutate(Mode = "Pedestrian",
           Volume = as.numeric(average_daily_zone_traffic_st_l_volume))
  Sys.sleep(15)
  cb <- get_analysis_data(analysis_name = paste0("cookco_bike_sum21_", gmn_zone_suffix),
                          metric = "za_bike") %>%
    mutate(Mode = "Bicycle",
           Volume = as.numeric(average_daily_zone_traffic_st_l_volume))
  
  gmn_sadt <- data.frame(zone_name = rep(c("Jackpot Trail_975834790_23",
                                           "Talus Trail_793102556_4",
                                           "Short Stacker Bike Trail_975834775_22",
                                           "High Climber Trail_875602096_19"), each = 3),
                         line_zone_name = rep(c("Jackpot Trail_874512079_11",
                                                "Talus Trail_793102556_3",
                                                "Short Stacker Trail / 875385854 / 1005",
                                                "High Climber Trail_875602096_15"), each = 3),
                         label = rep(c("Jackpot", "Talus", "Short Stacker", "High CLimber"), each = 3),
                         day_type = rep(c("0: All Days (M-Su)", "1: Average Weekday (M-F)", "2: Average Weekend Day (Sa-Su)"),
                                        times = 4),
                         sadt = c(30, 25, 42, # jackpot
                                  16, 14, 20, # talus
                                  59, 49, 81, # short stacker
                                  32, 26, 44)) # highclimber
  
  cook_line <- rbind(cp, cb) %>%
    filter(str_detect(day_part, "All"),
           zone_name %in% c("Short Stacker Trail / 875385854 / 1005",
                            "Jackpot Trail_874512079_11",
                            "High Climber Trail_875602096_15",
                            "Talus Trail_793102556_3"
           )) %>%
    group_by(zone_name, day_type) %>%
    summarise(Volume = sum(Volume)) %>%
    left_join(gmn_sadt %>%
                group_by(zone_name) %>%
                mutate(zone_name = line_zone_name),
              by = c("zone_name", "day_type"))
  
  Sys.sleep(15)
  dtb <- get_analysis_data(analysis_name = paste0("dt_bikes_sum21_", gmn_zone_suffix),
                           metric = "za_bike") %>%
    mutate(Mode = "Bicycle",
           Volume = as.numeric(average_daily_zone_traffic_st_l_volume))
  Sys.sleep(15)
  dtp <- get_analysis_data(analysis_name = paste0("dt_ped_sum21_", gmn_zone_suffix),
                           metric = "za_ped") %>%
    mutate(Mode = "Pedestrian",
           Volume = as.numeric(average_daily_zone_traffic_st_l_volume))
  
  duluth <- rbind(dtb, dtp) %>%
    filter(str_detect(day_part, "All")) %>%
    select(zone_name, day_type, Mode, Volume)
  
  
  # becks is not quite right
  
  zones <- c("St. Louis River Trail (Duluth Traverse)_946818017", # mission creek
             "Sargent Creek (Duluth Traverse)_1110512562", # becks - still not quite right bc it's on the wrong side of the road but ?
             "Spirit Mountain (Duluth Traverse)_1110507433", # spirit mountain
             "Lower Burner of Stovetop (Duluth Traverse)_914248661", # haines
             "Observation Hill (Duluth Traverse)_914497678", # twin ponds/observation hill
             "Nature Center - The Pines (Duluth Traverse)_940381542", # hartley
             "Lester River Trail / Duluth Traverse_824185019", # lester
             "Hawk Ridge Trail (Duluth Traverse)_824183333") # hawk ridge
  
  dt_stl <- duluth %>%
    filter(zone_name %in% zones) %>%
    mutate(Bike = ifelse(Mode ==  "Bicycle", Volume, 0)) %>%
    group_by(zone_name, day_type) %>%
    summarise(Volume = sum(Volume, na.rm = TRUE),
              Bike = sum(Bike, na.rm = TRUE))
  
  dt_sadt <- data.frame(zone_name = rep(c("St. Louis River Trail (Duluth Traverse)_946818017",
                                          "Sargent Creek (Duluth Traverse)_1110512562",
                                          "Spirit Mountain (Duluth Traverse)_1110507433",
                                          "Lower Burner of Stovetop (Duluth Traverse)_914248661",
                                          "Observation Hill (Duluth Traverse)_914497678",
                                          "Nature Center - The Pines (Duluth Traverse)_940381542",
                                          "Lester River Trail / Duluth Traverse_824185019",
                                          "Hawk Ridge Trail (Duluth Traverse)_824183333"),  each = 3),
                        site = rep(c("Mission Creek",
                                     "Becks Road",
                                     "Spirit Mountain",
                                     "Haines Road",
                                     "Twin Ponds Observation Hill",
                                     "Hartley Park",
                                     "Lester Park",
                                     "Hawk Ridge"), each = 3),
                        wd_ratio = rep(c(1.5,
                                         2.2,
                                         2.9,
                                         2.1,
                                         1.9,
                                         1.4,
                                         2.2,
                                         2.3), each = 3),
                        day_type = rep(c("0: All Days (M-Su)", "1: Average Weekday (M-F)", "2: Average Weekend Day (Sa-Su)"),
                                       times = 8),
                        sadt = c(130, 115, 168, # mission creek,
                                 129, 96, 214, # becks,
                                 189, 120, 354, # spirit mountain,
                                 86, 66, 137, # haines,
                                 122, 96, 187, # twin ponds/obvservation hill,
                                 265, 237, 334, # hartley,
                                 113, 84, 186, # hawk ridge,
                                 238, 174, 399) # lester,
  )
  
  dt_both <- dt_stl %>%
    select(zone_name, day_type, Volume, Bike) %>%
    full_join(dt_sadt, by = c("zone_name", "day_type"))
  
  gmn_validation_data <- cook_line %>%
    bind_rows(dt_both) %>%
    mutate(long_count = ifelse(str_detect(zone_name, "Short Stacker|Spirit Mountain"), TRUE, FALSE)) %>%
    group_by(day_type) %>%
    mutate(day_type = str_match(day_type, ": \\s*(.*?)\\s* \\(")[,2]) %>%
    ungroup()
  
  saveRDS(gmn_validation_data, file.path(here(), "data-intermediate/trails/gmn-validation-data.RDS"))
}