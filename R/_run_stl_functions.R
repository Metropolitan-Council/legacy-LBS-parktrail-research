# This script contains functions used to create streetlight analyses for parks

run_stl_za <- function(run = TRUE, 
                       fetch = FALSE,
                       metric = "Volume", 
                       mode = "All_Vehicles",
                       date_sequence, 
                       date_sequence_label,
                       time_step, 
                       zone_set_name, 
                       analysis_prefix){
  #' Run core zone activity analyses
  #'
  #' @description Run a sequence of StreetLight zone activity analyses with no add-ons
  #'
  #' @param run: TRUE/FALSE, should analyses be run?
  #' @param fetch: TRUE/FALSE, should analyses be fetched?
  #' @param metric: string indicating which StreetLight metric to use ("Index" or "Volume")
  #' @param mode: string indicating which StreetLight mode to use ("Vehicle", "Bicycle", or "Pedestrian")
  #' @param date_sequence: data frame where each row contains the start_date, end_date, and an index or label for an analysis period. only punctuation should be periods.
  #' @param date_sequence_label: column name from date_sequence data frame to be used as label for analysis period (string)
  #' @param time_step: string indicating time step used in analysis sequence ("Weekly", "Biweekly", or "Monthly)
  #' @param zone_set_name: name of StreetLight zone set to be used (string)
  #' @param analysis_prefix: name of analysis; date_sequence_label will be appended to create unique analysis names (string)

  # libraries
  require(streetlightR)
  require(dplyr)
  require(lubridate)
  require(purrr)
  
  # stl paramters
  if(exists("login_email") == FALSE){
    stop("Please load login_email")
  }
  
  if(is.character(paste(date_sequence[1, eval(date_sequence_label)])) == FALSE){
    stop("date_sequence_label must be a character column")
  }
  
  if(str_detect(paste(date_sequence[1, eval(date_sequence_label)]), "/")){
    stop("date_sequence_label cannot contain forward slashes")
  }
  
  # filter date_sequence to available dates
  date_sequence <- date_sequence %>%
    filter(mdy(end_date) <= mdy("04/30/2022"))
  
  # run analyses
  if(run == TRUE){
    
    cat(paste("\nRunning", time_step, mode, metric))
    
    purrr::map(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      # print some progress messages
      if(analysis_period %% 10 == 0){cat(paste0("\nrunning analysis ", analysis_period, " of ", nrow(date_sequence), 
                                         " (", scales::percent(analysis_period/nrow(date_sequence)), ")"))}
      
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = mode, 
          output_type = metric, 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          enable_home_work_locations = FALSE,
          zone_intersection_type = FALSE,
          hwl_enable_visitor = FALSE,
          traveler_attributes = FALSE
        ), 
        otherwise = "Error creating analysis"
      )
    })
  }
  
  metric_label <- ifelse(mode == "All_Vehicles", "za_all", ifelse(mode == "Pedestrian", "za_ped", "za_bike"))
  p_get_analysis_data <- possibly(.f = get_analysis_data, otherwise = "Error fetching data")
  
  # fetch analyses
  if(fetch == TRUE){
    
    cat(paste("\nFetching", time_step, mode, metric))
    
    za <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      print(analysis_period)
      # print some progress messages
      if(analysis_period %% 10 == 0){cat(paste0("\nfetching analysis ", analysis_period, " of ", nrow(date_sequence), 
                                                " (", scales::percent(analysis_period/nrow(date_sequence)), ")"))}
    
      p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_", 
                                                 date_sequence[analysis_period, eval(date_sequence_label)]),
                          metric = metric_label) %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
    })
  }
  
  # return data, if fetched
  if(fetch == TRUE){
    return(za)
  }
  
}



run_stl_od <- function(run = TRUE, 
                       fetch = FALSE,
                       metric = "Volume", 
                       mode = "Vehicle",
                       date_sequence, 
                       date_sequence_label,
                       time_step, 
                       zone_set_name, 
                       analysis_prefix){
  #' Run core origin-destination analyses
  #'
  #' @description Run a sequence of StreetLight origin-destination analyses with no add-ons
  #'
  #' @param run: TRUE/FALSE, should analyses be run?
  #' @param fetch: TRUE/FALSE, should analyses be fetched?
  #' @param metric: string indicating which StreetLight metric to use ("Index" or "Volume")
  #' @param mode: string indicating which StreetLight mode to use ("Vehicle", "Bicycle", or "Pedestrian")
  #' @param date_sequence: data frame where each row contains the start_date, end_date, and an index or label for an analysis period. only punctuation should be periods.
  #' @param date_sequence_label: column name from date_sequence data frame to be used as label for analysis period (string)
  #' @param time_step: string indicating time step used in analysis sequence ("Weekly", "Biweekly", or "Monthly)
  #' @param zone_set_name: name of StreetLight zone set to be used (string)
  #' @param analysis_prefix: name of analysis; date_sequence_label will be appended to create unique analysis names (string)
  
  # libraries
  require(streetlightR)
  require(dplyr)
  require(lubridate)
  require(purrr)
  
  # stl paramters
  if(exists("login_email") == FALSE){
    stop("Please load login_email")
  }
  
  if(is.character(paste(date_sequence[1, eval(date_sequence_label)])) == FALSE){
    stop("date_sequence_label must be a character column")
  }
  
  if(str_detect(paste(date_sequence[1, eval(date_sequence_label)]), "/")){
    stop("date_sequence_label cannot contain forward slashes")
  }
  
  # filter date_sequence to available dates
  date_sequence <- date_sequence %>%
    filter(mdy(end_date) <= mdy("04/30/2022"))
  
  # run analyses
  if(run == TRUE){
    
    cat(paste("\nRunning", time_step, mode, metric, "O-D"))
    
    purrr::map(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      # print some progress messages
      if(analysis_period %% 10 == 0){cat(paste0("\nrunning analysis ", analysis_period, " of ", nrow(date_sequence), 
                                                " (", scales::percent(analysis_period/nrow(date_sequence)), ")"))}
      
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "OD_Analysis", 
          analysis_name = paste0(analysis_prefix, "_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = mode, 
          output_type = metric, 
          origin_zone_set = zone_set_name, 
          destination_zone_set = zone_set_name,
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          enable_home_work_locations = FALSE,
          zone_intersection_type = FALSE,
          hwl_enable_visitor = FALSE,
          traveler_attributes = FALSE
        ), 
        otherwise = "Error creating analysis"
      )
    })
  }
  
  metric_label <- ifelse(mode == "All_Vehicles", "od_all", ifelse(mode == "Pedestrian", "od_ped", "od_bike"))
  
  # fetch analyses
  if(fetch == TRUE){
    
    cat(paste("\nFetching", time_step, mode, metric, "O-D"))
    
    od <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      # print some progress messages
      if(analysis_period %% 10 == 0){cat(paste0("\nfetching analysis ", analysis_period, " of ", nrow(date_sequence), 
                                                " (", scales::percent(analysis_period/nrow(date_sequence)), ")"))}
      
      p_get_analysis_data <- possibly(.f = get_analysis_data, otherwise = "Error fetching data")
      
      p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_", 
                                                 date_sequence[analysis_period, eval(date_sequence_label)]),
                          metric = metric_label) %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
    })
  }
  
  # return data, if fetched
  if(exists("od")){
    return(od)
  }
  
}



run_home_locations <- function(
                       run = TRUE, 
                       fetch = FALSE,
                       include_demographics = FALSE,
                       date_sequence, 
                       date_sequence_label,
                       zone_set_name, 
                       analysis_prefix){
  #' Run home location analyses
  #'
  #' @description Run a series of home location analyses on park perimeters for each mode. Option to add demographic data.
  #'
  #' @param run: TRUE/FALSE, should analyses be run?
  #' @param fetch: TRUE/FALSE, should analyses be fetched?
  #' @param include_demographics: should the demographic add-on be included in the analysis?
  #' @param date_sequence: data frame where each row contains the start_date, end_date, and an index or label for an analysis period
  #' @param date_sequence_label: column name from date_sequence data frame to be used as label for analysis period (string)
  #' @param zone_set_name: name of StreetLight zone set to be used (string)
  #' @param analysis_prefix: name of analysis; date_sequence_label will be appended to create unique analysis names (string)
  
  # libraries
  require(streetlightR)
  require(dplyr)
  require(lubridate)
  require(purrr)
  
  # stl paramters
  if(exists("login_email") == FALSE){
    stop("Please load login_email")
  }
  
  if(is.character(paste(date_sequence[1, eval(date_sequence_label)])) == FALSE){
    stop("date_sequence_label must be a character column")
  }
  
  if(str_detect(paste(date_sequence[1, eval(date_sequence_label)]), "/")){
    stop("date_sequence_label cannot contain forward slashes")
  }
  
  # filter date_sequence to available dates
  date_sequence <- date_sequence %>%
    filter(mdy(end_date) <= mdy("04/30/2022"))
  
  # run analyses
  if(run == TRUE){
    
    purrr::map(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_home_veh_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "All_Vehicles", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          enable_home_work_locations = TRUE,
          hwl_enable_visitor = TRUE,
          zone_intersection_type = "trips_by_pass_through_setting",
          traveler_attributes = include_demographics
        ), 
        otherwise = "Error creating analysis"
      )
      
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_home_bike_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "Bicycle", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          enable_home_work_locations = TRUE,
          hwl_enable_visitor = TRUE,
          zone_intersection_type = "trips_by_pass_through_setting",
          traveler_attributes = include_demographics
        ), 
        otherwise = "Error creating analysis"
      )
      
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_home_ped_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "Pedestrian", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          enable_home_work_locations = TRUE,
          hwl_enable_visitor = TRUE,
          zone_intersection_type = "trips_by_pass_through_setting",
          traveler_attributes = include_demographics
        ), 
        otherwise = "Error creating analysis"
      )
      
    }) # end purrr
  } # end if run == TRUE

  # fetch analyses - bgs, zips, states, optionally demographics
  if(fetch == TRUE){
    
    p_get_analysis_data <- possibly(.f = get_analysis_data, otherwise = "Error fetching data")
    
    cat("\nfetching block groups")
    bgs <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_veh_",
                                                 date_sequence[analysis_period, eval(date_sequence_label)]),
                          metric = "home_block_groups_all") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
     
      Sys.sleep(6)
      
      bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_bike_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_block_groups_bike") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      
      ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_ped_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_block_groups_ped") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      bind_rows(veh, bike, ped)
    })
    
    cat("\nfetching zip codes")
    zips <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_veh_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_zip_codes_all") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      
      bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_bike_",
                                                         date_sequence[analysis_period, eval(date_sequence_label)]),
                                  metric = "home_zip_codes_bike") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      
      ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_ped_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_zip_codes_ped") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      bind_rows(veh, bike, ped)
    })
    
    cat("\nfetching states")
    states <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_veh_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_state_all") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      
      bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_bike_",
                                                         date_sequence[analysis_period, eval(date_sequence_label)]),
                                  metric = "home_state_bike") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      
      ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_ped_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "home_state_ped") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      bind_rows(veh, bike, ped)
    })
    
    cat("\nfetching demographics")
    if(include_demographics == TRUE){
      
      equity <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
        # sleep to avoid overloading API
        Sys.sleep(6)
        
        veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_veh_",
                                                          date_sequence[analysis_period, eval(date_sequence_label)]),
                                   metric = "zone_traveler_equity_all") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        Sys.sleep(6)
        
        bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_bike_",
                                                           date_sequence[analysis_period, eval(date_sequence_label)]),
                                    metric = "zone_traveler_equity_bike") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        Sys.sleep(6)
        
        ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_ped_",
                                                          date_sequence[analysis_period, eval(date_sequence_label)]),
                                   metric = "zone_traveler_equity_ped") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        bind_rows(veh, bike, ped)
      })
      
      education_income <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
        # sleep to avoid overloading API
        Sys.sleep(6)
        
        veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_veh_",
                                                          date_sequence[analysis_period, eval(date_sequence_label)]),
                                   metric = "zone_traveler_education_income_all") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        Sys.sleep(6)
        
        bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_bike_",
                                                           date_sequence[analysis_period, eval(date_sequence_label)]),
                                    metric = "zone_traveler_education_income_bike") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        Sys.sleep(6)
        
        ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_home_ped_",
                                                          date_sequence[analysis_period, eval(date_sequence_label)]),
                                   metric = "zone_traveler_education_income_ped") %>%
          mutate_all(as.character) %>%
          mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
          suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
        
        bind_rows(veh, bike, ped)
      })
      
    } # end if include demographics == TRUE
  } # end if fetch == TRUE

  # return data, if fetched
  if(fetch == TRUE & include_demographics == FALSE){
    return(list(
      bgs = bgs, 
      zips = zips,
      states = states
    ))
  }
    
  if(fetch == TRUE & include_demographics == TRUE){
    return(list(
      bgs = bgs, 
      zips = zips, 
      states = states, 
      equity = equity, 
      education_income = education_income
    ))
  }
  
}

run_demographics <- function(run = TRUE, 
                             fetch = TRUE,
                             date_sequence, 
                             date_sequence_label,
                             zone_set_name, 
                             analysis_prefix){
  #' Run demographic analyses on park polygons
  #'
  #' @description Run a series of zone activity analyses with demographic add-on for each mode
  #'
  #' @param run: TRUE/FALSE, should analyses be run?
  #' @param fetch: TRUE/FALSE, should analyses be fetched?
  #' @param date_sequence: data frame where each row contains the start_date, end_date, and an index or label for an analysis period
  #' @param date_sequence_label: column name from date_sequence data frame to be used as label for analysis period (string)
  #' @param zone_set_name: name of StreetLight zone set to be used (string)
  #' @param analysis_prefix: name of analysis; date_sequence_label will be appended to create unique analysis names (string)
  
  # libraries
  require(streetlightR)
  require(dplyr)
  require(lubridate)
  require(purrr)
  
  # stl paramters
  if(exists("login_email") == FALSE){
    stop("Please load login_email")
  }
  
  if(is.character(paste(date_sequence[1, eval(date_sequence_label)])) == FALSE){
    stop("date_sequence_label must be a character column")
  }
  
  if(str_detect(paste(date_sequence[1, eval(date_sequence_label)]), "/")){
    stop("date_sequence_label cannot contain forward slashes")
  }
  
  # filter date_sequence to available dates
  date_sequence <- date_sequence %>%
    filter(mdy(end_date) <= mdy("04/30/2022"))
  
  # run analyses
  if(run == TRUE){
    
    purrr::map(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_poly_demos_veh_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "All_Vehicles", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          traveler_attributes = TRUE
        ), 
        otherwise = "Error creating analysis"
      )
      
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_poly_demos_bike_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "Bicycle", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          traveler_attributes = TRUE
        ), 
        otherwise = "Error creating analysis"
      )
      
      Sys.sleep(6)
      
      possibly(
        create_streetlight_analysis(
          login_email = login_email, 
          analysis_type = "Zone_Activity_Analysis", 
          analysis_name = paste0(analysis_prefix, "_poly_demos_ped_", 
                                 date_sequence[analysis_period, eval(date_sequence_label)]),
          travel_mode_type = "Pedestrian", 
          output_type = "Volume", 
          origin_zone_set = zone_set_name, 
          date_ranges = list(start_date = date_sequence[analysis_period, ]$start_date, 
                             end_date = date_sequence[analysis_period, ]$end_date), 
          day_types = day_types, 
          day_parts = day_parts, 
          enable_15min = FALSE,
          tags = list("streetlightR"), 
          traveler_attributes = TRUE
        ), 
        otherwise = "Error creating analysis"
      )
      
    }) # end purrr
  } # end if run == TRUE
  
  # fetch analyses
  if(fetch == TRUE){
    
    cat("\nfetching demographics")
    
    print("equity")
    equity <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_poly_demos_veh_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "zone_traveler_equity_all") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_poly_demos_bike_",
                                                         date_sequence[analysis_period, eval(date_sequence_label)]),
                                  metric = "zone_traveler_equity_bike") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix, "_poly_demos_ped_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "zone_traveler_equity_ped") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      bind_rows(veh, bike, ped)
    })
    
    print("education income")
    education_income <- purrr::map_dfr(.x = c(1:nrow(date_sequence)), .f = function(analysis_period){
      # sleep to avoid overloading API
      Sys.sleep(6)
      print("veh")
      veh <- p_get_analysis_data(analysis_name = paste0(analysis_prefix,  "_poly_demos_veh_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "zone_traveler_education_income_all") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      bike <- p_get_analysis_data(analysis_name = paste0(analysis_prefix,  "_poly_demos_bike_",
                                                         date_sequence[analysis_period, eval(date_sequence_label)]),
                                  metric = "zone_traveler_education_income_bike") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      Sys.sleep(6)
      ped <- p_get_analysis_data(analysis_name = paste0(analysis_prefix,  "_poly_demos_ped_",
                                                        date_sequence[analysis_period, eval(date_sequence_label)]),
                                 metric = "zone_traveler_education_income_ped") %>%
        mutate_all(as.character) %>%
        mutate(!!date_sequence_label := paste0(date_sequence[analysis_period, eval(date_sequence_label)])) %>%
        suppressMessages(left_join(date_sequence)) # suppress so "join by" message doesn't print each time
      
      bind_rows(veh, bike, ped)
    })
  }
  
  # return data, if fetched
  if(fetch == TRUE){
    return(list(
      equity = equity, 
      education_income = education_income
    ))
  }
  
}


run_hourly_activity <- function(
    run = TRUE,
    fetch = FALSE,
    trail = FALSE,
    dates, 
    label,
    zone_set_name = zone_set_name,
    analysis_prefix = park_stl_date_id
){
  
  #' Run hourly analyses 
  #'
  #' @description Run hourly analyses for each mode
  #'
  #' @param run: TRUE/FALSE, should analyses be run?
  #' @param fetch: TRUE/FALSE, should analyses be fetched?
  #' @param dates: 1 row of date_sequence with start_date and end_date
  #' @param label: string identifying time period of analysis (ex: summer2021)
  #' @param zone_set_name: name of StreetLight zone set to be used (string)
  #' @param analysis_prefix: name of analysis; label will be appended to create unique analysis names (string)
  
  parts <- c()
  for(i in c(5:23)){
    if(nchar(i) == 1){
      parts <- c(parts, paste0(i, "_00|0", i, "0", i))
    }
    else{
      parts <- c(parts, paste0(i, "_00|", i, i))
    }
  }
  
  hourly_day_parts <- paste0("All Day|0023,", paste(parts, collapse = ","))
  
  if(run == TRUE){
    
    # create 1 analysis for each mode
    if(trail == FALSE){
      try(create_streetlight_analysis(
        login_email = login_email,
        analysis_type = "Zone_Activity_Analysis",
        output_type = "Volume",
        analysis_name = paste0(analysis_prefix, "_hr_veh_", label),
        date_ranges = list(start_date = dates$start_date,
                           end_date = dates$end_date),
        travel_mode_type = "All_Vehicles",
        origin_zone_set = zone_set_name,
        day_types = day_types,
        day_parts = hourly_day_parts,
        tags = list("streetlightR")
      ))
      Sys.sleep(6)
    }
    
    try(create_streetlight_analysis(
      login_email = login_email, 
      analysis_type = "Zone_Activity_Analysis", 
      output_type = "Volume",
      analysis_name = paste0(analysis_prefix, "_hr_bike_", label), 
      date_ranges = list(start_date = dates$start_date, 
                         end_date = dates$end_date),
      travel_mode_type = "Bicycle", 
      origin_zone_set = zone_set_name,
      day_types = day_types,
      day_parts = hourly_day_parts, 
      tags = list("streetlightR")
    ))
    Sys.sleep(6)
    
    
    try(create_streetlight_analysis(
      login_email = login_email,
      analysis_type = "Zone_Activity_Analysis",
      output_type = "Volume",
      analysis_name = paste0(analysis_prefix, "_hr_ped_", label),
      date_ranges = list(start_date = dates$start_date,
                         end_date = dates$end_date),
      travel_mode_type = "Pedestrian",
      origin_zone_set = zone_set_name,
      day_types = day_types,
      day_parts = hourly_day_parts,
      tags = list("streetlightR")
    ))
  }
  
  if(fetch == TRUE){
    
    if(trail == FALSE){
      veh <- try(get_analysis_data(analysis_name = paste0(analysis_prefix, "_hr_veh_", label), 
                               metric = "za_all"))
      Sys.sleep(6)
    }
    
    bike <- try(get_analysis_data(analysis_name = paste0(analysis_prefix, "_hr_bike_", label), 
                             metric = "za_bike"))
    Sys.sleep(6)
    ped <- try(get_analysis_data(analysis_name = paste0(analysis_prefix, "_hr_ped_", label), 
                             metric = "za_ped"))
    if(trail == FALSE){
      return(list(veh_hourly = veh,
                  bike_hourly = bike,
                  ped_hourly = ped))
    }
    if(trail == TRUE){
      return(list(bike_hourly = bike,
                  ped_hourly = ped))
    }
  }
}
  
