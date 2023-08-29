## This script creates a sample of parks with no missing weeks and tests several methods
## of time series imputation. Methods are compared using RMSE and MAPE. The results of this 
## script are used in the "evaluating imputation" section of the park documentation. 

set.seed(1997); random_sample <- sample(n_weeks, 30)

load(file.path(here(), "data-intermediate/parks/raw-week-volume.rda"))

no_missing_names <- week_vehicle %>%
  filter(str_detect(day_type, "All"), 
         str_detect(day_part, "All"),
         intersection_type == "Trip End", 
         mode_of_travel == "All Vehicles LBS Plus - StL All Vehicles Volume") %>%
  group_by(zone_name) %>%
  summarise(count = n()) %>%
  filter(count == n_weeks)

no_missing <- week_vehicle %>%
  filter(zone_name %in% no_missing_names$zone_name, 
         str_detect(day_type, "All"), 
         str_detect(day_part, "All"), 
         intersection_type == "Trip End") %>%
  left_join(week_sequence, by = "label") %>%
  mutate(weekly_vehicles = as.numeric(average_daily_zone_traffic_st_l_volume))

#TODO: make it clear what we're imputing

# manually remove records to impute
manual_missing1 <- no_missing %>%
  arrange(start_date) %>%
  group_by(zone_name) %>%
  mutate(row_number = row_number()) %>% 
  mutate(missing_volume = ifelse(row_number %in% random_sample, NA, weekly_vehicles))

manual_missing <- manual_missing1 %>%
  group_by(zone_name) %>%
  mutate(imputed_volume = na_seadec(ts(missing_volume, frequency = 52), algorithm = "ma"), 
         interpolated_volume = na_interpolation(ts(missing_volume, frequency = 52), option = "linear"), 
         spline_volume = na_interpolation(ts(missing_volume, frequency = 52), option = "spline"), 
         ma_volume = na_ma(ts(missing_volume, frequency = 52))) 

ar <- manual_missing %>%
  filter(str_detect(zone_name, "Alexander-Ramsey"))

ar_ts <- ts(ar$missing_volume)
ar_filled <- ts(ar$ma_volume)
ar_truth <- ts(ar$weekly_vehicles)

ar$filled <- ar_filled 

err <- ar %>% 
  group_by(zone_name) %>%
  summarise(RMSE = sqrt(mean(weekly_vehicles - filled)^2), 
            MAPE = mean(abs((weekly_vehicles - filled)/weekly_vehicles)) * 100)

eval_dat <- purrr:::map_dfr(.x = c(1:500), .f = function(rep){
  man_missing <- no_missing %>%
    group_by(zone_name) %>%
    mutate(random_index = list(sample(n_weeks, 30)),
           row_number = row_number()) %>%
    mutate(missing_volume = ifelse(row_number %in% unlist(random_index), NA, weekly_vehicles),
           linear_volume = na_interpolation(ts(missing_volume, frequency = 52), option = "linear"),
           spline_volume = na_interpolation(ts(missing_volume, frequency = 52), option = "spline"),
           ma_volume = na_ma(ts(missing_volume, frequency = 52)),
           rep_number = rep)
  
  # residuals by point
  eval_dat <- man_missing %>%
    mutate(linear_resid = linear_volume - weekly_vehicles,
           spline_resid = spline_volume - weekly_vehicles,
           ma_resid = ma_volume - weekly_vehicles) %>%
    mutate(linear_rel = 100*linear_resid/weekly_vehicles,
           spline_rel = 100*spline_resid/weekly_vehicles,
           ma_rel = 100*ma_resid/weekly_vehicles)
})
# get RMSE, MAPE by method
eval <- eval_dat %>%
  tidyr::pivot_longer(cols = c("linear_volume", "spline_volume","ma_volume"),
                      names_to = "method") %>%
  group_by(method, rep_number) %>%
  summarise(RMSE = sqrt(mean(weekly_vehicles - value)^2),
            MAPE = mean(abs((weekly_vehicles - value)/weekly_vehicles)) * 100,
            MAE = mean(abs(weekly_vehicles - value))) %>%
  ungroup() %>%
  group_by(rep_number) %>%
  select(-MAE) %>%
  tidyr::pivot_wider(names_from = "method", 
                     values_from = c("RMSE", "MAPE")) %>%
  mutate(min_MAPE = min(MAPE_linear_volume, MAPE_ma_volume, MAPE_spline_volume), 
         min_RMSE = min(RMSE_linear_volume, RMSE_ma_volume, RMSE_spline_volume), 
         best_MAPE = case_when(min_MAPE == MAPE_linear_volume ~ "linear", 
                               min_MAPE == MAPE_ma_volume ~ "ma", 
                               min_MAPE == MAPE_spline_volume ~ "spline"), 
         best_RMSE = case_when(min_RMSE == RMSE_linear_volume ~ "linear", 
                               min_RMSE == RMSE_ma_volume ~ "ma", 
                               min_RMSE == RMSE_spline_volume ~ "spline")) 

# within each trial, which method had the smallest RMSE?
best_MAPE <- eval %>%
  group_by(best_MAPE) %>%
  summarise(pct = n()/nrow(eval)) %>%
  arrange(-pct)
best_RMSE <- eval %>%
  group_by(best_RMSE) %>%
  summarise(pct = n()/nrow(eval)) %>%
  arrange(-pct)
rmse_pct <- best_RMSE %>%
  filter(best_RMSE == "ma") %>%
  select(pct) 
# averages
avgs <- eval_dat %>%
  tidyr::pivot_longer(cols = c("linear_volume", "spline_volume","ma_volume"),
                      names_to = "method") %>%
  group_by(method, rep_number) %>%
  summarise(RMSE = sqrt(mean(weekly_vehicles - value)^2),
            MAPE = mean(abs((weekly_vehicles - value)/weekly_vehicles)) * 100,
            MAE = mean(abs(weekly_vehicles - value))) %>%
  ungroup() %>%
  group_by(method) %>%
  select(-MAE) %>%
  summarise(mean_RMSE = mean(RMSE), 
            mean_MAPE = mean(MAPE)) 
avgs_tab <- avgs %>%
  arrange(mean_RMSE) %>%
  mutate(`Imputation Method` = case_when(method == "linear_volume" ~ "Linear Interpolation", 
                                         method == "ma_volume" ~ "Weighted Moving Average", 
                                         method == "spline_volume" ~ "Spline Interpolation")) %>%
  mutate(`Mean RMSE` = round(mean_RMSE, 4), 
         `Mean MAPE` = round(mean_MAPE, 2)) %>%
  select(`Imputation Method`, `Mean RMSE`, `Mean MAPE`)

mean_mape <- avgs %>% filter(method == "ma_volume") %>% select(mean_MAPE)


## save pieces needed for documentation

save(no_missing_names, ar, ar_ts, ar_filled, ar_truth, err, avgs_tab, 
     file = file.path(here(), "data-intermediate/parks/imputation-data.rda"))
