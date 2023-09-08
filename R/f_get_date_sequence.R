library(dplyr)
library(lubridate)

get_date_sequence <- function(new_end_date) {
  #' Get date sequence for analysis
  #'
  #' @description takes end date of analysis period and returns weekly, biweekly, and monthly date sequences
  #'
  #' @param new_end_date: string in "dd-mm-yyyy" format denoting end date of analysis period
  #'

  week_sequence <- tibble(
    start_date = seq(
      as.Date("01-01-2019", format = "%d-%m-%Y"),
      as.Date(new_end_date, format = "%d-%m-%Y"), "week"
    ),
    end_date = start_date + 6,
    year = year(start_date)
  ) %>%
    mutate_at(.vars = c("start_date", "end_date"), format, "%m/%d/%Y") %>%
    mutate(
      week_of_year = week(mdy(start_date)),
      label = stringr::str_replace_all(start_date, "/", "\\."),
      year = ifelse(year == 2018 & week_of_year == 53, 2019, year),
      seq = as.character(row_number())
    ) # using slashes in the url makes it impossible to retreive with api

  biweekly_sequence <- week_sequence %>%
    mutate(
      end_date = as.character(mdy(end_date) + 7),
      end_date = paste0(month(end_date), "/", day(end_date), "/", year(end_date))
    ) %>%
    mutate(seq = as.character(row_number()))

  month_sequence <- tibble(
    start_date = seq(
      floor_date(as.Date("01-01-2019", format = "%d-%m-%Y"), "year"),
      floor_date(as.Date(new_end_date, format = "%d-%m-%Y"), "month"), "month"
    ),
    year = year(start_date),
    month = month(start_date),
    days_in_month = days_in_month(start_date),
    end_date = start_date + days(days_in_month - 1),
    label = paste0(month, ".", year)
  ) %>%
    mutate_at(.vars = c("start_date", "end_date"), format, "%m/%d/%Y") %>%
    mutate(seq = as.character(row_number()))

  assign("week_sequence", week_sequence, envir = .GlobalEnv)
  assign("biweekly_sequence", biweekly_sequence, envir = .GlobalEnv)
  assign("month_sequence", month_sequence, envir = .GlobalEnv)
}
