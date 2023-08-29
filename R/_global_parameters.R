# analysis period -----
analysis_period <- "January 2019 - April 2022"

# analysis end date (dd-mm-yyyy) -----
end_date <- "30-04-2022"

# date sequences
source(file.path(here(), "R/f_get_date_sequence.R"))
get_date_sequence(end_date)

# basic figure caption -----
fig_caption <- "Source: StreetLight Data, Inc. Accessed July 2023."

