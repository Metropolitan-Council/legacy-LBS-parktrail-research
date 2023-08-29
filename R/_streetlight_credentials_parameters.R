# StreetLight login email -----
login_email <- Sys.getenv("STREETLIGHT_LOGIN")
assign("login_email", login_email, envir = .GlobalEnv)

# TODO: check if API key is needed here at all

# day types and parts -----
day_types <- "All Days|17,Average Weekday|15,Average Weekend Day|67"
assign("day_types", day_types, envir = .GlobalEnv)

day_types_daily <- "All Days|17,Average Weekday|15,Average Weekend Day|67,Monday|1,Tuesday|2,Wednesday|3,Thursday|4,Friday|5,Saturday|6,Sunday|7"
assign("day_types_daily", day_types_daily, envir = .GlobalEnv)

day_parts <- "All Day|0023"
assign("day_parts", day_parts, envir = .GlobalEnv)

# dates -----
# TODO: is this actually used anywhere? I think we might be able to cut this



