## source basic utility functions, colors for plotting, etc
source(file.path(here::here(), "R/_load_packages.R"))
source(file.path(here(), "R/_utility_functions.R"))
source(file.path(here(), "R/_global_parameters.R"))
source(file.path(here(), "R/_streetlight_credentials_parameters.R"))
source(file.path(here(), "R/_set_aesthetics.R"))


meta <- readxl::read_xlsx("data-processed/unit_metadata_2023.07.xlsx")

theo <- meta %>%
  filter(unit_name == "Theodore Wirth, MPRB")

home_bg <- sf::read_sf("data-processed/home-locations/park-home-block-groups/park-home-block-groups.shp")

theo_home_bg <- home_bg %>%
  select(bg_id,
         !!theo$shp_id) %>%
  filter(!is.na(p_metro_25))


minneapolis <- tigris::places("MN") %>%
  filter(NAME == "Minneapolis")

metro <- tigris::counties("MN") %>%
  filter(NAME %in% c("Anoka",
                     "Carver",
                     "Dakota",
                     "Hennepin",
                     "Ramsey",
                     "Scott",
                     "Washington")
  ) %>%
  st_union()

theo_mpls <- st_intersection(theo_home_bg, minneapolis)
theo_metro <- st_intersection(theo_home_bg, metro)

# about 43% from block groups in Minneapolis
sum(theo_mpls$p_metro_25)

# about 97% in metro
sum(theo_metro$p_metro_25)
