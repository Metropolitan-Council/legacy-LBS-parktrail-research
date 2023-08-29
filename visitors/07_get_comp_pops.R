## This script pulls 2020 census data on race, income, and educational attainment to be compared
## with LBS estimates. Data is aggregated for the whole state for comparison with DNR units,
## for the 7-county metro area for comparison with Metro Regional units, and for the whole state
## minus the 7-county metro for comparison with Greater MN units. Data is saved to
## `data-intermediate/visiotrs` and is primarily used for providing context in unit-level
## factsheets.

#### get census population
# make sure your census API key is configured for this step!
v20 <- load_variables(2020, "pl", cache = TRUE)
# View(v20)
race_var <- c(
  "P2_005N", # white nh
  "P2_006N", # black nh
  "P2_007N", # amin nh
  "P2_008N", # asian nh
  "P2_009N", # hawaiian pi nh
  "P2_010N", # some other nh
  "P2_011N", # 2+ nh
  "P2_002N" # hispanic
)

income_var <- c(
  "B19001_002",
  "B19001_003",
  "B19001_004",
  "B19001_005",
  "B19001_006",
  "B19001_007",
  "B19001_008",
  "B19001_009",
  "B19001_010",
  "B19001_011",
  "B19001_012",
  "B19001_013",
  "B19001_014",
  "B19001_015",
  "B19001_016",
  "B19001_017"
)

edu_var <- c(
  "B15003_002",
  "B15003_003",
  "B15003_004",
  "B15003_005",
  "B15003_006",
  "B15003_007",
  "B15003_008",
  "B15003_009",
  "B15003_010",
  "B15003_011",
  "B15003_012",
  "B15003_013",
  "B15003_014",
  "B15003_015",
  "B15003_016",
  "B15003_017",
  "B15003_018",
  "B15003_019",
  "B15003_020",
  "B15003_021",
  "B15003_022",
  "B15003_023",
  "B15003_024",
  "B15003_025"
)

mncountyrace <- get_decennial(
  geography = "county",
  summary_var = "P1_001N", # total
  variables = race_var,
  year = 2020,
  state = "MN",
  geometry = FALSE
)

mncountyincome <- get_acs(
  geography = "county",
  summary_var = "B19001_001",
  variables = income_var,
  year = 2020,
  state = "MN",
  geometry = FALSE
)

mncountyedu <- get_acs(
  geography = "county",
  summary_var = "B15003_001",
  variables = edu_var,
  year = 2020,
  state = "MN",
  geometry = FALSE
)

census_race_1 <- mncountyrace %>%
  mutate(county = str_remove(NAME, " County, Minnesota")) %>%
  group_by(county, variable) %>%
  summarise(
    numerator = sum(value),
    denom = sum(summary_value)
  ) %>%
  mutate(race = case_when(
    variable == "P2_007N" ~ "American Indian",
    variable == "P2_008N" ~ "Asian",
    variable == "P2_006N" ~ "Black",
    variable == "P2_002N" ~ "Hispanic or Latinx",
    variable == "P2_011N" ~ "More than one race",
    variable == "P2_009N" ~ "Native Hawaiian and other Pacific Islander",
    variable == "P2_010N" ~ "Some other race",
    variable == "P2_005N" ~ "White"
  )) %>%
  transmute(
    county = county,
    group = as.factor(race),
    category = "Race/ethnicity",
    numerator = numerator,
    denom = denom
  )

census_income_1 <- mncountyincome %>%
  mutate(county = str_remove(NAME, " County, Minnesota")) %>%
  group_by(county, variable) %>%
  summarise(
    numerator = sum(estimate),
    denom = sum(summary_est)
  ) %>%
  mutate(income = case_when(
    variable == "B19001_002" ~ "inc10",
    variable == "B19001_003" ~ "inc_10_15",
    variable == "B19001_004" ~ "inc_15_20",
    variable == "B19001_005" ~ "inc_20_25",
    variable == "B19001_006" ~ "inc_25_30",
    variable == "B19001_007" ~ "inc_30_35",
    variable == "B19001_008" ~ "inc_35_40",
    variable == "B19001_009" ~ "inc_40_45",
    variable == "B19001_010" ~ "inc_45_50",
    variable == "B19001_011" ~ "inc_50_60",
    variable == "B19001_012" ~ "inc_60_75",
    variable == "B19001_013" ~ "inc_75_100",
    variable == "B19001_014" ~ "inc_100_125",
    variable == "B19001_015" ~ "inc_125_150",
    variable == "B19001_016" ~ "inc_150_200",
    variable == "B19001_017" ~ "inc_200"
  )) %>%
  mutate(income = case_when(
    income %in% c("inc_10", "inc_10_15", "inc_15_20", "inc_20_25") ~ "Less than $25,000",
    income %in% c("inc_25_30", "inc_30_35", "inc_35_40") ~ "$25,000 - 39,999",
    income %in% c("inc_40_45", "inc_45_50", "inc_50_60") ~ "$40,000 - 59,999",
    income %in% c("inc_60_75") ~ "$60,000 - 74,999",
    income %in% c("inc_75_100") ~ "$75,000 - 99,999",
    income %in% c("inc_100_125", "inc_125_150") ~ "$100,000 - 149,999",
    TRUE ~ "$150,000 or higher"
  )) %>%
  group_by(county, income) %>%
  summarise(
    category = "Income",
    numerator = sum(numerator),
    denom = unique(denom)
  ) %>%
  rename(group = income) %>%
  mutate(group = factor(group, levels = c(
    "Less than $25,000", "$25,000 - 39,999", "$40,000 - 59,999", "$60,000 - 74,999",
    "$75,000 - 99,999", "$100,000 - 149,999", "$150,000 or higher"
  )))


census_edu_1 <- mncountyedu %>%
  mutate(county = str_remove(NAME, " County, Minnesota")) %>%
  group_by(county, variable) %>%
  summarise(
    numerator = sum(estimate),
    denom = sum(summary_est)
  ) %>%
  mutate(education = case_when(
    variable %in% c(
      "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006",
      "B15003_007", "B15003_008", "B15003_009", "B15003_010",
      "B15003_011", "B15003_012", "B15003_013", "B15003_014",
      "B15003_015", "B15003_016", "B15003_017", "B15003_018"
    ) ~ "High school",
    variable %in% c("B15003_019", "B15003_020", "B15003_021") ~ "Associate degree or some college",
    variable %in% c("B15003_022") ~ "4-year degree",
    TRUE ~ "Graduate or professional degree"
  )) %>%
  group_by(county, education) %>%
  summarise(
    category = "Education",
    numerator = sum(numerator),
    denom = unique(denom)
  ) %>%
  rename(group = education) %>%
  mutate(group = factor(group, levels = c(
    "High school",
    "Associate degree or some college",
    "4-year degree",
    "Graduate or professional degree"
  )))

## state averages for DNR
state_race <- census_race_1 %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

state_income <- census_income_1 %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

state_education <- census_edu_1 %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

metro <- c("Anoka", "Carver", "Dakota", "Hennepin", "Ramsey", "Scott", "Washington")

## metro for metro
metro_race <- census_race_1 %>%
  filter(county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

metro_income <- census_income_1 %>%
  filter(county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

metro_education <- census_edu_1 %>%
  filter(county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

## greater mn for greater mn
greater_mn_race <- census_race_1 %>%
  filter(!county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

greater_mn_income <- census_income_1 %>%
  filter(!county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

greater_mn_education <- census_edu_1 %>%
  filter(!county %in% metro) %>%
  group_by(category, group) %>%
  summarise(
    numerator = sum(numerator),
    denominator = sum(denom)
  ) %>%
  mutate(percent = numerator / denominator)

# save all together
save(state_race, state_income, state_education,
  metro_race, metro_income, metro_education,
  greater_mn_race, greater_mn_income, greater_mn_education,
  file = file.path(here(), "data-intermediate/visitors/census-comp-pops.rda")
)
