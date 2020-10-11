# Write race & ethnicity data for Appalachia

# Author: Thea Rossman
# Version: 2020-03-13

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters

  # census query
race_ethnicity_vars_acs5 <- 
  c(
    white = "B03002_003",
    black = "B03002_004",
    american_indian_alaska_native = "B03002_005",
    asian = "B03002_006",
    native_hawaiian_pacific_islander = "B03002_007",
    other = "B03002_008",
    multiracial = "B03002_009",
    hispanic_latino = "B03001_003",
    total_population = "B03002_001"
  )
# year of interest
acs5_year <- 2018


  # data files (written in other scripts)
central_appalachian_counties_fips <- 
  here::here("./data/central_appalachian_counties_fips.csv")
full_appalachian_counties_fips <- 
  here::here("./data/appalachian_counties_fips.csv")

  # files to write
file_out_central <- 
  here::here("./data/central_appalachian_counties_race.csv")
file_out_full <- 
  here::here("./data/appalachian_counties_race.csv")

#===============================================================================

# Code

# read in data

all_appalachian_counties <-
  full_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

central_appalachian_counties <-
  central_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

# function to get race & ethnicity data for given county 
get_race_ethnicity <- function(statename, counties_df) {
  get_acs(
    geography = "county", 
    variables = race_ethnicity_vars_acs5,
    year = acs5_year,
    state = statename,
    county = 
      {{counties_df}} %>% 
      filter(state_name == statename) %>%
      mutate(fips = fips %% 1000) %>%
      pull(fips)
  ) %>%
    transmute(
      fips = as.integer(GEOID),
      variable,
      estimate
    ) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    left_join(all_appalachian_counties, by = "fips") %>%
    select(fips, county_name, state_name, everything())
}

# for ARC-defined appalachia
race_all_appalachia <- 
  all_appalachian_counties %>%
  select(statename = state_name) %>%
  distinct() %>%
  pmap_dfr(~ get_race_ethnicity(..1, all_appalachian_counties))

race_all_appalachia %>%
  write_csv(file_out_full)

# for ACF-defined appalachia
race_central_appalachia <-
  central_appalachian_counties %>%
  select(statename = state_name) %>%
  distinct %>%
  pmap_dfr(~ get_race_ethnicity(..1, central_appalachian_counties))

race_central_appalachia %>%
  write_csv(file_out_central)



