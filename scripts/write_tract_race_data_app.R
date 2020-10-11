# Get race & ethnicity data for Appalachian census tracts

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

  # files to write to
file_out_central <- 
  here::here("./data/central_appalachian_tracts_race.csv")
file_out_full <- 
  here::here("./data/appalachian_tracts_race.csv")


#===============================================================================

# Code


# read in data
all_appalachian_counties <-
  full_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

central_appalachian_counties <-
  central_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

# get acs data for census tracts
get_acs_state_tract <- function(state_name) {
  get_acs(
    geography = "tract", 
    variables = race_ethnicity_vars_acs5,
    year = acs5_year,
    state = state_name
  ) %>%
    transmute(
      fips = GEOID,
      county_fips = map_chr(fips, str_sub, 1L, 5L) %>% as.integer(),
      name = NAME,
      variable,
      estimate
    ) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      n_nonwhite = total_population - white,
      prop_nonwhite = n_nonwhite / total_population
    )
}

all_appalachian_census_tracts <- 
  all_appalachian_counties %>%
  select(state_name) %>%
  distinct() %>%
  map_dfr(get_acs_state_tract) %>%
  filter(county_fips %in% (all_appalachian_counties %>% pull(fips)))

central_appalachian_census_tracts <-
  all_appalachian_census_tracts %>%
  filter(county_fips %in% (central_appalachian_counties %>% pull(fips)))


all_appalachian_census_tracts %>%
  write_csv(file_out_full)

central_appalachian_census_tracts %>%
  write_csv(file_out_central)
