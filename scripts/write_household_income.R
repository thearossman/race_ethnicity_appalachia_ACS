# Write data for variables relevant to analyzing systemic racism factors

# Author: Thea Rossman
# Version: 2020-03-16

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters

vars_acs5 <- c(median_household_income = "B19013_001")

# year of interest
acs5_year <- 2018

# data files (written in other scripts)
central_appalachian_counties_fips <- 
  here::here("./data/central_appalachian_counties_fips.csv")
full_appalachian_counties_fips <- 
  here::here("./data/appalachian_counties_fips.csv")

# files to write to
file_out_central <- 
  here::here("./data/central_appalachian_race_income.csv")
file_out_full <- 
  here::here("./data/appalachian_race_income.csv")


#===============================================================================

# Code


# read in data
all_appalachian_counties <-
  full_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

central_appalachian_counties <-
  central_appalachian_counties_fips %>%
  read_csv(col_types = c(col_integer(), col_character(), col_character()))

# get acs data for census blocks
get_acs_block_grps <- function(county_fips) {
  get_acs(
    geography = "block group", 
    variables = vars_acs5,
    year = acs5_year,
    state = county_fips %/% 1000,
    county = county_fips %% 1000
  ) %>%
    transmute(
      census_block_fips = GEOID,
      county_fips = county_fips,
      name = NAME,
      variable,
      estimate
    ) %>%
    pivot_wider(names_from = variable, values_from = estimate)
}

all_appalachian_census_block_groups <- 
  all_appalachian_counties %>%
  pull(fips) %>%
  map_dfr(get_acs_block_grps) 

central_appalachian_census_block_groups <-
  all_appalachian_census_block_groups %>%
  filter(county_fips %in% (central_appalachian_counties %>% pull(fips)))

all_appalachian_census_block_groups %>%
  write_csv(file_out_full)

central_appalachian_census_block_groups %>%
  write_csv(file_out_central)
