# Get race & ethnicity data for Appalachian census blocks

# Author: Thea Rossman
# Version: 2020-03-13

# Libraries
library(tidyverse)
library(tidycensus)

# Parameters

# census query
race_ethnicity_vars_dec <- 
  c(
    white = "P005003",
    black = "P005004",
    hispanic_latino = "P004003",
    total_population = "P005001"
  )
# year of interest
dec_year <- 2010

# data files (written in other scripts)
central_appalachian_counties_fips <- 
  here::here("./data/central_appalachian_counties_fips.csv")
full_appalachian_counties_fips <- 
  here::here("./data/appalachian_counties_fips.csv")

# files to write to
file_out_central <- 
  here::here("./data/central_appalachian_blocks_race_2010_decennial.csv")
file_out_full <- 
  here::here("./data/appalachian_blocks_race_2010_decennial.csv")


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
get_decennial_blocks <- function(county_fips) {
  get_decennial(
    geography = "block", 
    variables = race_ethnicity_vars_dec,
    year = dec_year,
    state = county_fips %/% 1000,
    county = county_fips %% 1000 
  ) %>%
    transmute(
      census_block_fips = GEOID,
      county_fips = county_fips,
      name = NAME,
      variable,
      value
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(
      n_nonwhite = total_population - white,
      prop_nonwhite = n_nonwhite / total_population
    )
}

all_appalachian_census_blocks <- 
  all_appalachian_counties %>%
  pull(fips) %>%
  map_dfr(get_decennial_blocks) 

central_appalachian_census_blocks <-
  all_appalachian_census_blocks %>%
  filter(county_fips %in% (central_appalachian_counties %>% pull(fips)))

all_appalachian_census_blocks %>%
  write_csv(file_out_full)

central_appalachian_census_blocks %>%
  write_csv(file_out_central)
