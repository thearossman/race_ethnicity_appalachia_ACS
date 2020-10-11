# Description

# Author: Name
# Version: 2020-03-13

# Libraries
library(tidyverse)
library(tidycensus)


# Parameters

ky_central_app_counties <- 
  c(
    "Adair", "Bath", "Bell", "Boyd", "Breathitt", "Carter", "Casey", "Clark", 
    "Clay", "Clinton", "Cumberland", "Edmonson", "Elliott", "Estill", "Fleming", 
    "Floyd", "Garrard", "Green", "Greenup", "Harlan", "Hart", "Jackson", 
    "Johnson", "Knott", "Knox", "Laurel", "Lawrence", "Lee", "Leslie", 
    "Letcher", "Lewis", "Lincoln", "McCreary", "Madison", "Magoffin", "Martin", 
    "Menifee", "Metcalfe", "Monroe", "Montgomery", "Morgan", "Nicholas", 
    "Owsley", "Perry", "Pike", "Powell", "Pulaski", "Robertson", "Rockcastle", 
    "Rowan", "Russell", "Wayne", "Whitley",  "Wolfe"
  )

tn_central_app_counties <-
  c(
    "Anderson", "Bledsoe", "Blount", "Bradley", "Campbell", "Cannon", "Carter", 
    "Claiborne", "Clay", "Cocke", "Coffee", "Cumberland", "De Kalb", "Fentress", 
    "Franklin", "Grainger", "Greene", "Grundy", "Hamblen", "Hamilton", 
    "Hancock", "Hawkins", "Jackson", "Jefferson", "Johnson", "Knox", "Lawrence", 
    "Lewis", "Loudon", "McMinn", "Macon", "Marion", "Meigs", "Monroe", "Morgan", 
    "Overton", "Pickett", "Polk", "Putnam", "Rhea", "Roane", "Scott", 
    "Sequatchie", "Sevier", "Smith", "Sullivan", "Unicoi", "Union", "Van Buren", 
    "Warren", "Washington", "White"
  )

va_central_app_counties <- 
  c(
    "Alleghany", "Bath", "Bland", "Botetourt", "Buchanan", "Carroll", 
    "Craig", "Dickenson", "Floyd", "Giles", "Grayson", "Henry", "Highland", 
    "Lee", "Montgomery", "Patrick", "Pulaski", "Rockbridge", "Russell", 
    "Scott", "Smyth", "Tazewell", "Washington", "Wise", "Norton", "Wythe"
  )

  # files containing FIPS codes from Census bureau
fips_all_geocodes_file <- "~/Downloads/all-geocodes-v2018.xlsx"
fips_state_file <- "~/Downloads/state-geocodes-v2018.xlsx"
  # files to write to
file_out_fips <- here::here("./data/central_appalachian_counties_fips.csv")


#===============================================================================

# CODE

# get all geocodes from census bureau website in order to filter to Central App
state_fips <- 
  fips_state_file %>%
  readxl::read_xlsx(skip = 5) %>%
  transmute(
    state_fips = as.integer(`State (FIPS)`),
    name = Name
  ) %>%
  filter(state_fips != 0)

all_US_fips <- 
  fips_all_geocodes_file %>%
  readxl::read_xlsx(skip = 4) %>%
  transmute(
    summary_level = as.integer(`Summary Level`),
    state_code = as.integer(`State Code (FIPS)`),
    county_code = as.integer(`County Code (FIPS)`),
    full_county_fips = state_code*1000 + county_code,
    county_subdivision_code = as.integer(`County Subdivision Code (FIPS)`),
    place_code = as.integer(`Place Code (FIPS)`),
    consolidated_city_code = as.integer(`Consolidtated City Code (FIPS)`),
    area_name = 
      `Area Name (including legal/statistical area description)`
  )


# create a function that returns tibble of county fips for each state
# (recall that for WV, all counties are included)
central_app_countynames <- 
  tribble(
    ~ state, ~ counties,
    "Kentucky", ky_central_app_counties,
    "Tennessee", tn_central_app_counties,
    "Virginia", va_central_app_counties
  )

get_countyfips <- function(statename, counties) {
  all_US_fips %>%
    filter(
      state_code %in% 
        (state_fips %>% filter(name == statename) %>% pull(state_fips))
    ) %>%
    mutate(county_name = map_chr(area_name, word)) %>%
    filter(
      county_name %in% counties, 
      place_code == 0
    ) %>%
    transmute(
      fips = full_county_fips,
      county_name = area_name,
      state_name = statename
    )
}

wv_counties <- 
  all_US_fips %>%
  filter(
    state_code == 
      (state_fips %>% filter(name == "West Virginia") %>% pull(state_fips)),
    place_code == 0,
    county_code != 0
  ) %>%
  transmute(
    fips = full_county_fips,
    county_name = area_name,
    state_name = "West Virginia"
  )

central_appalachia_counties <- 
  central_app_countynames %>% 
  pmap_dfr(~ get_countyfips(..1, ..2)) %>%
  bind_rows(wv_counties) 

write_csv(central_appalachia_counties, file_out_fips)
