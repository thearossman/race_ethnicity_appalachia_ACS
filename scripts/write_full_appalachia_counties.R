# Write full Appalachia county boundaries

# Author: Thea
# Version: 2020-03-13

# Libraries
library(tidyverse)

# Parameters
  # file from appalachian regional commission website; contains fips codes
arc_counties_file <- "~/Downloads/County-Economic-Status_FY2020_Data.xlsx"

file_out_fips <- here::here("./data/appalachian_counties_fips.csv")


#===============================================================================

# write out file with FIPS
all_appalachian_counties <- 
  arc_counties_file %>%
  readxl::read_xlsx(
    sheet = "ARC Counties", 
    skip = 4
  ) %>%
  transmute(
    fips = as.integer(FIPS), 
    county_name = County, 
    state_name = State
  ) %>%
  drop_na()

write_csv(all_appalachian_counties, file_out_fips)



