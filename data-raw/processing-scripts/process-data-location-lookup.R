library(tidyr)
library(dplyr)
library(janitor)
library(sf)
library(tidyverse)
library(pins)
library(readxl)
library(rivermile)

# fisheries_location_lookup  ----
# RST
rst_sites <- read_csv(here::here('data-raw','tables_with_data', 'rst_sites.csv')) |>
  clean_names() |>
  mutate(data_type = "rst",
         stream = tolower(paste(watershed, "River")),
         site_name = tolower(rst_name),
         agency = tolower(operator),
         downstream_latitude = NA,
         downstream_longitude = NA) |>
  assign_sub_basin(sub_basin) |>
  select(stream, sub_basin, data_type, site_name, agency, latitude, longitude, downstream_latitude, downstream_longitude, link) |>
  glimpse()

# hatcheries
hatcheries <- read_csv(here::here('data-raw','tables_with_data', 'fish_hatchery_locations.csv')) |>
  clean_names() |>
  mutate(stream = tolower(paste(watershed, "River")),
         data_type = "hatchery",
         link = resource,
         downstream_latitude = NA,
         downstream_longitude = NA) |>
  rename(agency = operator) |>
  assign_sub_basin(sub_basin) |>
  select(-c(google_earth_location,  watershed)) |>
  # select(stream, sub_basin, data_type, everything()) |>
  select(stream, sub_basin, data_type, site_name, agency, latitude, longitude, downstream_latitude, downstream_longitude, link) |>
  glimpse()

# redd/carcass surveys ----
# not about these data - it was compiled doing  literature review and documented on google sheets by Willie
# https://docs.google.com/spreadsheets/d/1rk1uoicdGNwcT6UKDLlQr7YkX9W5Fh2FCea3FAxxbGg/edit?gid=444986309#gid=444986309
# for now, I have left out the shapefiles created to associate geodata, and rely on lat long (will work on schema)

## Survey Lines
redd_carcass_surveys <- read_csv(here::here('data-raw', 'redd_carcass_survey_data' ,'redd_carcass.csv')) |>
  clean_names() |>
  select(-c(id, upstream_river_access, upstream_google_earth, downstream_access, downstream_google_earth, downstream_rkm, upstream_rkm,
            has_holding, had_redd, has_carcass)) |>
  rename(latitude = upstream_lat,
         longitude = upstream_long,
         downstream_latitude = downstream_lat,
         downstream_longitude = downstream_long) |>
  mutate(agency = tolower(agency),
         species = tolower(species),
         data_type = "redd and carcass survey",
         stream = tolower(paste(watershed, "River")),
         site_name = NA,
         # stream_short = tolower(watershed),
         agency = case_when(is.na(agency) ~ "usfws",
                            T ~ agency)) |>
  filter(!is.na(latitude) | !is.na(longitude)) |>
  select(-watershed) |>
  assign_sub_basin(sub_basin) |>
  select(stream, sub_basin, data_type, site_name, agency, latitude, longitude, downstream_latitude, downstream_longitude, link) |>
  # st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  # find_nearest_river_miles() |>
  glimpse()

# Habitat data ------------------------------------------------------------
habitat_raw <- read_csv(here::here('data-raw','tables_with_data', 'habitat_data.csv'))

habitat <- habitat_raw |>
  janitor::clean_names() |>
  rename(stream = river,
         site_name = location_name,
         longitude = longtidue) |>
  select(stream, site_name, latitude, longitude, link) |>
  assign_sub_basin(sub_basin) |>
  mutate(data_type = "habitat")


# Combine -----------------------------------------------------------------

data_location_lookup <- bind_rows(rst_sites, hatcheries, redd_carcass_surveys, habitat)

# save rda files
usethis::use_data(data_location_lookup, overwrite = TRUE)
