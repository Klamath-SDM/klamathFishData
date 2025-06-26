library(tidyr)
library(dplyr)
library(janitor)
library(sf)
library(tidyverse)
library(pins)
library(readxl)
library(rivermile)

# library(googlesheets4)
# note that if we want to source directly from google sheets we need to make the sheet public, lets discuss if we
# want want to do that so that updates are done directly on the sheet (?)
# for now I am downloading spredsheets to tables_with_data folder
# rst <- read_sheet("https://docs.google.com/spreadsheets/d/1rk1uoicdGNwcT6UKDLlQr7YkX9W5Fh2FCea3FAxxbGg",
           # sheet = "RST")
# using sub-basin shp to assign sub_basin names to other data
# sub_basin <- st_read("data-raw/tables_with_data/sub-basin-boundaries/Klamath_HUC8_Subbasin.shp")

# function to assign sub-basin to datasets #TODO let's consider moving this function to an R package(?)
# assign_sub_basin <- function(data, sub_basin, is_point = TRUE, lon_col = "longitude", lat_col = "latitude", sub_basin_col = "NAME") {
#   if (is_point) {
#     sf_data <- st_as_sf(data, coords = c(lon_col, lat_col), crs = 4326)
#   } else {
#     sf_data <- st_as_sf(data)
#   }
#   sf_data <- sf_data |>
#     st_transform(st_crs(sub_basin)) |>
#     st_join(sub_basin[sub_basin_col]) |>
#     rename(sub_basin = !!sub_basin_col) |>
#     mutate(sub_basin = tolower(sub_basin))
#   if (is_point) {
#     coords <- st_coordinates(sf_data)
#     sf_data[[lon_col]] <- coords[, 1]
#     sf_data[[lat_col]] <- coords[, 2]
#     sf_data <- st_drop_geometry(sf_data)
#   }
#   return(sf_data)
# }


# habitat data ---- #TODO check with Maddee on source of this table -this has to be moved to klamathHabitatData
# habitat_data <- read_csv(here::here('data-raw','tables_with_data','habitat_data.csv')) |>
#   clean_names() |>
#   mutate(longitude = as.numeric(longtidue)) |>
#   rename(stream = river) |>
#   assign_sub_basin(sub_basin) |>
#   select(-longtidue) |>
#   select(stream, sub_basin, everything()) |>
#   glimpse()

# fisheries_location_lookup  ----
# RST
rst_sites <- read_csv(here::here('data-raw','tables_with_data', 'rst_sites.csv')) |>
  clean_names() |>
  mutate(data_type = "rst",
         stream = tolower(paste(watershed, "River")),
         site_name = tolower(rst_name),
         agency = tolower(operator),
         coverage_start = NA,
         coverage_end = NA) |>
  assign_sub_basin(sub_basin) |>
  select(stream, sub_basin, data_type, site_name, agency, coverage_start, coverage_end, latitude, longitude, link) |>
  glimpse()

# hatcheries
hatcheries <- read_csv(here::here('data-raw','tables_with_data', 'fish_hatchery_locations.csv')) |>
  clean_names() |>
  mutate(stream = tolower(paste(watershed, "River")),
         data_type = "hatchery",
         coverage_start = NA,
         coverage_end = NA,
         link = resource) |>
  rename(agency = operator) |>
  assign_sub_basin(sub_basin) |>
  select(-c(google_earth_location,  watershed)) |>
  # select(stream, sub_basin, data_type, everything()) |>
  select(stream, sub_basin, data_type, site_name, agency, coverage_start, coverage_end, latitude, longitude, link) |>
  glimpse()

fisheries_location_lookup <- bind_rows(rst_sites, hatcheries)

# redd/carcass surveys ----
# not about these data - it was compiled doing  literature review and documented on google sheets by Willie
# https://docs.google.com/spreadsheets/d/1rk1uoicdGNwcT6UKDLlQr7YkX9W5Fh2FCea3FAxxbGg/edit?gid=444986309#gid=444986309
# in order to get geodata, shapefiles were manually created on GIS, and linked to sheet
# for some of the surveys we have "extent", and for others we have "points"

## Survey Lines
redd_carcass <- read_csv(here::here('data-raw', 'redd_carcass_survey_data' ,'redd_carcass.csv')) |>
  clean_names() |>
  select(-c(upstream_river_access, upstream_google_earth, downstream_access, downstream_lat, downstream_long, downstream_google_earth,
            has_holding, had_redd, has_carcass)) |>
  rename(latitude = upstream_lat,
         longitude = upstream_long) |>
  mutate(agency = tolower(agency),
         species = tolower(species),
         data_type = "redd and carcass survey",
         stream_short = tolower(watershed),
         agency = case_when(is.na(agency) ~ "usfws",
                            T ~ agency)) |>
  filter(!is.na(latitude) | !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  glimpse()

# escapement modeled data ----
klamath_project_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")

klamath_mainstem_fall_chinook_escapement <-  klamath_project_board |>
  pin_read("klamath_mainstem_fall_chinook_escapement") |> glimpse()

#TODO - filter so that it matches megatable

# modeled population data ----
cdfw_population_raw <- read_xlsx(here::here("data-raw", "tables_with_data", "modeled", "Salmonid_Population_Monitoring_Data_CMPv2023.xlsx"), sheet = "Population Data")

klamath_cdfw_population_raw <- cdfw_population_raw |>
  filter(Watershed %in% c("Trinity River", "Scott River", "Shasta River", "Lower Klamath","Klamath River"))

fisheries_model_estimates <- klamath_cdfw_population_raw |>
  janitor::clean_names() |>
  rename(stream = population,
         lifestage = life_stage) |>
  mutate(species = ifelse(!is.na(run_designation), tolower(paste0(run_designation, " ", species)), tolower(species)),
         julian_year = as.numeric(ifelse(!is.na(brood_year), brood_year, survey_season)),
         stream = tolower(stream),
         lifestage = tolower(lifestage),
         estimate_type = tolower(metric),
         estimation_method = tolower(estimation_method),
         sex = NA,
         estimate = value,
         confidence_interval = 95,
         lower_bounds_estimate = x95_lower_ci,
         upper_bounds_estimate = x95_upper_ci,
         is_complete_estimate = ifelse(full_population_estimate == "N", F, T)) |>
  filter(origin == "Mixed") |>
  select(julian_year, stream, species, lifestage, sex, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate) |>
  glimpse()


# save rda files
usethis::use_data(fisheries_location_lookup, overwrite = TRUE)
usethis::use_data(fisheries_model_estimates, overwrite = TRUE)
# usethis::use_data(rst_sites, overwrite = TRUE)
# usethis::use_data(hatcheries, overwrite = TRUE)
# usethis::use_data(redd_carcass_data, overwrite = TRUE)
# usethis::use_data(redd_carcass_location, overwrite = TRUE)
