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

fisheries_location_lookup <- bind_rows(rst_sites, hatcheries, redd_carcass_surveys)

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
