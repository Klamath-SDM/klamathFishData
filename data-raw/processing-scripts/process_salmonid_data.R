library(tidyr)
library(dplyr)
library(janitor)
library(sf)
library(tidyverse)
library(pins)

# library(googlesheets4)
# note that if we want to source directly from google sheets we need to make the sheet public, lets discuss if we
# want want to do that so that updates are done directly on the sheet (?)
# for now I am downloading spredsheets to tables_with_data folder
# rst <- read_sheet("https://docs.google.com/spreadsheets/d/1rk1uoicdGNwcT6UKDLlQr7YkX9W5Fh2FCea3FAxxbGg",
           # sheet = "RST")
# using sub-basin shp to assign sub_basin names to other data
sub_basin <- st_read("data-raw/tables_with_data/sub-basin-boundaries/Klamath_HUC8_Subbasin.shp")

# function to assign sub-basin to datasets #TODO let's consider moving this function to an R package(?)
assign_sub_basin <- function(data, sub_basin, is_point = TRUE, lon_col = "longitude", lat_col = "latitude", sub_basin_col = "NAME") {
  if (is_point) {
    sf_data <- st_as_sf(data, coords = c(lon_col, lat_col), crs = 4326)
  } else {
    sf_data <- st_as_sf(data)
  }
  sf_data <- sf_data |>
    st_transform(st_crs(sub_basin)) |>
    st_join(sub_basin[sub_basin_col]) |>
    rename(sub_basin = !!sub_basin_col) |>
    mutate(sub_basin = tolower(sub_basin))
  if (is_point) {
    coords <- st_coordinates(sf_data)
    sf_data[[lon_col]] <- coords[, 1]
    sf_data[[lat_col]] <- coords[, 2]
    sf_data <- st_drop_geometry(sf_data)
  }
  return(sf_data)
}


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
# TODO use rivermile package to assign river mile, brainstorm schema to have more unified datasets
# shapefile 1 ---
survey_shapefile_1 <- st_read("data-raw/redd_carcass_survey_data/shapefiles/redd_survey_coho_USFWS.shp")
survey_shapefile_1 <- st_transform(survey_shapefile_1, crs = 4326)
survey_lines_metadata_1 <- read_csv(here::here('data-raw', 'redd_carcass_survey_data' ,'redd_carcass.csv')) |>
  clean_names() |>
  filter(id >= 1 & id <= 7) |>
  select(-c(upstream_google_earth, downstream_google_earth,
            downstream_lat, downstream_long, upstream_lat, upstream_long)) |>
  mutate(Id = id) |>
  select(-id) |>
  mutate(species = "coho salmon and fall chinook salmon") |>   #adding this for now to include both reaches as one (id 8-13 too)
  glimpse()

# join shapefile and metadata
survey_lines_1 <- survey_shapefile_1 |>
  left_join(survey_lines_metadata_1, by = "Id") |>
  mutate(stream = paste(watershed, "River")) |>
  assign_sub_basin(sub_basin, is_point = FALSE) |>
  select(-watershed) |>
  glimpse()

print(st_crs(survey_shapefile_1))
print(st_geometry_type(survey_shapefile_1))

# shapefile 2 ---
survey_shapefile_2 <- st_read("data-raw/redd_carcass_survey_data/shapefiles/redd_carcass_fall_chinook.shp") |> select(-Shape_Leng)
survey_shapefile_2 <- st_transform(survey_shapefile_2, crs = 4326)
#metadata of those on shapefiles
survey_lines_metadata_2 <- read_csv(here::here('data-raw', 'redd_carcass_survey_data', 'redd_carcass.csv')) |>
  clean_names() |>
  filter(id >= 18 & id <= 20) |>
  select(-c(upstream_google_earth, downstream_google_earth,
            downstream_lat, downstream_long, upstream_lat, upstream_long)) |>
  mutate(Id = id,
         temporal_coverage = ifelse(temporal_coverage == "Oct 7 - Late Nov/Early Dec 2024", "2024", temporal_coverage)) |>
  select(-id) |>
  # select(-c(upstream_lat, upstream_long, downstream_long, downstream_lat, data_type)) |>
  # filter(!is.na(latitude)) |>
  glimpse()

# join shapefile and metadata
survey_lines_2 <- survey_shapefile_2 |>
  left_join(survey_lines_metadata_2, by = "Id") |>
  mutate(stream = paste(watershed, "River")) |>
  assign_sub_basin(sub_basin, is_point = FALSE) |>
  select(-watershed) |>
  glimpse()

print(st_crs(survey_shapefile_2))
print(st_geometry_type(survey_shapefile_2))

# survey points
survey_points <- read_csv(here::here('data-raw', 'redd_carcass_survey_data', 'redd_carcass.csv')) |>
  clean_names() |>
  filter(id >= 14 & id <= 17 | id >= 21 & id <= 27,
         !is.na(upstream_lat)) |>
  select(-c(upstream_google_earth, downstream_google_earth, downstream_lat, downstream_long)) |>
  mutate(Id = id) |>
  select(-id) |>
  rename(latitude = upstream_lat,
         longitude = upstream_long) |>
  mutate(temporal_coverage = case_when(
    temporal_coverage == 2002 ~ "2002 fish kill event",
    TRUE ~ "2008"),
    agency = "CDFW",
    stream = paste(watershed, "River")) |>
  assign_sub_basin(sub_basin) |>
  select(-watershed) |>
  glimpse()


#note that the reason why id numbers are missing is because extent was the same, we just added the extra species to the data entry
all_surveys <- bind_rows(survey_lines_1, survey_lines_2, survey_points) |>
  clean_names() |>
  select(-c(label)) |>
  select(id, stream, sub_basin, data_type, species, temporal_coverage,everything()) |>
  glimpse()

# redd/carcass data table
redd_carcass_data <- all_surveys |>
  mutate(coverage_start = temporal_coverage,
         coverage_end = temporal_coverage) |>
  select(id, upstream_rkm, downstream_rkm, species, coverage_start, coverage_end) |>
  glimpse()
# redd/carcass location table
redd_carcass_location <- all_surveys |>
  mutate(survey_type = data_type,
         survey_species = species,
         life_stage = NA) |>
  select(stream, agency, survey_species, life_stage, survey_type, sub_basin) |> # TODO check - note that huc8 was replaced with sub-basin
  glimpse()
# fields needed for location table
# reach, upstream_rkm, upstream_latitude, upstream_longitude, downstream_rkm, downstream_latitude, downstream_longitude

# escapement modeled data ----
klamath_project_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")

klamath_mainstem_fall_chinook_escapement <-  klamath_project_board |>
  pin_read("klamath_mainstem_fall_chinook_escapement") |> glimpse()

#TODO - filter so that it matches megatable

# modeled population data ----
cdfw_population_raw <- read_xlsx(here::here("data-raw", "tables_with_data", "modeled", "Salmonid_Population_Monitoring_Data_CMPv2023.xlsx"), sheet = "Population Data")

klamath_cdfw_population_raw <- cdfw_population_raw |>
  filter(Watershed %in% c("Trinity River", "Scott River", "Shasta River", "Lower Klamath","Klamath River"))

klamath_cdfw_population_processed <- klamath_cdfw_population_raw |>
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
# usethis::use_data(rst_sites, overwrite = TRUE)
# usethis::use_data(hatcheries, overwrite = TRUE)
# usethis::use_data(redd_carcass_data, overwrite = TRUE)
# usethis::use_data(redd_carcass_location, overwrite = TRUE)
