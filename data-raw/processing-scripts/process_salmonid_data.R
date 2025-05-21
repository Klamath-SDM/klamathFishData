library(tidyr)
library(dplyr)
library(janitor)
library(sf)


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
    rename(sub_basin = !!sub_basin_col)
  if (is_point) {
    coords <- st_coordinates(sf_data)
    sf_data[[lon_col]] <- coords[, 1]
    sf_data[[lat_col]] <- coords[, 2]
    sf_data <- st_drop_geometry(sf_data)
  }
  return(sf_data)
}


# habitat data ---- #TODO check with Maddee on source of this table
habitat_data <- read_csv(here::here('data-raw','tables_with_data','habitat_data.csv')) |>
  clean_names() |>
  mutate(longitude = as.numeric(longtidue)) |>
  rename(stream = river) |>
  assign_sub_basin(sub_basin) |>
  select(-longtidue) |>
  select(stream, sub_basin, everything()) |>
  glimpse()

# RST ----
rst_sites <- read_csv(here::here('data-raw','tables_with_data', 'rst_sites.csv')) |>
  clean_names() |>
  mutate(data_type = "RST data",
         stream = paste(watershed, "River")) |>
  assign_sub_basin(sub_basin) |>
  select(stream, sub_basin, data_type, rst_name, operator, latitude, longitude, link) |>
  glimpse()

# hatcheries ----
hatcheries <- read_csv(here::here('data-raw','tables_with_data', 'fish_hatchery_locations.csv')) |>
  clean_names() |>
  mutate(stream = paste(watershed, "River"),
         data_type = "hatchery") |>
  rename(agency = operator) |>
  assign_sub_basin(sub_basin) |>
  select(-c(google_earth_location,  watershed)) |>
  select(stream, sub_basin, data_type, everything()) |>
  glimpse()

# save rda files
usethis::use_data(habitat_data, overwrite = TRUE)
usethis::use_data(rst_sites, overwrite = TRUE)
usethis::use_data(hatcheries, overwrite = TRUE)
