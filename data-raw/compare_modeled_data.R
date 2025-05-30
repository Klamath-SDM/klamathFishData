library(tidyverse)
library(pins)
library(dplyr)
library(ggplot2)
library(readxl)

# The goal of this script is to figure out if CDFW modeled data and Ashley's prepared data are duplicates

klamath_project_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")

# model-output
load(file = "data/model_output.rda")

sort(unique(model_output$julian_year))

model_output <- model_output |>
  mutate(lower_bounds_estimate = as.numeric(lower_bounds_estimate),
         upper_bounds_estimate = as.numeric(upper_bounds_estimate),
         stream = case_when(stream == "shasta" ~ "shasta river",
                            T ~ stream)) |>
  glimpse()

# CDFW
klamath_cdfw_population_processed <-  klamath_project_board |>
  pin_read("klamath_cdfw_population_processed") |> glimpse()
sort(unique(klamath_cdfw_population_processed$julian_year))



model_output <- model_output |> dplyr::mutate(source = "model")
klamath_cdfw_population_processed <- klamath_cdfw_population_processed |> dplyr::mutate(source = "cdfw")

combined <- dplyr::bind_rows(model_output, klamath_cdfw_population_processed)

dupes <- combined |> dplyr::select(-source) |> duplicated() |> which()

# See duplicated rows
combined[dupes, ]

# there are defenitly duplicates, will plot to see if it helps for details

model_output <- model_output |> mutate(source = "model")
klamath_cdfw_population_processed <- klamath_cdfw_population_processed |> mutate(source = "cdfw")

combined <- bind_rows(model_output, klamath_cdfw_population_processed)

combined <- combined |>
  mutate(row_id = paste(julian_year, stream, species, lifestage, sex, estimate_type, estimate)) |>
  group_by(row_id) |>
  mutate(overlap = n() > 1) |>
  ungroup()

# the only data entries that show overlap are all cdfw data

#reading in klamath_mainstem_fall_chinook_escapement to compare to mega table

klamath_mainstem_fall_chinook_escapement <-  klamath_project_board |>
  pin_read("klamath_mainstem_fall_chinook_escapement") |> glimpse()

# fall chinook adult count estimates do match mega table, however:
# Note that data only covers shasta and scott river
# There are some years with two data entries for the same river (mostly shasta river), only one of those matches the mega table
# after looking into data, I think this is related to cdfw raw data column "origin", it should be "mixed" in order to match megatable

klamath_cdfw_population_processed |> filter(species == "fall chinook salmon" & estimate_type == "count") |> view()

# adding the code from KlamathEDA modeled-fisheries-data.Rmd below

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
