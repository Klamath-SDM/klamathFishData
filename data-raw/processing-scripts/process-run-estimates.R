library(dplyr)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tidyverse)
library(purrr)
library(readxl)

source("data-raw/processing-scripts/read-fall-megatable-pdf.R")
source("data-raw/processing-scripts/read-spring-megatable-pdf.R")

# Fall Run megatable -----------------------------------
fall_run_estimate_clean <- filter(fall_megatable, section == "In-River Run") |>
  filter(location == "Total In-river Run") |>
  mutate(origin = "unknown") |> # TODO look into fisheries regulation literature
  glimpse()

fall_river_run <- fall_run_estimate_clean |>
  mutate(location = "klamath basin") |>
  select(-c(section, subsection)) |>
  group_by(location, year, species, origin, lifestage) |>
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(id_cols = c(location, year, species, origin),
              names_from = lifestage,
              values_from = value,
              values_fill = 0) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage),
         estimate_type = NA,
         lifestage = case_when(lifestage == "adults" ~ "adult",
                               T ~ lifestage)) |>
  rename(estimate = value) |>
  mutate(source = "CDFW Fall Chinook Salmon Megatable available here: https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=122850&inline") |>
  select(location, year, species, origin, lifestage, estimate_type, estimate, source) |>
  filter(year != "2024") |>
  glimpse()


# Spring Run megatable -----------------------------------
spring_run_size_clean <- filter(spring_megatable, section == "Run Size Estimate") |>
  rename(lifestage = category) |>
  mutate(species = "spring chinook salmon", # TODO look into fisheries regulation literature
         origin = "unknown",
         location = "klamath basin") |>
  select(-c(section, subsection)) |>
  group_by(location, year, species, origin, lifestage) |>
  summarize(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(id_cols = c(location, year, species, origin),
              names_from = lifestage,
              values_from = value,
              values_fill = 0) |>
  # pivot_wider(id_cols = c(location, year, species, origin, year), names_from = lifestage, values_from = value, values_fill = 0) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage),
         estimate_type = NA,
         lifestage = case_when(lifestage == "adults" ~ "adult",
                               T ~ lifestage)) |>
  rename(estimate = value) |>
  mutate(source = "CDFW Spring Chinook Salmon Megatable available here: https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=165311") |>
  select(location, year, species, origin, lifestage, estimate_type, estimate, source) |>
  filter(year != "2024") |>
  glimpse()



# ========================================================== #
# ===== Adding 2024 and 2025 data =========================

source("data-raw/processing-scripts/read-krtt-2026-pdf.R")

# lamath River Technical Team May 20, 2026 report -----------------------------------
krtt_2026_data_run <- filter(fall_run_2024_2025, section == "In-River Run") |> glimpse()

fall_2024_2025_run_estimates <- krtt_2026_data_run |>
  rename(estimate = value) |>
  mutate(origin = "unknown",
         location = "klamath basin",
         estimate_type = NA,
         lifestage = tolower(lifestage),
         source = "Pacific Fishery Management Council report Klamath River Fall Chinook Salmon Age-Specific Escapement, River Harvest, and Run Size Estimates, 2025 Run Klamath River Technical Team May 20,2026
         [available here:](https://www.pcouncil.org/documents/2026/06/2026-run-klamath-river-fall-chinook-salmon-age-specific-escapement-river-harvest-and-run-size-estimates-2026-run-may-20-2026.pdf/)") |>
  select(location, year, species, origin, lifestage, estimate_type, estimate, source) |>
  filter(lifestage != "total run") |>
  glimpse()


# # bind spring and fall
inriver_run_estimates <- bind_rows(spring_run_size_clean, fall_river_run, fall_2024_2025_run_estimates) |>
  distinct() |>
  glimpse()
#
# # save clean data
usethis::use_data(inriver_run_estimates, overwrite = TRUE)
