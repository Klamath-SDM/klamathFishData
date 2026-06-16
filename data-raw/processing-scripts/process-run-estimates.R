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
  glimpse()

# # bind spring and fall
inriver_run_estimates <- bind_rows(spring_run_size_clean, fall_river_run) |>
  distinct() |>
  glimpse()
#
# # save clean data
usethis::use_data(inriver_run_estimates, overwrite = TRUE)

