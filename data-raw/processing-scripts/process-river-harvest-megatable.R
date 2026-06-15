library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
library(purrr)
library(readxl)

source("data-raw/processing-scripts/read-fall-megatable-pdf.R")
source("data-raw/processing-scripts/read-spring-megatable-pdf.R")


# Fall Run megatable -----------------------------------
fall_harvest_clean <- filter(fall_megatable, section == "In-River Harvest") |>
  filter(!location %in% c("Subtotals", "Total In-river Harvest", "Angler Harvest Subtotals:",
                          "Indian Net Harvest Subtotals:", "Tribal  Harvest Subtotals:")) |>
  mutate(origin = "unknown") |> # TODO look into fisheries regulation literature
  glimpse()


fall_harvest <- fall_harvest_clean |>
  filter(!location %in% c("Balance of Klamath system", "Tribal Net Harvest  e/")) |>
  mutate(location = case_when(location %in% c("Klamath River (below Hwy 101 bridge)", "Klamath River (Hwy 101 to Trinity mouth)",
                                              "Klamath River (Hwy 101 to Coon Cr Falls)", "Klamath River (Coon Cr Falls to IGH)",
                                              "Klamath River (Hwy 101 to Weitchpec)", "Klamath River (Weitchpec to IGH)") ~ "Klamath River",
                              location %in% c("Trinity River basin (above Willow Creek)", "Trinity River (Hoopa Reservation)", "Trinity River basin above Weitchpec aa/") ~ "Trinity River",
                              T ~ location)) |>
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
spring_harvest <- filter(spring_megatable, section == "River Harvest") |>
# spring_harvest <- spring_harvest_clean |>
  rename(lifestage = category) |>
  mutate(species = "spring chinook salmon") |>
  mutate(location = case_when(
    location %in% c("yurok_tribal_harvest", "klamath_river_angler", "total_river_harvest") ~ "klamath river",
    location %in% c("hoopa_tribal_harvest", "trinity_river_angler") ~ "trinity river",
    TRUE ~ location),
    origin = "unknown") |>
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

# bind spring and fall
salmon_harvest <- bind_rows(spring_harvest, fall_harvest) |>
  distinct() |>
  glimpse()

# save clean data
usethis::use_data(salmon_harvest, overwrite = TRUE)
