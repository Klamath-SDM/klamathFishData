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

source("data-raw/processing-scripts/read_fall_megatable_pdf.R")
source("data-raw/processing-scripts/read_spring_megatable_pdf.R")

# Goal is to create a data object for spawner escapement data, which primarily comes from
# the Megatable for fall and spring Chinook. There are, however, other data sources
# and to make this table most useful we combine all escapement data sources. If data are
# overlapping, choose the best and most representative.


# Fall Run megatable -----------------------------------
fall_spawner_escapement_clean <- filter(fall_megatable, section == "Spawning Escapement") |>
  filter(!location %in% c("Subtotals", "Hatchery Spawner Subtotals:",
                          "Total Spawner Escapement","Natural Spawner Subtotals",
                          "Angler Harvest","Angler Harvest Subtotals:","Indian Net Harvest  e/",
                          "Indian Net Harvest Subtotals:", "Totals","In-river Harvest and Escapement",
                          "Angling Mortality (2.04% of harvest)f/","Net Mortality (8.70% of harvest)  f/",
                          "Catch and Release Mortality gg/", "Total In-river Run",
                          "Klamath River (below Hwy 101 bridge)", "Klamath River (Hwy 101 to Weitchpec)",
                          "Klamath River (Weitchpec to IGH)", "Trinity River basin above Weitchpec aa/",
                          "Trinity River (Hoopa Reservation)", "Klamath River (Hwy 101 to Trinity mouth)")) |>
  mutate(origin = case_when(subsection == "Hatchery Spawners" ~ "hatchery",
                            T ~ "wild"),
         location = case_when(location == "Iron Gate Hatchery  (IGH)" ~ "Iron Gate Hatchery",
                              location == "Trinity River Hatchery (TRH)" ~ "Trinity River Hatchery",
                              location %in% c("Trinity River basin (above Willow Creek, excluding TRH)", "Main Stem Trinity Riverdd/ (excluding TRH)") ~ "Trinity River",
                              location == "Salmon River basin" ~ "Salmon River",
                              location == "Scott River basin" ~ "Scott River",
                              location == "Shasta River basin" ~ "Shasta River",
                              location == "Bogus Creek basin" ~ "Bogus Creek",
                              location == "Main Stem Klamath River (excluding IGH)" ~ "Klamath River",
                              location == "Misc. Klamath tributaries (above Hoopa and Yurok Reservations)" ~ "Other Klamath Tributaries",
                              location == "Hoopa and Yurok Reservation tribs." ~ "Hoopa and Yurok Tributaries",
                              location == "Misc. Klamath-Trinity tributaries (above Hoopa and Yurok Reservations)" ~ "Other Klamath Trinity Tributaries",
                              location == "Main Stem Klamath Rivern/ (excluding IGH)" ~ "Klamath River",
                              location %in% c("Misc. Klamath tributarieso/ (above Yurok Reservation)", "Misc. Klamath tributarieso/ii/ (above Yurok Reservation)") ~ "Other Klamath Tributaries",
                              location == "Yurok Reservation tribs. (Klamath River) p/" ~ "Yurok Klamath Tributaries",
                              location == "Klamath Natural Spawner Subtotals:" ~ "Klamath Basin",
                              location == "Misc. Trinity tributaries  o/ (above Hoopa Reservation)" ~ "Other Trinity Tributaries",
                              location == "Hoopa Reservation tribs.  (Trinity River) p/" ~ "Hoopa Trinity Tributaries",
                              location == "Trinity Natural Spawner Subtotals:" ~ "Trinity Basin",
                              T ~ location)) |>
  select(-c(section, subsection)) |>
  pivot_wider(id_cols = c(location, year, species, origin), names_from = lifestage, values_from = value, values_fill = 0) |>
  # fix missing Grilse values
  mutate(Grilse = ifelse(is.na(Grilse), Totals - Adults, Grilse)) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage)) |>
  rename(estimate = value) |>
  mutate(source = "CDFW Fall Chinook Salmon Megatable available here: https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=122850&inline")


# Spring Run megatable -----------------------------------
spring_spawner_escapement_clean <- spring_spawner_escapement |>
  rename(lifestage = category) |>
  mutate(species = "spring chinook salmon") |>
  filter(!location %in% c("Trinity River Basin","Klamath River Basin", "Subtotals")) |>
  mutate(origin = case_when(subsection == "Hatchery Spawners" ~ "hatchery",
                            T ~ "wild"),
         location = case_when(location == "Trinity River Hatchery (TRH) b/" ~ "Trinity River Hatchery",
                              location == "South Fork e/" ~ "South Fork Trinity River",
                              location == "Above JCW, excluding TRH b/" ~ "Trinity River",
                              location == "Misc. Tribs. f/" ~ "Other Trinity Tributaries",
                              location == "Misc. Tribs. d/" ~ "Other Klamath Tributaries",
                              T ~ location)) |>
  select(-c(section, subsection)) |>
  pivot_wider(id_cols = c(location, year, species, origin), names_from = lifestage, values_from = value, values_fill = 0) |>
  # fix missing Grilse values
  mutate(Grilse = ifelse(is.na(Grilse), Totals - Adults, Grilse)) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage)) |>
  replace_na(list(value = 0)) |>
  rename(estimate = value) |>
  mutate(source = "CDFW Spring Chinook Salmon Megatable available here: https://casalmon.org/wp-content/uploads/2024/08/FINAL-2023-Spring-Chinook-Megatable-v.28-Mar-2024.pdf")


# Coho, Steelhead from CDFW ----------------------------------------------------------

# Coho and steelhead are not included in the Megatable and are pulled from CDFW data table

cdfw_population_raw <- read_xlsx(here::here("data-raw", "tables_with_data", "Salmonid_Population_Monitoring_Data_CMPv2023.xlsx"), sheet = "Population Data")

klamath_cdfw_population_raw <- cdfw_population_raw |>
  filter(Watershed %in% c("Trinity River", "Scott River", "Shasta River", "Lower Klamath","Klamath River"))

klamath_cdfw_population_processed <- klamath_cdfw_population_raw |>
  janitor::clean_names() |>
  rename(location = population,
         lifestage = life_stage) |>
  filter(!id %in% c(4528, 4529)) |>
  mutate(species = ifelse(!is.na(run_designation), tolower(paste0(run_designation, " ", species)), tolower(species)),
         origin = tolower(origin),
         year = as.numeric(ifelse(!is.na(brood_year), brood_year, survey_season)),
         location = tolower(location),
         location = ifelse(location == "lower klamath", "lower klamath river", location),
         lifestage = tolower(lifestage),
         estimate_type = tolower(metric),
         estimation_method = tolower(estimation_method),
         estimate = value,
         confidence_interval = 95,
         lower_bounds_estimate = x95_lower_ci,
         upper_bounds_estimate = x95_upper_ci,
         is_complete_estimate = ifelse(full_population_estimate == "N", F, T),
         year = case_when(location == "trinity river" & value == 3459 ~ 2020,
                                 location == "trinity river" & value == 1936 ~ 2021,
                                 T ~ year),
         source = "These data were downloaded from California Department of Fish and Wildlife (CDFW)
[document library](https://www.nrm.dfg.ca.gov/documents/ContextDocs.aspx?cat=Fisheries--AnadromousSalmonidPopulationMonitoring)") |>
  select(year, location, species, origin, lifestage, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate, source) |>
  filter(lifestage%in% c("adult", "adult and subadult"), species %in% c("coho salmon", "winter steelhead", "steelhead"))


## Combine spring and fall run - TODO do some checks before saving
spawner_escapement <- bind_rows(klamath_cdfw_population_processed,
                                spring_spawner_escapement_clean,
                                fall_spawner_escapement_clean) |>
  mutate(origin = case_when(origin == "natural" ~ "wild",
                            T ~ origin),
         lifestage = case_when(lifestage == "adults" ~ "adult",
                               T ~ lifestage))

spawner_escapement |> distinct(estimation_method) |> view()
# save clean data
usethis::use_data(spawner_escapement, overwrite = TRUE)

