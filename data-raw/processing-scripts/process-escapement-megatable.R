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

# Additional cleaning to spawner escapement data structure -----------------------------------
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
         lifestage = tolower(lifestage))


## Spring run spawner escapement ----

# Additional Cleaning ----
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
  replace_na(list(value = 0))

## Combine spring and fall run - TODO do some checks before saving
spawner_escapement <- bind_rows(spring_spawner_escapement_clean, fall_spawner_escapement_clean)


# save clean data
usethis::use_data(spawner_escapement, overwrite = TRUE)

