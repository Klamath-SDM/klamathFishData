# Processing script for model output (e.g. population, survival)
library(tidyverse)
library(pins)
library(readxl)

# Exploratory analysis: https://github.com/Klamath-SDM/KlamathEDA/blob/main/data-raw/fisheries/modeled/modeled-fisheries-data.md
# Moved processing from that analysis here


# cdfw population table ---------------------------------------------------
cdfw_population_raw <- read_xlsx(here::here("data-raw", "tables_with_data", "Salmonid_Population_Monitoring_Data_CMPv2023.xlsx"), sheet = "Population Data")

klamath_cdfw_population_raw <- cdfw_population_raw |>
  filter(Watershed %in% c("Trinity River", "Scott River", "Shasta River", "Lower Klamath","Klamath River"))

klamath_cdfw_population_processed <- klamath_cdfw_population_raw |>
  janitor::clean_names() |>
  rename(stream = population,
         lifestage = life_stage) |>
  filter(!id %in% c(4528, 4529)) |>
  mutate(species = ifelse(!is.na(run_designation), tolower(paste0(run_designation, " ", species)), tolower(species)),
         origin = tolower(origin),
         julian_year = as.numeric(ifelse(!is.na(brood_year), brood_year, survey_season)),
         stream = tolower(stream),
         stream = ifelse(stream == "lower klamath", "lower klamath river", stream),
         lifestage = tolower(lifestage),
         estimate_type = tolower(metric),
         estimation_method = tolower(estimation_method),
         sex = NA,
         estimate = value,
         confidence_interval = 95,
         lower_bounds_estimate = x95_lower_ci,
         upper_bounds_estimate = x95_upper_ci,
         is_complete_estimate = ifelse(full_population_estimate == "N", F, T),
         julian_year = case_when(stream == "trinity river" & value == 3459 ~ 2020,
                                 stream == "trinity river" & value == 1936 ~ 2021,
                                 T ~ julian_year),
         source = "These data were downloaded from California Department of Fish and Wildlife (CDFW)
[document library](https://www.nrm.dfg.ca.gov/documents/ContextDocs.aspx?cat=Fisheries--AnadromousSalmonidPopulationMonitoring)") |>
  select(julian_year, stream, species, origin, lifestage, sex, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate, source)

#klamath_cdfw_population_processed |> group_by(julian_year, species, origin, stream, lifestage, sex, estimate_type) |> tally() |> filter(n>1)


# klamath mainstem fall escapement ----------------------------------------

escapement_raw <- read_csv(here::here("data-raw", "tables_with_data", "fall_chinook_escapement.csv"))

klamath_mainstem_fall_chinook_escapement <- escapement_raw |>
  rename(julian_year = Year,
         estimation_method = Estimator) |>
  separate(`Lower Upper`, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = " ") |>
  mutate(estimate = gsub(",","",estimate),
         estimate = gsub(" ", "",estimate),
         estimate = as.numeric(estimate),
         lower_bounds_estimate = as.numeric(gsub(",","",lower_bounds_estimate)),
         upper_bounds_estimate = as.numeric(gsub(",","",upper_bounds_estimate)),
         estimation_method = tolower(estimation_method),
         species = "fall chinook",
         stream = "klamath river",
         source = "[Gough, S. A., C. Z. Romberger, and N. A. Som. 2018. Fall Chinook Salmon Run Characteristics and Escapement in the Mainstem Klamath River below Iron Gate Dam, 2017. U.S. Fish and Wildlife Service. Arcata Fish and Wildlife Office, Arcata Fisheries Data Series Report Number DS 2018–58, Arcata, California](https://www.fws.gov/sites/default/files/documents/2017%20klamath%20spawn%20survey%20report%202017%20FINAL1.pdf)")

# sucker survival ---------------------------------------------------------

sucker_survival_model <- read_csv(here::here("data-raw", "tables_with_data", "sucker_survival.csv"))

sucker_data <- sucker_survival_model |>
  select(sex, year, population, apparent_survival_CI, apparent_survival_estimate) |>
  filter(!apparent_survival_estimate %in% c("B","C")) |> # remove the B and C because they seem like estimates that have issues
  separate(apparent_survival_CI, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = "-") |>
  mutate(estimate_type = "apparent survival",
         apparent_survival_estimate = as.numeric(apparent_survival_estimate)) |>
  rename(estimate = apparent_survival_estimate) |>
  bind_rows(sucker_survival_model |>
              select(sex, year, population, seniority_probability_estimate, seniority_probability_CI) |>
              filter(!seniority_probability_estimate %in% c("B","C")) |> # remove the B and C because they seem like estimates that have issues
              separate(seniority_probability_CI, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = "-") |>
              mutate(estimate_type = "seniority probability",
                     seniority_probability_estimate = as.numeric(seniority_probability_estimate)) |>
              rename(estimate = seniority_probability_estimate)
  ) |>
  bind_rows(sucker_survival_model |>
              select(sex, year, population, annual_population_rate_of_change_estimate, annual_population_rate_of_change_CI) |>
              filter(!annual_population_rate_of_change_estimate %in% c("B","C")) |> # remove the B and C because they seem like estimates that have issues
              separate(annual_population_rate_of_change_CI, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = "-") |>
              mutate(estimate_type = "annual population rate of change") |>
              rename(estimate = annual_population_rate_of_change_estimate)
  ) |>
  rename(julian_year = year,
         species = population) |>
  mutate(confidence_interval = 95,
         sex = tolower(sex),
         stream = "upper klamath lake",
         lifestage = "adult",
         estimation_method = "Cormark-Jolly-Seber model",
         lower_bounds_estimate = as.numeric(lower_bounds_estimate),
         upper_bounds_estimate = as.numeric(upper_bounds_estimate),
         is_complete_estimate = T,
         source = "[Hewitt, D.A., Janney, E.C., Hayes, B.S., and Harris, A.C., 2018, Status and trends of adult Lost River (Deltistes luxatus) and shortnose (Chasmistes brevirostris) sucker populations in Upper Klamath Lake, Oregon, 2017: U.S. Geological Survey Open-File Report 2018-1064, 31 p., https://doi.org/10.3133/ofr20181064.](https://pubs.usgs.gov/of/2018/1064/ofr20181064.pdf)") |>
  select(julian_year, stream, species, lifestage, sex, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate, source)
# megatable ---------------------------------------------------------------

# add in data from the megatables to make it more usable for modeling
# don't add in-river harvest or in-river run for now
# see megatable_processing.R

load("data/megatable_data.rda")

megatable_for_modeling <- megatable_data |>
  filter(lifestage == "Adults", section == "Spawning Escapement", !is.na(value), !location %in% c("Total In-river Run","Catch and Release Mortality gg/","Net Mortality (8.70% of harvest)  f/","Angling Mortality (2.04% of harvest)f/","In-river Harvest and Escapement","Totals","Subtotals", "Angler Harvest Subtotals:","Indian Net Harvest Subtotals:", "Total Spawner Escapement","Klamath Natural Spawner Subtotals:", "Hatchery Spawner Subtotals:", "Natural Spawner Subtotals", "Trinity Natural Spawner Subtotals:" )) |>
  mutate(origin = ifelse(location %in% c("Iron Gate Hatchery  (IGH)","Trinity River Hatchery (TRH)"), "hatchery", "natural"),
         stream = case_when(location == "Salmon River basin" ~ "salmon river",
                            location == "Scott River basin" ~ "scott river",
                            location == "Shasta River basin" ~ "shasta river",
                            location == "Bogus Creek basin" ~ "bogus creek",
                            location %in% c("Iron Gate Hatchery  (IGH)","Trinity River Hatchery (TRH)") ~ tolower(location),
                            location %in% c("Main Stem Klamath Rivern/ (excluding IGH)", "Main Stem Klamath River (excluding IGH)","Main Stem Klamath Rivern/ (excluding IGH)") ~ "klamath river",
                            location %in% c("Klamath River (below Hwy 101 bridge)","Klamath River (Weitchpec to IGH)","Klamath River (Hwy 101 to Trinity mouth)","Klamath River (Hwy 101 to Weitchpec)" ) ~ "lower klamath river",
                            location %in% c("Trinity River (Hoopa Reservation)", "Main Stem Trinity Riverdd/ (excluding TRH)","Trinity River basin above Weitchpec aa/", "Trinity River basin (above Willow Creek, excluding TRH)","Main Stem Trinity Riverdd/ (excluding TRH)") ~ "trinity river",
                            location %in% c("Misc. Klamath tributaries (above Hoopa and Yurok Reservations)",
                                            "Misc. Klamath-Trinity tributaries (above Hoopa and Yurok Reservations)",
                                            "Misc. Klamath tributarieso/ (above Yurok Reservation)",
                                            "Misc. Trinity tributaries  o/ (above Hoopa Reservation)",
                                            "Misc. Klamath tributarieso/ii/ (above Yurok Reservation)")  ~ "other tributaries",
                            location %in% c("Hoopa and Yurok Reservation tribs.", "Yurok Reservation tribs. (Klamath River) p/","Hoopa Reservation tribs.  (Trinity River) p/") ~ "yurok and hoopa reservation tribs"),
         estimate_type = "abundance",
         lifestage = "spawner") |>
  rename(julian_year = year,
         estimate = value) |>
  select(-c(location, subsection, section)) |>
  group_by(lifestage, julian_year, species, stream, origin, estimate_type) |>
  summarize(estimate = sum(estimate)) |>
  mutate(source = "[CDFW Megatable](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment)")

# combine data ------------------------------------------------------------
# Data schema is here: https://lucid.app/lucidchart/347f8e9d-1a80-4d4c-a02b-4400def18031/edit?viewport_loc=-304%2C-126%2C1514%2C1427%2C0_0&invitationId=inv_80bba736-9f3f-475b-920c-2cc754343974

population_data <- klamath_cdfw_population_processed |>
  bind_rows(klamath_mainstem_fall_chinook_escapement |>
              mutate(lifestage = "adult",
                     sex = NA,
                     estimate_type = "abundance",
                     confidence_interval = 95,
                     is_complete_estimate = NA))

#population_data |> group_by(julian_year, stream, species, origin, lifestage, sex, estimate_type) |> tally() |> filter(n>1)

#megatable overlap: scott, shasta,klamath 2001-2017 fill megatable beyond that, trinity 2002-2022
fisheries_model_estimates <- bind_rows(population_data, sucker_data,
                                       megatable_for_modeling |>
                                         mutate(remove = case_when(stream %in% c("scott river","shasta river") ~ "remove",
                                                                   stream == "klamath" & julian_year %in% 2001:2017 ~ "remove",
                                                                   stream == "trinity" & julian_year %in% 2002:2022 ~ "remove")) |>
                                         filter(is.na(remove))) |>
  mutate(species = ifelse(species == "fall chinook", "fall chinook salmon", species)) |>
  select(-remove)

#fisheries_model_estimates |> group_by(julian_year, stream, species, origin, lifestage, sex, estimate_type) |> tally() |> filter(n>1)

# save data ---------------------------------------------------------------

# save locally
usethis::use_data(fisheries_model_estimates, overwrite = TRUE)

# # save to s3 storage
# klamath_project_board |> pins::pin_write(fisheries_model_estimates,
#                                          type = "csv",
#                                          title = "model output")
