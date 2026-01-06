# Goal is to create a data object with juvenile abundance estimates for salmon
library(tidyverse)
library(readxl)

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
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate, source) |>
  filter(!lifestage %in% c("adult", "adult and subadult")) # remove all adult escapement as these come from the spawner_escapement data object

salmon_juvenile_abundance <- klamath_cdfw_population_processed |>
  mutate(species = case_when(species == "fall chinook" ~ "fall chinook salmon",
                             T ~ species),
         origin = case_when(origin == "natural" ~ "wild",
                            T ~ origin))

usethis::use_data(salmon_juvenile_abundance, overwrite = TRUE)
