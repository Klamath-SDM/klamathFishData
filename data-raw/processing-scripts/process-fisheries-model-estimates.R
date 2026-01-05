# Processing script for model output (e.g. population, survival)
library(tidyverse)
library(readxl)

# Goal is to bring together fisheries data focusing on modeled estimates, not raw count
# This table currently combines species, lifestage etc. and may be broken out into
# more specific tables like was done for spawner_escapement


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

# klamath mainstem fall escapement ----------------------------------------

# See GitHub issue: https://github.com/Klamath-SDM/klamathFishData/issues/37
# Currently not using this until we confirm which source is best
# escapement_raw <- read_csv(here::here("data-raw", "tables_with_data", "fall_chinook_escapement.csv"))
#
# klamath_mainstem_fall_chinook_escapement <- escapement_raw |>
#   rename(julian_year = Year,
#          estimation_method = Estimator) |>
#   separate(`Lower Upper`, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = " ") |>
#   mutate(estimate = gsub(",","",estimate),
#          estimate = gsub(" ", "",estimate),
#          estimate = as.numeric(estimate),
#          lower_bounds_estimate = as.numeric(gsub(",","",lower_bounds_estimate)),
#          upper_bounds_estimate = as.numeric(gsub(",","",upper_bounds_estimate)),
#          estimation_method = tolower(estimation_method),
#          species = "fall chinook",
#          stream = "klamath river",
#          source = "[Gough, S. A., C. Z. Romberger, and N. A. Som. 2018. Fall Chinook Salmon Run Characteristics and Escapement in the Mainstem Klamath River below Iron Gate Dam, 2017. U.S. Fish and Wildlife Service. Arcata Fish and Wildlife Office, Arcata Fisheries Data Series Report Number DS 2018–58, Arcata, California](https://www.fws.gov/sites/default/files/documents/2017%20klamath%20spawn%20survey%20report%202017%20FINAL1.pdf)")

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

# combine data ------------------------------------------------------------

fisheries_model_estimates <- bind_rows(klamath_cdfw_population_processed, sucker_data) |>
  mutate(species = case_when(species == "fall chinook" ~ "fall chinook salmon",
                             species == "lost river lakeshore spawning" ~ "lost river sucker lakeshore spawning",
                             species == "lost river river spawning" ~ "lost river sucker river spawning",
                             species == "shortnose" ~ "shortnose sucker",
                             T ~ species),
        origin = case_when(origin == "natural" ~ "wild",
                                     T ~ origin))

# save data ---------------------------------------------------------------

# save locally
usethis::use_data(fisheries_model_estimates, overwrite = TRUE)

