# Goal is to prepare a dataset related to sucker adult data. As more data are added
# this dataset may need to be broken into smaller datasets.
library(tidyverse)
library(readxl)

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

sucker_adult_estimates <- sucker_data |>
  mutate(species = case_when(species == "lost river lakeshore spawning" ~ "lost river sucker lakeshore spawning",
                             species == "lost river river spawning" ~ "lost river sucker river spawning",
                             species == "shortnose" ~ "shortnose sucker",
                             T ~ species))

usethis::use_data(sucker_adult_estimates, overwrite = TRUE)
