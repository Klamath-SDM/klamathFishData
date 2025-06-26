# Processing script for model output (e.g. population, survival)
library(tidyverse)
library(pins)

# pull raw data -----------------------------------------------------------
# define AWS data bucket
# note that you need to set up access keys in R environ
klamath_project_board <- pins::board_s3(
  bucket="klamath-sdm",
  access_key=Sys.getenv("aws_access_key_id"),
  secret_access_key=Sys.getenv("secret_access_key_id"),
  session_token = Sys.getenv("session_token_id"),
  region = "us-east-1"
)

# Exploratory analysis: https://github.com/Klamath-SDM/KlamathEDA/blob/main/data-raw/fisheries/modeled/modeled-fisheries-data.md
sucker_survival_raw <- klamath_project_board |>
  pin_read("sucker_survival_model")

cdfw_population_raw <- klamath_project_board |>
  pin_read("klamath_cdfw_population_processed")

klamath_mainstem_fall_escapement_raw <- klamath_project_board |>
  pin_read("klamath_mainstem_fall_chinook_escapement")

# process data ------------------------------------------------------------
# Data schema is here: https://lucid.app/lucidchart/347f8e9d-1a80-4d4c-a02b-4400def18031/edit?viewport_loc=-304%2C-126%2C1514%2C1427%2C0_0&invitationId=inv_80bba736-9f3f-475b-920c-2cc754343974

population_data <- cdfw_population_raw |>
  bind_rows(klamath_mainstem_fall_escapement_raw |>
              mutate(lifestage = "adult",
                     sex = NA,
                     estimate_type = "abundance",
                     confidence_interval = 95,
                     is_complete_estimate = NA))

sucker_data <- sucker_survival_raw |>
  select(sex, year, population, apparent_survival_CI, apparent_survival_estimate) |>
  filter(!apparent_survival_estimate %in% c("B","C")) |> # remove the B and C because they seem like estimates that have issues
  separate(apparent_survival_CI, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = "-") |>
  mutate(estimate_type = "apparent survival",
         apparent_survival_estimate = as.numeric(apparent_survival_estimate)) |>
  rename(estimate = apparent_survival_estimate) |>
  bind_rows(sucker_survival_raw |>
    select(sex, year, population, seniority_probability_estimate, seniority_probability_CI) |>
      filter(!seniority_probability_estimate %in% c("B","C")) |> # remove the B and C because they seem like estimates that have issues
      separate(seniority_probability_CI, into = c("lower_bounds_estimate", "upper_bounds_estimate"), sep = "-") |>
      mutate(estimate_type = "seniority probability",
             seniority_probability_estimate = as.numeric(seniority_probability_estimate)) |>
      rename(estimate = seniority_probability_estimate)
  ) |>
  bind_rows(sucker_survival_raw |>
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
           is_complete_estimate = T) |>
  select(julian_year, stream, species, lifestage, sex, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate)

fisheries_model_estimates <- bind_rows(population_data, sucker_data)
# save data ---------------------------------------------------------------

# save locally
save(fisheries_model_estimates, file = "data/fisheries_model_estimates.rda")

# # save to s3 storage
# klamath_project_board |> pins::pin_write(fisheries_model_estimates,
#                                          type = "csv",
#                                          title = "model output")
