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

coho_juvenile_raw <-  klamath_project_board |>
  pin_read("juvenile_raw")

coho_adult_raw <-  klamath_project_board |>
  pin_read("spawner_raw")

sucker_survival_raw <- klamath_project_board |>
  pin_read("survival_raw")

fall_escapement_raw <- klamath_project_board |>
  pin_read("escapement_processed")

# process data ------------------------------------------------------------
# Data schema is here: https://lucid.app/lucidchart/347f8e9d-1a80-4d4c-a02b-4400def18031/edit?viewport_loc=-304%2C-126%2C1514%2C1427%2C0_0&invitationId=inv_80bba736-9f3f-475b-920c-2cc754343974

# TODO we need to fill in estimation method for adult data

population_data <- coho_juvenile_raw |>
  rename(estimate = juvenile_abundance) |>
  bind_rows(coho_adult_raw |>
              rename(year = adult_year,
                     estimate = spawners) |>
              mutate(lifestage = "adult",
                     temp = as_date(weir_removed),
                     is_complete_estimate = case_when(month(temp) %in% 11:12 & year(temp) < 2025 ~ F,
                                                      T ~ T)) |>
              select(-c(temp, weir_removed))) |>
  rename(julian_year = year) |>
  mutate(species = "coho",
         estimate_type = "abundance",
         lower_bounds_estimate = NA,
         upper_bounds_estimate = NA,
         confidence_interval = NA,
         sex = NA,
         is_complete_estimate = ifelse(is.na(is_complete_estimate), T, is_complete_estimate)) |>
  select(julian_year, stream, species, lifestage, sex, estimate_type, estimate, confidence_interval, lower_bounds_estimate, upper_bounds_estimate,
         estimation_method, is_complete_estimate)

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
           is_complete_estimate = T) |>
  select(julian_year, stream, species, lifestage, sex, estimate_type, estimate, confidence_interval,
         lower_bounds_estimate, upper_bounds_estimate, estimation_method, is_complete_estimate)

fall_escapement <- fall_escapement_raw |>
  mutate(lifestage = "adult",
         sex = NA,
         estimate_type = "abundance",
         confidence_interval = 95,
         is_complete_estimate = T)

fisheries_model_estimates <- bind_rows(population_data, sucker_data, fall_escapement)
# save data ---------------------------------------------------------------

# save locally
save(fisheries_model_estimates, file = "data/fisheries_model_estimates.rda")

# # save to s3 storage
# klamath_project_board |> pins::pin_write(fisheries_model_estimates,
#                                          type = "csv",
#                                          title = "model output")
