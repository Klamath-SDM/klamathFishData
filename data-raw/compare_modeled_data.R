library(tidyverse)
library(pins)
library(dplyr)
library(ggplot2)

# The goal of this script is to figure out if CDFW modeled data and Ashley's prepared data are duplicates

klamath_project_board <- pins::board_s3(bucket = "klamath-sdm", region = "us-east-1")


# coho_juvenile_raw <-  klamath_project_board |>
#   pin_read("juvenile_raw") |> glimpse()
#
# coho_adult_raw <-  klamath_project_board |>
#   pin_read("spawner_raw")
#
# sucker_survival_raw <- klamath_project_board |>
#   pin_read("survival_raw")

# model-output
load(file = "data/model_output.rda")

sort(unique(model_output$julian_year))

model_output <- model_output |>
  mutate(lower_bounds_estimate = as.numeric(lower_bounds_estimate),
         upper_bounds_estimate = as.numeric(upper_bounds_estimate))

# CDFW
klamath_cdfw_population_processed <-  klamath_project_board |>
  pin_read("klamath_cdfw_population_processed") |> glimpse()
sort(unique(klamath_cdfw_population_processed$julian_year))



model_output <- model_output |> dplyr::mutate(source = "model")
klamath_cdfw_population_processed <- klamath_cdfw_population_processed |> dplyr::mutate(source = "cdfw")

combined <- dplyr::bind_rows(model_output, klamath_cdfw_population_processed)

dupes <- combined |> dplyr::select(-source) |> duplicated() |> which()

# See duplicated rows
combined[dupes, ]

# there are defenitly duplicates, will plot to see if it helps for details

model_output <- model_output |> mutate(source = "model")
klamath_cdfw_population_processed <- klamath_cdfw_population_processed |> mutate(source = "cdfw")

combined <- bind_rows(model_output, klamath_cdfw_population_processed)

combined <- combined |>
  mutate(row_id = paste(julian_year, stream, species, lifestage, sex, estimate_type, estimate)) |>
  group_by(row_id) |>
  mutate(overlap = n() > 1) |>
  ungroup()

# overlapping by year
combined |>
  count(julian_year, overlap) |>
  ggplot(aes(x = julian_year, y = n, fill = overlap)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "tomato"),
                    labels = c("FALSE" = "Unique", "TRUE" = "Overlap")) +
  labs(x = "Year", y = "Count", fill = "Overlap?",
       title = "Overlap of Estimates by Year") +
  theme_minimal()


combined |>
  count(julian_year, stream, overlap) |>
  ggplot(aes(x = julian_year, y = stream, fill = overlap)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey90")) +
  labs(title = "Overlap of Estimates by Stream and Year", fill = "Overlap?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined |>
  filter(overlap == TRUE) |>
  ggplot(aes(x = julian_year, y = estimate, color = stream)) +
  geom_point() +
  labs(title = "Estimates Present in Both Datasets",
       y = "Estimate", x = "Year") +
  theme_minimal()

