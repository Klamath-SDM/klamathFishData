library(pdftools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
library(purrr)


pdf_path <- "data-raw/Avian_Predation_on_UKB_Suckers_Summary_Report_2021_2023.pdf"
tables <- extract_tables(pdf_path, method = "stream", output = "tibble")

#### PIT-tagged LRS, SNS, KLS, SNS-KLS and SARP ----
tag_data_raw <- tables[[2]] |> glimpse()

tag_data_raw <- as.data.frame(tag_data_raw, stringsAsFactors = FALSE) %>%
  clean_names()

# The first number = the number of PIT-tagged fish available to avian predators that year in that location.
# This is essentially how many tagged suckers (or salmon) were released or present in the waterbody.
# The number in parentheses = the number of those tags recovered from avian predator nesting colonies in the same year.
# Recovery = physical detection of PIT tags on bird colonies (e.g., cormorants, pelicans) after birds consumed tagged fish.
# Importantly, these are minimum counts of predation events — not corrected for imperfect detection or deposition, so the true number of fish eaten could be higher.
parse_avail <- function(x) {
  # available number before parentheses, allowing commas
  v <- str_extract(x, "^[0-9,]+")
  v[grepl("^\\s*NR\\b", x, ignore.case = TRUE)] <- NA_character_
  as.numeric(str_replace_all(v, ",", ""))
}

parse_recovered <- function(x) {
  # number inside parentheses
  v <- str_extract(x, "(?<=\\()[0-9]+(?=\\))")
  v[grepl("^\\s*NR\\b", x, ignore.case = TRUE)] <- NA_character_
  as.numeric(v)
}

# split cols
tag_data_split <- tag_data_raw |>
  mutate(across(
    starts_with("x20"),
    list(available = ~ parse_avail(.x),
         recovered = ~ parse_recovered(.x)),
    .names = "{.col}_{.fn}"))

# remove the original columns
tag_data_split <- tag_data_split |>
  select(-matches("^x20\\d{2}$"))


tag_data_clean <- tag_data_split |>
  mutate(location_1 = case_when(
    row_number() %in% 1:9  ~ "Upper Klamath Lake",
    row_number() %in% 10    ~ "Klamath River",
    row_number() %in% 11:13 ~ "Clear Lake Reservoir",
    row_number() %in% 14 ~ "Sheepy Lake",
    TRUE ~ location_1)) |>
  slice(-c(5))

# year columns
year_cols <- grep("^x20\\d{2}_(available|recovered)$", names(tag_data_split), value = TRUE)

avian_predation_pit_tag <- tag_data_clean |>
  mutate(fish_group_2 = na_if(str_trim(fish_group_2), "")) |>
  filter(!(is.na(fish_group_2) & rowSums(!is.na(across(all_of(year_cols)))) == 0)) |>
  pivot_longer(cols = all_of(year_cols),
               names_to   = c("year", "metric"),
               names_pattern = "^x(20\\d{2})_(available|recovered)$",
               values_to  = "value") |>
  mutate(year  = as.integer(year),
         value = suppressWarnings(as.numeric(value))) |>
  pivot_wider(names_from  = metric,
              values_from = value) |>
  rename(location = location_1,
         fish_group = fish_group_2) |>
  arrange(location, fish_group, year) |>
  as.tibble() |>
  # mutate(recovered_rate = if_else(!is.na(available) & available > 0, recovered / available, NA_real_))
  glimpse()

# save clean data - how many tagged suckers were available and how many were recovered on bird colonies
# usethis::use_data(avian_predation_pit_tag, overwrite = TRUE)


#TODO decide if we want the two tables below

#### estimate predation rates - LRS, SNS, KLS, SNS-KLS and SARP (UKL and Clear Lake) ----
estimate_predation_raw <- tables[[3]] |> glimpse()

estimate_predation_raw <- as.data.frame(estimate_predation_raw, stringsAsFactors = FALSE) |>
  clean_names()

#### Estimates of predation rates PIT-tagged Sucker Assisted Rearing Program (SARP) juvenile suckers ----
estimate_predation_sarp_raw <- tables[[4]] |> glimpse()

estimate_predation_sarp_raw <- as.data.frame(estimate_predation_sarp_raw, stringsAsFactors = FALSE) |>
  clean_names()
