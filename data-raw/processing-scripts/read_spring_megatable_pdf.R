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

# This script reads and cleans Spring run megatable

##  ---- SPAWNER ----
pdf_path <- "data-raw/2023.Spring.Chinook.Megatable.v.28-Mar-2024.pdf"
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

# Same approach than fall run megatable ----
spawner_1 <- tables_stream[[3]] # 1980 - 1982
spawner_2 <- tables_stream[[6]] # 1983 - 1985
spawner_3 <- tables_stream[[9]] # 1986 - 1988
spawner_4 <- tables_stream[[11]] # 1989 - 1991
spawner_5 <- tables_stream[[14]] # 1992 - 1994
spawner_6 <- tables_stream[[17]] # 1995 - 1997
spawner_7 <- tables_stream[[20]] # 1998 -2000
spawner_8 <- tables_stream[[23]] # 2001 - 2003
spawner_9 <- tables_stream[[26]] # 2004 - 2006
spawner_10 <- tables_stream[[29]] # 2007 - 2009
spawner_11 <- tables_stream[[32]] # 2010 - 2012
spawner_12 <- tables_stream[[35]] # 2013 - 2015

# tables 13 - 15 are read a bit differently, so they will have a different cleaning approach
spawner_13_1 <- tables_stream[[36]] # the following 3 tables have data for spawner 2016 - 2018
spawner_13_2 <- tables_stream[[37]] #
spawner_13_3 <- tables_stream[[38]] # check this one since it is only one line of data

spawner_14_1 <- tables_stream[[42]] # 2019 -2021 Note that it skips "Natural Spawner and Klamath Basin"
spawner_14_2 <- tables_stream[[43]] # 2019 -2021
spawner_14_3 <- tables_stream[[44]] # 2019 -2021 - total spawner escapent check this one since it is only one line of data

spawner_15_1 <- tables_stream[[48]] # 2022 - 2024 spawner
spawner_15_2 <- tables_stream[[49]] # 2022 - 2024 Note that it skips "Natural Spawner and Klamath Basin"
spawner_15_3 <- tables_stream[[50]] # 2022 - 2024 - total spawner escapent check this one since it is only one line of data


# 1, 3 - 12
spawner_pt_1 <- bind_rows(spawner_1, spawner_3, spawner_4, spawner_5, spawner_6, spawner_7,
                          spawner_8, spawner_9, spawner_10, spawner_11, spawner_12)

# function from fall spawner escapement - testing

# automating spawning 1, 2, 4, 5
clean_spawner_table <- function(tbl, start_year) {
  # Dynamically create the years for 3-year windows
  years <- start_year:(start_year + 2)
  # Standardize and clean names
  tbl_clean <- tbl |>
    filter(if_any(everything(), ~ . != "")) |>
    janitor::clean_names()

  # Find the column that contains the values to split (likely always 6th)
  split_col <- names(tbl_clean)[3]

  # Extract the "totals_Y1 grilse_Y2" column into two
  tbl_clean <- tbl_clean |>
    extract(
      {{split_col}},
      into = c(
        paste0("adults_", years[1]),
        paste0("totals_", years[1]),
        paste0("grilse_", years[2])
      ),
      regex = "^([\\d,]+|--)[ ]*[a-zA-Z/]*\\s+([\\d,]+|--)[ ]*[a-zA-Z/]*\\s+([\\d,]+|--)[ ]*[a-zA-Z/]*$",
      remove = TRUE
    )

  # Rename known columns (same x# structure as original example)
  tbl_renamed <- tbl_clean |>
    rename(
      location = x1,
      !!paste0("grilse_", years[1]) := x2,
      !!paste0("adults_", years[2]) := x4,
      !!paste0("totals_", years[2]) := x5,
      !!paste0("grilse_", years[3]) := x7,
      !!paste0("adults_", years[3]) := x8,
      !!paste0("totals_", years[3]) := x9)

  # Keep only the columns we just renamed
  value_cols <- c("location",
                  paste0("grilse_", years),
                  paste0("adults_", years),
                  paste0("totals_", years))
  spawning_df <- tbl_renamed |>
    select(any_of(value_cols))

  # Fix location continuations and assign subsections
  spawning_df_clean <- spawning_df |>
    mutate(is_continuation = str_starts(location, "\\("),
           location = if_else(is_continuation, paste0(lag(location), " ", location), location)) |>
    filter(!(lead(is_continuation, default = FALSE))) |>
    select(-is_continuation) |>
    mutate(subsection = case_when(
      str_detect(location, "Hatchery Spawners") ~ "Hatchery Spawners",
      str_detect(location, "Natural Spawners") ~ "Natural Spawners",
      TRUE ~ NA_character_
    )) |>
    fill(subsection, .direction = "down") |>
    filter(!location %in% c("Hatchery Spawners", "Natural Spawners"),
           !is.na(location)) |>
    mutate(section = "Spawning Escapement")

  # Pivot longer for tidy structure
  spawning_long <- spawning_df_clean |>
    mutate(across(-location, as.character)) |>
    pivot_longer(
      cols = -c(location, subsection, section),
      names_to = c("category", "year"),
      names_sep = "_",
      values_to = "value") |>
    mutate(
      year = as.integer(year),
      category = str_to_title(category),
      value = readr::parse_number(value))

  return(spawning_long)
}

spawner_indices_1 <- c(3, 6, 9, 11, 14, 17, 20, 23, 26, 29, 32, 35)
# 1, 3 - 12
spawner_years_1 <- c(1980, 1983, 1986, 1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013)

spawner_cleaned <- purrr::map2(
  .x = tables_stream[spawner_indices_1],
  .y = spawner_years_1,
  .f = clean_spawner_table
)

# first part of cleaning spawner tables
combined_spawner_1 <- bind_rows(spawner_cleaned) # 1, 3 - 12

# second part of cleaning
#automating tables 13-15
clean_spawner_group <- function(tbl1, tbl2, tbl3, start_year) {
  years <- start_year:(start_year + 2)

  # clean table ---
  tbl1_clean <- tbl1 |>
    rename(location = ...1,
           !!paste0("grilse_", years[1]) := ...2,
           !!paste0("adults_", years[1]) := all_of(as.character(years[1])),
           !!paste0("totals_", years[1]) := ...4,
           !!paste0("grilse_", years[2]) := ...5,
           !!paste0("adults_", years[2]) := all_of(as.character(years[2])),
           !!paste0("totals_", years[2]) := ...7,
           !!paste0("grilse_", years[3]) := ...8,
           !!paste0("adults_", years[3]) := all_of(as.character(years[3])),
           !!paste0("totals_", years[3]) := ...10) |>
    select(location, tidyselect::matches("^grilse_|^adults_|^totals_"))

  # clean second table ---
  tbl2_clean <- tbl2 |>
    select(1:4, 6:8, 10:12) |>
    mutate(location = as.character(`Klamath River Basin`),
           !!paste0("grilse_", years[1]) := as.character(...2),
           !!paste0("adults_", years[1]) := as.character(...3),
           !!paste0("totals_", years[1]) := as.character(...4),
           !!paste0("grilse_", years[2]) := as.character(...6),
           !!paste0("adults_", years[2]) := as.character(...7),
           !!paste0("totals_", years[2]) := as.character(...8),
           !!paste0("grilse_", years[3]) := as.character(...10),
           !!paste0("adults_", years[3]) := as.character(...11),
           !!paste0("totals_", years[3]) := as.character(...12)) |>
    select(location, tidyselect::matches("^grilse_|^adults_|^totals_"))

  # clean total line table ---
  tbl3 <- as.data.frame(tbl3)
  numbers <- str_extract_all(tbl3, "\\d{1,3}(?:,\\d{3})*")[[1]] |> parse_number()

  tbl3_clean <- tibble(x1 = "Total Spawner Escapement",
                       value = as.character(numbers)) |>
    mutate(field = paste0("x", row_number() + 1)) |>
    pivot_wider(names_from = field, values_from = value)

  names(tbl3_clean) <- c("location", paste0(rep(c("grilse_", "adults_", "totals_"), each = 3),
                                            rep(years, times = 3))[1:10])

  # combine---
  combined <- bind_rows(tbl1_clean, tbl2_clean, tbl3_clean)

  # label subsections and section ---
  combined <- combined |>
    mutate(subsection = case_when(
      str_detect(location, "Hatchery Spawners") ~ "Hatchery Spawners",
      str_detect(location, "Salmon River") ~ "Natural Spawners",
      TRUE ~ NA_character_)) |>
    fill(subsection, .direction = "down") |>
    filter(!location %in% c("Hatchery Spawners"), !is.na(location)) |>
    mutate(section = "Spawning Escapement")
  # Pivot longer for tidy structure
  combined <- combined |>
    mutate(across(-location, as.character)) |>
    pivot_longer(
      cols = -c(location, subsection, section),
      names_to = c("category", "year"),
      names_sep = "_",
      values_to = "value") |>
    mutate(
      year = as.integer(year),
      category = str_to_title(category),
      value = readr::parse_number(value))

  return(combined)
}

# For 2016–2018 (spawner_13)
spawner_13 <- clean_spawner_group(spawner_13_1, spawner_13_2, spawner_13_3, start_year = 2016)

# For 2019–2021 (spawner_14)
spawner_14 <- clean_spawner_group(spawner_14_1, spawner_14_2, spawner_14_3, start_year = 2019)

# For 2022–2024 (spawner_15)
spawner_15 <- clean_spawner_group(spawner_15_1, spawner_15_2, spawner_15_3, start_year = 2022)

spring_spawner_escapement <- bind_rows(spawner_13, spawner_14, spawner_15, combined_spawner_1)
# end spawner escapement cleaning---



# Run-size estimates ----
# reaching tables
run_size_1 <- tables_stream[[2]] # 1980 - 1982
run_size_2 <- tables_stream[[4]] # 1983 - 1985
run_size_3 <- tables_stream[[7]] # 1986 - 1988
run_size_4 <- tables_stream[[10]] # 1989 - 1991
run_size_5 <- tables_stream[[13]] # 1992 - 1994
run_size_6 <- tables_stream[[16]] # 1995 - 1997
run_size_7 <- tables_stream[[19]] # 1998 - 2000
run_size_8 <- tables_stream[[22]] # 2001 - 2003
run_size_9 <- tables_stream[[25]] # 2004 - 2006
run_size_10 <- tables_stream[[28]] # 2007- 2009
run_size_11 <- tables_stream[[31]] # 2010 - 2012
run_size_12 <- tables_stream[[34]] # 2013- 2015
run_size_13 <- tables_stream[[41]] |> slice(-1) # 2016 - 2018 These last 3 tables are slightly different
run_size_14 <- tables_stream[[47]] |> slice(-1) # 2019 - 2021
run_size_15 <- tables_stream[[53]] |> slice(-1) # 2022 - 2024


clean_run_size_table <- function(run_size_tbl, start_year) {
  years <- start_year:(start_year + 2)
  # standardize col names
  names(run_size_tbl) <- janitor::make_clean_names(names(run_size_tbl))
  rename_map <- setNames(
    c("x1", names(run_size_tbl)[2:10]),
    c("location", paste0(rep(c("grilse_", "adults_", "totals_"), times = 3), rep(years, each = 3))[1:9])
  )

  # rename and select cols to keep
  run_size_tbl_clean <- run_size_tbl |>
    rename(!!!rename_map) |>
    select(any_of(names(rename_map))) |>
    mutate(
      subsection = "Total Run Size Estimates",
      section = "Run Size Estimate"
    )

  return(run_size_tbl_clean)
}

# Put all run_size tables into one list - except three of them
run_size_list <- list(
  run_size_1, run_size_2, run_size_3, run_size_4, run_size_5,
  run_size_6, run_size_7, run_size_8, run_size_9, run_size_10,
  run_size_11, run_size_12, run_size_13, run_size_14, run_size_15) #not adding 13-15 because they look different

run_years <- seq(1980, by = 3, length.out = length(run_size_list))
run_size_cleaned <- purrr::map2(run_size_list, run_years, clean_run_size_table)
combined_run_size <- dplyr::bind_rows(run_size_cleaned)

run_size <- combined_run_size |>
  select(where(~ !all(is.na(.)))) |>  # unify column types
  mutate(across(matches("^(grilse|adults|totals)_"), as.character)) |>
  pivot_longer(cols = matches("^(grilse|adults|totals)_"), # pivot to long
               names_to = c("category", "year"),
               names_sep = "_",
               values_to = "value") |>
  mutate(year = as.integer(year),
         category = str_to_title(category),
         value = value |>
           str_remove("\\s*[a-zA-Z/]+$") |>  # remove things like n/, p/, hh/
           readr::parse_number()) |>
  filter(!is.na(value)) |>  # remove rows with no data
  select(location, subsection, section, category, year, value)


# In-river harvest ----
# TODO need to read figure out why river harvest table is not showing except for the years below
harvest_13 <- tables_stream[[39]] # harvest 2016 - 2018
harvest_13_1 <- tables_stream[[40]] # total river harvest 2016 - 2018
harvest_14 <- tables_stream[[45]] # harvest 2019 - 2021
harvest_14_1 <- tables_stream[[46]] # total river harvest 2019 - 2021


harvest_15 <- tables_stream[[51]] # harvest 2022 - 2024
harvest_15_2 <- tables_stream[[52]] # Total river harvest 2022 - 2024
run_size_15 <- tables_stream[[53]] # total run-size 2022 - 2024

### Alternative approach 1 ----

# Extract text from page 1
page_text <- pdf_text(pdf_path)[1]

# View raw text
cat(page_text)

page_text <- str_split(page_text, "\n")[[1]] |>
  trimws() |>
  discard(~ .x == "")

start_idx <- which(str_detect(page_text, regex("River Harvest", ignore_case = TRUE))) + 1
end_idx <- which(str_detect(page_text, regex("^Totals|^Total Run-size Estimates", ignore_case = TRUE))) - 1

# Extract just the lines from the In-River Run section
run_lines <- page_text[start_idx:end_idx]

# Preview
run_lines

run_data <- str_split_fixed(run_lines, "\\s{2,}", n = 7)
run_df <- as.data.frame(run_data, stringsAsFactors = FALSE)

# Preview
head(run_df)

start_year <- 1980
colnames(run_df) <- c("location",
                      paste0("grilse_", start_year),
                      paste0("adults_", start_year),
                      paste0("totals_", start_year),
                      paste0("grilse_", start_year + 1),
                      paste0("adults_", start_year + 1),
                      paste0("totals_", start_year + 1))

# Tidy it
run_clean <- run_df |>
  mutate(across(-location, ~ readr::parse_number(str_remove(.x, "\\s*[a-zA-Z/]+$")))) |>
  mutate(section = "In-River Run") |>
  pivot_longer(
    cols = -c(location, section),
    names_to = c("category", "year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  mutate(
    year = as.integer(year),
    category = str_to_title(category)
  )



## Combining all sections of the megatable  ----

# megatable_data <- bind_rows(in_river_run, in_river_harvest, spawner_escapement)

# save clean data
# usethis::use_data(megatable_data, overwrite = TRUE)
