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

# This script reads and cleans fall run megatable
##  ---- SPAWNER ----
pdf_path <- "data-raw/2022_Klamath_Basin_Megatable_20230216.pdf"
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

# Create empty lists to hold all 15 spawner, harvest, and run tables
spawner_list <- list()
harvest_list <- list()
run_list <- list()

# Loop through 15 logical PDF "pages" (each pair of table entries)
for (i in 1:12) {
  spawner_index <- 2 * i - 1
  harvest_run_index <- 2 * i

  # Assign the corresponding tables
  spawner_list[[i]] <- tables_stream[[spawner_index]]
  harvest_list[[i]] <- tables_stream[[harvest_run_index]]
  run_list[[i]] <- tables_stream[[harvest_run_index]]
}

# Optionally name the list elements for clarity
names(spawner_list) <- paste0("spawner_", 1:12)
names(harvest_list) <- paste0("harvest_", 1:12)
names(run_list)     <- paste0("run_", 1:12)

### SPAWNER first five pages ----
spawner_1 <- spawner_list[[1]]
spawner_2 <- spawner_list[[2]] # 1981 - 1983
spawner_3 <- spawner_list[[3]]  # 1984 - 1986
spawner_3_test <- spawner_3 |> # NOT same structure as 1, fixing so it matches 6
  select(1:7, 9, 11) |>
  rename(...8 = ...9,
         ...9 = ...11)
spawner_4 <- spawner_list[[4]] # 1987 - 1989
spawner_5 <- spawner_list[[5]] # 1990 - 1992

# automating spawning 1, 2, 4, 5
clean_spawner_table <- function(tbl, start_year) {
  # Dynamically create the years for 3-year windows
  years <- start_year:(start_year + 2)
  # Standardize and clean names
  tbl_clean <- tbl |>
    filter(if_any(everything(), ~ . != "")) |>
    janitor::clean_names()

  # Find the column that contains the values to split (likely always 6th)
  split_col <- names(tbl_clean)[6]

  # Extract the "totals_Y1 grilse_Y2" column into two
  tbl_clean <- tbl_clean |>
    extract(
      {{split_col}},
      into = c(paste0("totals_", years[1]), paste0("grilse_", years[2])),
      regex = "^(-- d/|\\d{1,3}(?:,\\d{3})*)\\s+(\\d{1,3}(?:,\\d{3})*)$",
      remove = TRUE)

  # Rename known columns (same x# structure as original example)
  tbl_renamed <- tbl_clean |>
    rename(
      location = x1,
      !!paste0("grilse_", years[1]) := x2,
      !!paste0("adults_", years[1]) := x4,
      !!paste0("adults_", years[2]) := x7,
      !!paste0("totals_", years[2]) := x9,
      !!paste0("grilse_", years[3]) := x11,
      !!paste0("adults_", years[3]) := x13,
      !!paste0("totals_", years[3]) := x15)

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


spawner_indices <- c(1, 2, 4, 5)
spawner_years <- c(1978, 1981, 1987, 1990)

spawner_cleaned <- purrr::map2(
  .x = spawner_list[spawner_indices],
  .y = spawner_years,
  .f = clean_spawner_table
)

combined_spawner <- bind_rows(spawner_cleaned) # 1, 2, 4, 5

### SPAWNER rest of pages #### ----
spawner_3 <- spawner_3 |>
  select(1:7, 9, 11) |>
  rename("...8" = "...9",
         "...9" = "...11")

# 1993 - 1995
spawner_6 <- spawner_list[[6]] |>  # 6 and 7, 8, 9 same structure
  slice(1:19)

# 1996 - 1998 - it reads the entire page at once
spawner_7 <- spawner_list[[7]] |>  # 6 and 7, 8, 9 same structure
  slice(1:19)
spawner_8 <- tables_stream[[14]] |> # reading from tables_stream since reading of tables changed after this
  slice(1:24)
spawner_9 <- tables_stream[[15]] |>
  slice(1:24)
spawner_10 <- tables_stream[[16]] |>
  select(1:5, 7:10) |>
  rename(...6 = ...7,
         ...7 = ...8,
         ...8 = ...9,
         ...9 = ...10)
spawner_11 <- tables_stream[[17]] |>
  slice(1:24)
spawner_12 <- tables_stream[[18]] |>
  slice(1:24)
spawner_13 <- tables_stream[[19]]
spawner_14 <- tables_stream[[21]] # spawner 2017-2019
spawner_15 <- tables_stream[[23]]

# automating spawning
clean_spawner_table_2 <- function(tbl, start_year) {
  # Dynamically create the years for 3-year windows
  years <- start_year:(start_year + 2)

  # Standardize and clean names
  tbl_clean <- tbl |>
    filter(if_any(everything(), ~ . != "")) |>
    janitor::clean_names()

  # Find the column that contains the values to split (likely always 6th)
  split_col <- names(tbl_clean)[4]

  # Extract the "totals_Y1 grilse_Y2" column into two
  tbl_clean <- tbl_clean |>
    extract(
      {{split_col}},
      into = c(paste0("totals_", years[1]), paste0("grilse_", years[2])),
      regex = "^([\\d,]+|--)[ ]*[a-zA-Z/]*[ ]+([\\d,]+|--)[ ]*[a-zA-Z/]*$",
      remove = TRUE)

  # Rename known columns (same x# structure as original example)
  tbl_renamed <- tbl_clean |>
    rename(
      location = x1,
      !!paste0("grilse_", years[1]) := x2,
      !!paste0("adults_", years[1]) := x3,
      !!paste0("adults_", years[2]) := x5,
      !!paste0("totals_", years[2]) := x6,
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
      TRUE ~ NA_character_)) |>
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
      value = value |>
        str_remove("\\s*[a-zA-Z]+/?$") |>  # remove "n/", "p/", "hh/"
        na_if("--") |>                     # "--" as NA
        readr::parse_number())

  return(spawning_long)
}

spawner_list_cleaned <- list(spawner_1, spawner_2, spawner_3, spawner_4, spawner_5,
                             spawner_6, spawner_7, spawner_8, spawner_9, spawner_10,
                             spawner_11, spawner_12, spawner_13, spawner_14, spawner_15)


spawner_indices_2 <- c(3, 6:15)
spawner_years_2 <- c(1984, 1993, 1996, 1999, 2002, 2005, 2008, 2011, 2014, 2017, 2020)

spawner_cleaned_2 <- purrr::map2(
  .x = spawner_list_cleaned[spawner_indices_2],
  .y = spawner_years_2,
  .f = clean_spawner_table_2)

combined_spawner_2 <- bind_rows(spawner_cleaned_2) # 3, 6-15
spawner_escapement <- bind_rows(combined_spawner, combined_spawner_2)


##  ---- IN-RIVER HARVEST ----

harvest_1 <- harvest_list[[1]] |> slice(2:14) |> select(1, 2, 4, 6, 8:12)
harvest_2 <- harvest_list[[2]] |> slice(2:14) |> select(1:4, 6:10)
harvest_3 <- harvest_list[[3]] |> slice(2:14) |> select(1:4, 7, 9, 11:13)
harvest_4 <- harvest_list[[4]] |> slice(2:14) |> select(1:4, 6:10)
harvest_5 <- harvest_list[[5]] |> slice(2:14) |> select(1:4, 6:10)
harvest_6 <- harvest_list[[6]] |> slice(2:14) |> select(1:4, 6:10)
harvest_7 <- tables_stream[[13]] |>  # reads entire page, needs to crop out escapement
  slice(20:33)
harvest_8 <- tables_stream[[14]] |>
  slice(25:38)
harvest_9 <- tables_stream[[15]] |>
  slice(25:38)
harvest_10 <- tables_stream[[16]] |>
  slice(25:38) |>
  select(1:5, 7:10)
harvest_11 <- tables_stream[[17]] |>
  slice(25:38)
harvest_12 <- tables_stream[[18]] |>
  slice(25:38)
harvest_13 <- tables_stream[[20]] |> # only in-river harvest 2014-2016
  slice(9:22) |>
  select(1:4, 6:10)
harvest_14 <- tables_stream[[22]] |>  # only in-river harvest 2017-2019
  slice(9:22) |>
  select(1:4, 6:10)
harvest_15 <- tables_stream[[24]] |> # only in-river harvest
  slice(9:22) |>
  select(1:4, 6:10)

# automating
clean_harvest_table <- function(tbl, start_year, tribal_label = "Indian Net Harvest  e/") {
  years <- start_year:(start_year + 2)

  tbl_clean <- tbl |>
    filter(if_any(everything(), ~ . != "")) |>
    janitor::clean_names()

  colnames(tbl_clean) <- paste0("x", seq_len(ncol(tbl_clean)))
  split_col <- names(tbl_clean)[4]

  tbl_clean <- tbl_clean |>
    extract(
      {{split_col}},
      into = c(paste0("totals_", years[1]), paste0("grilse_", years[2])),
      regex = "^([\\d,]+|--)[ ]*[a-zA-Z/]*[ ]+([\\d,]+|--)[ ]*[a-zA-Z/]*$",
      remove = TRUE)

  tbl_renamed <- tbl_clean |>
    rename(
      location = x1,
      !!paste0("grilse_", years[1]) := x2,
      !!paste0("adults_", years[1]) := x3,
      !!paste0("adults_", years[2]) := x5,
      !!paste0("totals_", years[2]) := x6,
      !!paste0("grilse_", years[3]) := x7,
      !!paste0("adults_", years[3]) := x8,
      !!paste0("totals_", years[3]) := x9)

  value_cols <- c("location",
                  paste0("grilse_", years),
                  paste0("adults_", years),
                  paste0("totals_", years))

  harvest_df_clean <- tbl_renamed |>
    select(any_of(value_cols)) |>
    mutate(is_continuation = str_starts(location, "\\("),
           location = if_else(is_continuation, paste0(lag(location), " ", location), location)) |>
    filter(!(lead(is_continuation, default = FALSE))) |>
    select(-is_continuation) |>
    mutate(subsection = case_when(
      str_detect(location, "Angler Harvest") ~ "Angler Harvest",
      str_detect(location, tribal_label) ~ tribal_label,
      TRUE ~ NA_character_)) |>
    fill(subsection, .direction = "down") |>
    filter(!location %in% c("Angler Harvest", tribal_label),
           !is.na(location)) |>
    mutate(section = "In-River Harvest")

  harvest_long <- harvest_df_clean |>
    mutate(across(-location, as.character)) |>
    pivot_longer(
      cols = -c(location, subsection, section),
      names_to = c("category", "year"),
      names_sep = "_",
      values_to = "value") |>
    mutate(year = as.integer(year),
           category = str_to_title(category),
           value = value |>
             str_remove("\\s*[a-zA-Z]+/?$") |>
             na_if("--") |>
             readr::parse_number()
    )

  return(harvest_long)
}


# All harvest tables
harvest_list_cleaned <- list(harvest_1, harvest_2, harvest_3, harvest_4, harvest_5,
                             harvest_6, harvest_7, harvest_8, harvest_9, harvest_10,
                             harvest_11, harvest_12, harvest_13, harvest_14, harvest_15)

# Start years
harvest_years <- seq(1978, by = 3, length.out = length(harvest_list_cleaned))

# Pages that use "Tribal Harvest  e/" instead of "Indian Net Harvest  e/"
tribal_label_override_indices <- c(1, 12, 13, 14, 15)

# Create a vector of labels for each table
tribal_labels <- ifelse(seq_along(harvest_list_cleaned) %in% tribal_label_override_indices,
                        "Tribal Harvest  e/",
                        "Indian Net Harvest  e/")

harvest_cleaned <- pmap(
  list(harvest_list_cleaned, harvest_years, tribal_labels),
  clean_harvest_table)

in_river_harvest <- bind_rows(harvest_cleaned)

## IN-RIVER RUN ----

run_1 <- run_list[[1]] |> slice(15:21) |> select(1, 2, 4, 6, 8:12) # 1978-80
run_2 <- run_list[[2]] |> slice(15:21) |> select(1:4, 6:10) # 1981-83
run_3 <- run_list[[3]] |> slice(15:21) |> select(1:4, 7, 9, 11:13) #1984-86
run_4 <- run_list[[4]] |> slice(15:21) |> select(1:4, 6:10) #1987-89
run_5 <- run_list[[5]] |> slice(15:21) |> select(1:4, 6:10) #1990-92
run_6 <- run_list[[6]] |> slice(15:21) |> select(1:4, 6:10) #1993-95
run_7 <- tables_stream[[13]] |> slice(34:40) # 1996-1998
run_8 <- run_list[[7]] |> slice(39:45) # 1999-2001
run_9 <- tables_stream[[15]] |> slice(39:46) # 2002-04
run_10 <- run_list[[8]] |> slice(39:46) |> select(1:5, 7:10) #2005-2007
run_11 <- tables_stream[[17]] |> slice(39:45)  # 2008-2010
run_12 <- run_list[[9]] |> slice(39:45) #2011-2013


# different methodology for last 3 pages since extract_tables is no working the same way
area_run <- list(c(500, 0, 792, 612))
end_pages <- 13:15
# Extract and name each run table
run_13_15 <- map(end_pages, ~ extract_tables(file = pdf_path, pages = .x, area = area_run,
                                             method = "stream", guess = FALSE, output = "tibble")[[1]])
run_13 <- run_13_15[[1]] |> slice(5:12) |> select(1:4, 6:10) #2014-2016
run_14 <- run_13_15[[2]] |> slice(5:12) |> select(1:4, 6:10) #2017-2019
run_15 <- run_13_15[[3]] |> slice(5:12) |> select(1:4, 6:10) #2020-2022

# automating
clean_in_river_run_table <- function(tbl_totals, start_year) {
  # Dynamically create the years for 3-year windows
  years <- start_year:(start_year + 2)
  # Standardize and clean names
  tbl_totals_clean <- tbl_totals |>
    filter(if_any(everything(), ~ . != "")) |>
    janitor::clean_names()

  colnames(tbl_totals_clean) <- paste0("x", seq_len(ncol(tbl_totals_clean)))
  split_col <- names(tbl_totals_clean)[4]

  tbl_totals_clean <- tbl_totals_clean |>
    extract(
      {{split_col}},
      into = c(paste0("totals_", years[1]), paste0("grilse_", years[2])),
      regex = "^([\\d,]+|--)[ ]*[a-zA-Z/]*[ ]+([\\d,]+|--)[ ]*[a-zA-Z/]*$",
      remove = TRUE)

  tbl_totals_renamed <- tbl_totals_clean |>
    rename(
      location = x1,
      !!paste0("grilse_", years[1]) := x2,
      !!paste0("adults_", years[1]) := x3,
      !!paste0("adults_", years[2]) := x5,
      !!paste0("totals_", years[2]) := x6,
      !!paste0("grilse_", years[3]) := x7,
      !!paste0("adults_", years[3]) := x8,
      !!paste0("totals_", years[3]) := x9)

  value_cols <- c("location",
                  paste0("grilse_", years),
                  paste0("adults_", years),
                  paste0("totals_", years))

  totals_df_clean <- tbl_totals_renamed |>
    select(any_of(value_cols)) |>
    mutate(is_continuation = str_starts(location, "\\("),
           location = if_else(is_continuation, paste0(lag(location), " ", location), location)) |>
    filter(!(lead(is_continuation, default = FALSE))) |>
    select(-is_continuation) |>
    mutate(subsection = case_when(
      str_detect(location, "Totals") ~ "Totals",
      TRUE ~ NA_character_)) |>
    fill(subsection, .direction = "down") |>
    filter(!location == "Totals",
           !is.na(location)) |>
    mutate(section = "In-River Run")

  totals_long <- totals_df_clean |>
    mutate(across(-location, as.character)) |>
    pivot_longer(cols = -c(location, subsection, section),
                 names_to = c("category", "year"),
                 names_sep = "_",
                 values_to = "value") |>
    mutate(year = as.integer(year),
           category = str_to_title(category),
           value = value |>
             str_remove("\\s*[a-zA-Z]+/?$") |>
             na_if("--") |>
             readr::parse_number())

  return(totals_long)
}

inriver_run_cleaned_list <- list(run_1, run_2, run_3, run_4, run_5,
                                 run_6, run_7, run_8, run_9, run_10,
                                 run_11, run_12, run_13, run_14, run_15)

all_years <- seq(1978, by = 3, length.out = 15)

inriver_run_cleaned <- pmap(
  list(inriver_run_cleaned_list, all_years),
  clean_in_river_run_table)

in_river_run <- bind_rows(inriver_run_cleaned)

## Combining all sections of the megatable  ----

fall_megatable <- bind_rows(in_river_run, in_river_harvest, spawner_escapement) |>
  rename(lifestage = category) |>
  mutate(species = "fall chinook salmon")

