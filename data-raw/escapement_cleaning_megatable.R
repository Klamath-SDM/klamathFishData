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
library(tabulizer)
library(purrr)


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

spawner_1 <- spawner_list[[1]]
# harvest_1 <- harvest_list[[1]]
# run_1 <- run_list[[1]]

### SPAWNER first five pages ----

# 1981 - 1983
spawner_2 <- spawner_list[[2]] # same structure as 1
# harvest_2 <- harvest_list[[2]]
# run_2 <- run_list[[2]]

# 1984 - 1986
spawner_3 <- spawner_list[[3]]  # NOT same structure as 1, fixing so it matches 6
spawner_3_test <- spawner_3 |>
  select(1:7, 9, 11) |>
  rename(...8 = ...9,
         ...9 = ...11)
# harvest_3 <- harvest_list[[3]]
# run_3 <- run_list[[3]]

# 1987 - 1989
spawner_4 <- spawner_list[[4]] # same structure as 1
# harvest_4 <- harvest_list[[4]]
# run_4 <- run_list[[4]]

# 1990 - 1992
spawner_5 <- spawner_list[[5]] # same structure as 1
# harvest_5 <- harvest_list[[5]]
# run_5 <- run_list[[5]]

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
      remove = TRUE
    )

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
      !!paste0("totals_", years[3]) := x15
    )

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
      values_to = "value"
    ) |>
    mutate(
      year = as.integer(year),
      category = str_to_title(category),
      value = readr::parse_number(value)
    )

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
      remove = TRUE
    )

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
      !!paste0("totals_", years[3]) := x9
    )

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
      values_to = "value"
    ) |>
    mutate(
      year = as.integer(year),
      category = str_to_title(category),
      value = value |>
        str_remove("\\s*[a-zA-Z]+/?$") |>  # remove "n/", "p/", "hh/"
        na_if("--") |>                     # "--" as NA
        readr::parse_number()
    )

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
clean_spawner_escapement <- bind_rows(combined_spawner, combined_spawner_2)

