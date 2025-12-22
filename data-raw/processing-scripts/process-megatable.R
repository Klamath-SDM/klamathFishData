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

megatable <- bind_rows(in_river_run, in_river_harvest, spawner_escapement) |>
  rename(lifestage = category) |>
  mutate(species = "fall chinook salmon")


# Additional cleaning to data structure -----------------------------------
fall_spawner_escapement <- filter(megatable, section == "Spawning Escapement") |>
  filter(!location %in% c("Subtotals", "Hatchery Spawner Subtotals:",
                          "Total Spawner Escapement","Natural Spawner Subtotals",
                          "Angler Harvest","Angler Harvest Subtotals:","Indian Net Harvest  e/",
                          "Indian Net Harvest Subtotals:", "Totals","In-river Harvest and Escapement",
                          "Angling Mortality (2.04% of harvest)f/","Net Mortality (8.70% of harvest)  f/",
                          "Catch and Release Mortality gg/", "Total In-river Run",
                          "Klamath River (below Hwy 101 bridge)", "Klamath River (Hwy 101 to Weitchpec)",
                          "Klamath River (Weitchpec to IGH)", "Trinity River basin above Weitchpec aa/",
                          "Trinity River (Hoopa Reservation)", "Klamath River (Hwy 101 to Trinity mouth)")) |>
  mutate(origin = case_when(subsection == "Hatchery Spawners" ~ "hatchery",
                            T ~ "wild"),
         location = case_when(location == "Iron Gate Hatchery  (IGH)" ~ "Iron Gate Hatchery",
                              location == "Trinity River Hatchery (TRH)" ~ "Trinity River Hatchery",
                              location %in% c("Trinity River basin (above Willow Creek, excluding TRH)", "Main Stem Trinity Riverdd/ (excluding TRH)") ~ "Trinity River",
                              location == "Salmon River basin" ~ "Salmon River",
                              location == "Scott River basin" ~ "Scott River",
                              location == "Shasta River basin" ~ "Shasta River",
                              location == "Bogus Creek basin" ~ "Bogus Creek",
                              location == "Main Stem Klamath River (excluding IGH)" ~ "Klamath River",
                              location == "Misc. Klamath tributaries (above Hoopa and Yurok Reservations)" ~ "Other Klamath Tributaries",
                              location == "Hoopa and Yurok Reservation tribs." ~ "Hoopa and Yurok Tributaries",
                              location == "Misc. Klamath-Trinity tributaries (above Hoopa and Yurok Reservations)" ~ "Other Klamath Trinity Tributaries",
                              location == "Main Stem Klamath Rivern/ (excluding IGH)" ~ "Klamath River",
                              location %in% c("Misc. Klamath tributarieso/ (above Yurok Reservation)", "Misc. Klamath tributarieso/ii/ (above Yurok Reservation)") ~ "Other Klamath Tributaries",
                              location == "Yurok Reservation tribs. (Klamath River) p/" ~ "Yurok Klamath Tributaries",
                              location == "Klamath Natural Spawner Subtotals:" ~ "Klamath Basin",
                              location == "Misc. Trinity tributaries  o/ (above Hoopa Reservation)" ~ "Other Trinity Tributaries",
                              location == "Hoopa Reservation tribs.  (Trinity River) p/" ~ "Hoopa Trinity Tributaries",
                              location == "Trinity Natural Spawner Subtotals:" ~ "Trinity Basin",
                              T ~ location)) |>
  select(-c(section, subsection)) |>
  pivot_wider(id_cols = c(location, year, species, origin), names_from = lifestage, values_from = value, values_fill = 0) |>
  # fix missing Grilse values
  mutate(Grilse = ifelse(is.na(Grilse), Totals - Adults, Grilse)) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage))


## Spring run spawner escapement ----

pdf_path <- "data-raw/2023.Spring.Chinook.Megatable.v.28-Mar-2024.pdf"
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

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

spring_run_spawner_escapement <- bind_rows(spawner_13, spawner_14, spawner_15, combined_spawner_1)

# Additional Cleaning ----
sr_spawner_escapement <- spring_run_spawner_escapement |>
  rename(lifestage = category) |>
  mutate(species = "spring chinook salmon") |>
  filter(!location %in% c("Trinity River Basin","Klamath River Basin", "Subtotals")) |>
  mutate(origin = case_when(subsection == "Hatchery Spawners" ~ "hatchery",
                            T ~ "wild"),
         location = case_when(location == "Trinity River Hatchery (TRH) b/" ~ "Trinity River Hatchery",
                              location == "South Fork e/" ~ "South Fork Trinity River",
                              location == "Above JCW, excluding TRH b/" ~ "Trinity River",
                              location == "Misc. Tribs. f/" ~ "Other Trinity Tributaries",
                              T ~ location)) |>
  select(-c(section, subsection)) |>
  pivot_wider(id_cols = c(location, year, species, origin), names_from = lifestage, values_from = value, values_fill = 0) |>
  # fix missing Grilse values
  mutate(Grilse = ifelse(is.na(Grilse), Totals - Adults, Grilse)) |>
  pivot_longer(cols = c(Grilse, Adults, Totals), names_to = "lifestage", values_to = "value") |>
  filter(lifestage != "Totals") |>
  mutate(location = tolower(location),
         lifestage = tolower(lifestage))
# TODO add sr script to this repo, rename scripts to something like "read sr_megatable_pdf" and "read fall_megatable_pdf"
# then source them and create process_megatable_spawner, process_megatable_harvest, etc

## Combine spring and fall run - TODO
# spawner_escapement <- bind_rows(fall_spawner_escapement, sr_spawner_escapement)


# save clean data
# usethis::use_data(spawner_escapement, overwrite = TRUE)


