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


# Define the PDF file path (download it manually first)
pdf_path <- "data-raw/2022_Klamath_Basin_Megatable_20230216.pdf"

# Try stream method with tibble output
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

## Page 1 ----

### Spawner Escapement ----

spawner_1 <- tables_stream[[1]]

spawner_1_clean <- spawner_1 |>
  filter(if_any(everything(), ~ . != "")) |>
  janitor::clean_names() |>
  extract(spawner_escapement,
          into = c("totals_1978", "grilse_1979"),
          regex = "^(-- d/|\\d{1,3}(?:,\\d{3})*)\\s+(\\d{1,3}(?:,\\d{3})*)$",
          remove = TRUE)

spawner_1_df <- spawner_1_clean |>
  # slice(-(1:2)) |>
  rename(location = x1,
         grilse_1978 = x2,
         adults_1978 = x4,
         adults_1979 = x7,
         totals_1979 = x9,
         grilse_1980 = x11,
         adults_1980 = x13,
         totals_1980 = x15,) |>
  select(location, grilse_1978, adults_1978, totals_1978, grilse_1979, adults_1979,
         totals_1979, grilse_1980, adults_1980, totals_1980)

spawner_1_df_clean <- spawner_1_df |>
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
         # "Subtotals", "Total In-river Harvest"),
         !is.na(location)) |>
  mutate(section = "Spawning Escapement")


# pivot longer
spawner_1_long <- spawner_1_df_clean |>
  mutate(across(matches("_(1978|1979|1980)$"), as.character)) |>
  pivot_longer(cols = matches("_(1978|1979|1980)$"),
               names_to = c("category", "year"),
               names_sep = "_",
               values_to = "value") |>
  mutate(year = as.integer(year),
         category = str_to_title(category),
         # value = parse_number(value)
         value = readr::parse_number(value)) |>
  view()

### In-River Harvest ----
harvest_1 <- tables_stream[[2]]  # or tables_stream[[1]] depending on quality

harvest_1_clean <- harvest_1 |>
  filter(if_any(everything(), ~ . != "")) |>
  janitor::clean_names() |>
  slice(3:14) |>
  # separate(x74_906_6_761, into = c("totals_1978", "grilse_1979"), sep = "\\s+", convert = TRUE) |> view()
  extract(x74_906_6_761,
          into = c("totals_1978", "grilse_1979"),
          regex = "^(-- d/|\\d{1,3}(?:,\\d{3})*)\\s+(\\d{1,3}(?:,\\d{3})*)$",
          remove = TRUE) |>
  mutate(across(where(is.character), ~ na_if(., "--"))) |>
  mutate(across(c(totals_1978, grilse_1979), ~ readr::parse_number(.)))

# Slice and rename columns
harvest_1_clean_df <- harvest_1_clean |>
  slice(2:12) |>
  rename(location = subtotals,
         grilse_1978 = x16_414,
         adults_1978 = x58_492,
         adults_1979 = x30_637,
         totals_1979 = x37_398,
         grilse_1980 = x26_982,
         adults_1980 = x21_483,
         totals_1980 = x48_465,) |>
  select(location, grilse_1978, adults_1978, totals_1978, grilse_1979, adults_1979,
         totals_1979, grilse_1980, adults_1980, totals_1980)
# mutate(across(-location, ~ na_if(., "--")))

# assign subsection from group headers
harvest_1_clean_df_clean <- harvest_1_clean_df |>
  mutate(subsection = case_when(
    str_detect(location, "Angler Harvest") ~ "Angler Harvest",
    str_detect(location, "Tribal Net Harvest") ~ "Tribal Net Harvest",
    TRUE ~ NA_character_)) |>
  fill(subsection, .direction = "down") |>
  filter(!location %in% c("Angler Harvest", "Tribal Net Harvest  e/"),
         # "Subtotals", "Total In-river Harvest"),
         !is.na(location)) |>
  mutate(section = "In-River Harvest")

# pivot longer
harvest_1_df_long <- harvest_1_clean_df_clean |>
  mutate(across(matches("_(1978|1979|1980)$"), as.character)) |>
  pivot_longer(cols = matches("_(1978|1979|1980)$"),
               names_to = c("category", "year"),
               names_sep = "_",
               values_to = "value") |>
  mutate(year = as.integer(year),
         category = str_to_title(category),
         # value = parse_number(value)
         value = readr::parse_number(value)) |>
  view()

### In-River Run ----

run_1 <- tables_stream[[2]]  # or tables_stream[[1]] depending on quality

run_1_clean <- run_1 |>
  filter(if_any(everything(), ~ . != "")) |>
  janitor::clean_names() |>
  slice(15:21) |>
  # separate(x74_906_6_761, into = c("totals_1978", "grilse_1979"), sep = "\\s+", convert = TRUE) |> view()
  extract(x74_906_6_761,
          into = c("totals_1978", "grilse_1979"),
          regex = "^(-- d/|\\d{1,3}(?:,\\d{3})*)\\s+(\\d{1,3}(?:,\\d{3})*)$",
          remove = TRUE) |>
  mutate(across(where(is.character), ~ na_if(., "--"))) |>
  mutate(across(c(totals_1978, grilse_1979), ~ readr::parse_number(.)))

# Slice and rename columns
run_1_clean_df <- run_1_clean |>
  slice(-(1:2)) |>
  rename(location = subtotals,
         grilse_1978 = x16_414,
         adults_1978 = x58_492,
         adults_1979 = x30_637,
         totals_1979 = x37_398,
         grilse_1980 = x26_982,
         adults_1980 = x21_483,
         totals_1980 = x48_465,) |>
  select(location, grilse_1978, adults_1978, totals_1978, grilse_1979, adults_1979,
         totals_1979, grilse_1980, adults_1980, totals_1980)
# mutate(across(-location, ~ na_if(., "--")))

# assign subsection from group headers
run_1_clean_df_clean <- run_1_clean_df |>
  mutate(subsection = case_when(
    str_detect(location, "Totals") ~ "Totals",
    TRUE ~ NA_character_)) |>
  fill(subsection, .direction = "down") |>
  filter(!location == "Totals",
         # "Subtotals", "Total In-river Harvest"),
         !is.na(location)) |>
  mutate(section = "In-River Run")

# pivot longer
run_1_df_long <- run_1_clean_df_clean |>
  mutate(across(matches("_(1978|1979|1980)$"), as.character)) |>
  pivot_longer(cols = matches("_(1978|1979|1980)$"),
               names_to = c("category", "year"),
               names_sep = "_",
               values_to = "value") |>
  mutate(year = as.integer(year),
         category = str_to_title(category),
         # value = parse_number(value)
         value = readr::parse_number(value)) |>
  view()


#combine all 3 sections of first page

megatable_first_page <- bind_rows(spawner_1_long, harvest_1_df_long, run_1_df_long)


##  ---- SPAWNER ----

library(tidyverse)
library(tabulizer)
library(janitor)

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
         ...9 = ...11) |>
  view()
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



### SPAWNER rest of pages ----


# spawner_3 dont forget this needs to be included

# 1993 - 1995
spawner_6 <- spawner_list[[6]] # 6 and 7, 8, 9 same structure
# harvest_6 <- harvest_list[[6]]
# run_6 <- run_list[[6]]

# 1996 - 1998 - it reads the entire page at once
spawner_7 <- spawner_list[[7]] # 6 and 7, 8, 9 same structure
spawner_8 <- tables_stream[[14]] # doing this since reading of tables changed after this

spawner_9 <- tables_stream[[16]]
spawner_10 <- tables_stream[[17]]
spawner_11 <- tables_stream[[18]]
spawner_12 <- tables_stream[[19]]
spawner_13 <- tables_stream[[20]] # reading changes again soon #TODO check approach
spawner_14 <- tables_stream[[21]]
spawner_15 <- tables_stream[[22]]







