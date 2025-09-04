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
pdf_path <- "data-raw/2023.Spring.Chinook.Megatable.v.28-Mar-2024.pdf"
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

# Same approach than fall run megatable ----

#for some reason it is not reading the table the same way - River Harvest is too empty, that might be why

run_size_1 <- tables_stream[[2]] # 1980 - 1982
spawner_1 <- tables_stream[[3]] # 1980 - 1982

run_size_2 <- tables_stream[[4]] # 1983 - 1985
# page_2 <- tables_stream[[5]]
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

spawner_13_1 <- tables_stream[[36]] # the following 3 tables have data for spawner 2016 - 2018
spawner_13_2 <- tables_stream[[37]] #
spawner_13_3 <- tables_stream[[38]] # check this one since it is only one line of data

harvest_13 <- tables_stream[[39]] # harvest 2016 - 2018
harvest_13_1 <- tables_stream[[40]] # total river harvest 2016 - 2018
run_size_13 <- tables_stream[[41]] # total run-size 2016 - 2018

# these 3 are together
spawner_14_1 <- tables_stream[[42]] # 2019 -2021 Note that it skips "Natural Spawner and Klamath Basin"
spawner_14_2 <- tables_stream[[43]] # 2019 -2021
spawner_14_3 <- tables_stream[[44]] # 2019 -2021 - total spawner escapent check this one since it is only one line of data


harvest_14 <- tables_stream[[45]] # harvest 2019 - 2021
harvest_14_1 <- tables_stream[[46]] # total river harvest 2019 - 2021
run_size_14 <- tables_stream[[47]] # total run-size 2019 - 2021

spawner_15 <- tables_stream[[48]] # 2022 - 2024 spawner
spawner_15_1 <- tables_stream[[49]] # 2022 - 2024 Note that it skips "Natural Spawner and Klamath Basin"
spawner_15_2 <- tables_stream[[50]] # 2022 - 2024 - total spawner escapent check this one since it is only one line of data


harvest_15 <- tables_stream[[51]] # harvest 2022 - 2024
harvest_15_2 <- tables_stream[[52]] # Total river harvest 2022 - 2024
run_size_15 <- tables_stream[[53]] # total run-size 2022 - 2024



# function from fall spawner escapement - testing

# automating spawning 1, 2, 4, 5
# clean_spawner_table <- function(tbl, start_year) {
  # Dynamically create the years for 3-year windows
  years <- start_year:(start_year + 2)
  # Standardize and clean names
  tbl_clean <- spawner_3 |>
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

#   return(spawning_long)
# }






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

