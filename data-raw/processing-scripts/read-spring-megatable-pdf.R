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

pdf_file <- "data-raw/2023.Spring.Chinook.Megatable.v.28-Mar-2024.pdf"

# 9 column labels in left-to-right order (3 years × 3 measures)
COL_LABELS <- c("yr1_grilse", "yr1_adults", "yr1_totals",
                "yr2_grilse", "yr2_adults", "yr2_totals",
                "yr3_grilse", "yr3_adults", "yr3_totals")

# Row label order matches order in PDF
ALL_LABELS <- c("yurok_tribal_harvest", "klamath_river_angler",
                "hoopa_tribal_harvest", "trinity_river_angler",
                "total_river_harvest")

clean_num <- function(x) {
  x <- gsub("[a-pr-z]/$", "", x)   # footnote suffix like "b/", "h/", "j/"
  x <- gsub(",", "", x)
  x <- trimws(x)
  suppressWarnings(as.integer(x))
}

extract_harvest_page <- function(words) {

  # Locate the RIVER/IN-RIVER HARVEST section -----------------------
  # Identify the uppercase "HARVEST" that has "RIVER" within 3 words before it
  harvest_y <- NA_real_
  runsize_y <- NA_real_

  for (i in seq_len(nrow(words))) {
    if (words$text[i] == "HARVEST" && is.na(harvest_y)) {
      lo    <- max(1L, i - 3L)
      prevs <- words$text[lo:(i - 1L)]
      if ("RIVER" %in% prevs || "IN-RIVER" %in% prevs) harvest_y <- words$y[i]
    }
    if (words$text[i] == "RUN-SIZE" && !is.na(harvest_y) && is.na(runsize_y))
      runsize_y <- words$y[i]
  }
  if (is.na(harvest_y) || is.na(runsize_y)) return(NULL)

  # ---- extract the three years -----------------------------------------
  # Year labels sit ~15-19 px below the HARVEST line
  years <- words |>
    filter(abs(y - harvest_y) < 22,
           grepl("^[0-9]{4}$", text),
           as.integer(text) > 1970) |>
    pull(text) |> as.integer() |> unique() |> sort()
  years <- years[1:3]
  if (length(years) < 3 || anyNA(years)) return(NULL)

  # ---- find the "Grilse Adults Totals ..." column header row -----------
  col_hdr_words <- words |>
    filter(y > harvest_y, y < runsize_y,
           text %in% c("Grilse", "Adults", "Totals")) |>
    arrange(y)
  if (nrow(col_hdr_words) == 0) return(NULL)

  col_row_y   <- col_hdr_words$y[1]
  col_centers <- col_hdr_words |>
    filter(abs(y - col_row_y) < 3) |>
    arrange(x) |>
    pull(x)
  if (length(col_centers) != 9) return(NULL)

  nearest_col <- function(x_pos) COL_LABELS[which.min(abs(col_centers - x_pos))]

  # ---- identify label rows by y-position (left margin, x < 180) --------
  lbl_words <- words |>
    filter(y > col_row_y, y < runsize_y, x < 180) |>
    mutate(yr = round(y, 0)) |>
    arrange(yr)

  row_ys   <- list()
  angler_n <- 0L
  for (yk in sort(unique(lbl_words$yr))) {
    txts <- lbl_words$text[lbl_words$yr == yk]
    if      ("Yurok"  %in% txts) {
      row_ys[["klamath_yurok_tribal_harvest"]] <- yk
    } else if ("Angler" %in% txts) {
      angler_n <- angler_n + 1L
      key <- if (angler_n == 1L) "klamath_river_angler" else "trinity_river_angler"
      row_ys[[key]] <- yk
    } else if ("Hoopa" %in% txts) {
      row_ys[["trinity_hoopa_tribal_harvest"]] <- yk
    } else if ("Total" %in% txts) {
      row_ys[["total_river_harvest"]]  <- yk
    }
  }
  if (length(row_ys) == 0) return(NULL)

  # ---- parse numeric tokens and assign to (label, column) --------------
  ly_vec <- unlist(row_ys)
  ly_nms <- names(ly_vec)

  num_df <- words |>
    filter(y > col_row_y, y < runsize_y, x > 140) |>
    mutate(yr  = round(y, 0),
           val = clean_num(text)) |>
    filter(!is.na(val)) |>
    mutate(
      # nearest label row by y-distance
      label = ly_nms[apply(
        outer(yr, ly_vec, function(a, b) abs(a - b)), 1, which.min)],
      # nearest column center by x-distance
      col   = sapply(x, nearest_col)
    )

  if (nrow(num_df) == 0) return(NULL)

  # ---- pivot to wide (one row per label, one column per yr_measure) ----
  wide <- num_df |>
    select(label, col, val) |>
    group_by(label, col) |>
    summarise(val = last(val), .groups = "drop") |>
    pivot_wider(names_from = col, values_from = val)

  # ensure all 9 columns exist (fill absent ones with NA)
  for (cn in COL_LABELS)
    if (!cn %in% names(wide)) wide[[cn]] <- NA_integer_

  # ---- assemble final (year × label) records ---------------------------
  out_rows <- lapply(ALL_LABELS, function(lbl) {
    row_w <- if (lbl %in% wide$label) wide[wide$label == lbl, ] else NULL
    lapply(seq_along(years), function(i) {
      p <- paste0("yr", i, "_")
      data.frame(
        year   = years[i],
        label  = lbl,
        grilse = if (!is.null(row_w)) row_w[[paste0(p, "grilse")]][[1L]] else NA_integer_,
        adults = if (!is.null(row_w)) row_w[[paste0(p, "adults")]][[1L]] else NA_integer_,
        totals = if (!is.null(row_w)) row_w[[paste0(p, "totals")]][[1L]] else NA_integer_,
        stringsAsFactors = FALSE)
      }) |> bind_rows()
    }) |> bind_rows()
  out_rows
}

# run across all 15 data pages
page_data <- pdf_data(pdf_path) # one data frame per page

harvest_list <- lapply(seq_len(15), function(pg) {
  result <- tryCatch(
    extract_harvest_page(page_data[[pg]]),
    error = function(e) { message("Page ", pg, " error: ", e$message); NULL }
  )
  if (!is.null(result)) {
    message("Page ", pg, ": years ",
            paste(sort(unique(result$year)), collapse = ", "))
  } else {
    message("Page ", pg, ": no harvest section found")
  }
  result
})

harvest_df <- bind_rows(harvest_list) |>
  arrange(year, match(label, ALL_LABELS))

# spring_harvest_clean <- harvest_df |>
#   rename(location = label) |>
#   pivot_longer(
#     cols = c(grilse, adults, totals),
#     names_to = "category",
#     values_to = "value") |>
#   mutate(category = str_to_title(category),
#          section = "River Harvest",
#          subsection = "Harvest") |>
#   select(location, subsection, section, category, year, value) |>
#   filter(!is.na(value))

spring_in_river_harvest <- harvest_df |>
  pivot_longer(
    cols = c(grilse, adults, totals),
    names_to = "category",
    values_to = "value") |>
  mutate(location = label,
         section = "River Harvest",
         subsection = case_when(
           label %in% c("yurok_tribal_harvest", "hoopa_tribal_harvest") ~ "tribal harvest",
           label %in% c("klamath_river_angler", "trinity_river_angler") ~ "river angler",
           label == "total_river_harvest" ~ "total river harvest",
           TRUE ~ label),
         category = str_to_title(category)) |>
  select(location, subsection, section, category, year, value) |> glimpse()

## Combining all sections of the megatable  ----
# pending - in_river_run
spring_megatable <- bind_rows(spring_in_river_harvest, spring_spawner_escapement)

# save clean data
# usethis::use_data(spring_megatable, overwrite = TRUE)
