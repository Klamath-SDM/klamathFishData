library(tidyverse)
library(lubridate)
library(tabulapdf)
library(janitor)
library(dplyr)
library(tidyr)

# USFWS Klamath Falls National Fish Hatchery Report - 2024
pdf_path <- "data-raw/20250619_FY2024 KFNFH Annual Report_Final Draft.pdf"
tables_raw <- extract_tables(pdf_path, method = "stream", output = "tibble")

## Release totals
table_11_raw <- tables_raw[[13]] |>
  clean_names() |>
  mutate(stock_date = tolower(stock_date)) |>
  glimpse()

hatchery_release <- table_11_raw |>
  mutate(stock_date = as.character(stock_date),
         stock_date = na_if(stock_date, ""),
         date_anchor = if_else(!is.na(stock_date), stock_date, NA_character_))|>
  fill(date_anchor, .direction = "down") |>
  mutate(stock_date = if_else(is.na(stock_date), date_anchor, stock_date)) |>
  select(-date_anchor) |>
  slice(1:18, 25, 27:28, 30:35, 41, 43:48) |>
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH") |>
  glimpse()

# save data
# usethis::use_data(hatchery_release_KFNFH, overwrite = TRUE)

## Historical Collections and Releases Summary
table_1_raw <- tables_raw[[2]] |>
  clean_names() |>
  glimpse()

table_1 <- table_1_raw |>
  clean_names() |>
  (\(x) {
    new_names <- paste(names(x), x[1, ], sep = "_") |>
      gsub("_NA", "", x = _) |>
      make_clean_names()
    x[-1, ] |>
      setNames(new_names) |>
      mutate(across(-1, ~parse_number(.x)))
  })()

historical_collections_releases <- table_1 |>
  rename(sarp_tl_mm = tl_4_mm,
         fingerling_tl_mm = tl_6_mm,
         fry_tl_mm = tl_8_mm,
         salvage_sl_mm = sl_or_tl_mm) |>
  slice(-10) |> # removing the total
  mutate(fiscal_year = as.numeric(fiscal_year),
         salvage_tl_mm = case_when(salvage_sl_mm == 129 ~ salvage_sl_mm,
                                   salvage_sl_mm == 126 ~ salvage_sl_mm,
                                   TRUE ~ NA),
         salvage_sl_mm = case_when(salvage_tl_mm == 129 ~ NA,
                                   salvage_tl_mm == 126 ~ NA,
                                   TRUE ~ salvage_sl_mm))|>
  select(-c(sarp_tl_mm, fingerling_tl_mm, fry_tl_mm, salvage_sl_mm, salvage_tl_mm)) |>
  mutate(hatchery_name = "KFNFH") |>
  glimpse()

# save data
# usethis::use_data(historical_collections_releases, overwrite = TRUE)
