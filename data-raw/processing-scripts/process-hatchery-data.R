library(tidyverse)
library(lubridate)
# Sys.setenv(JAVA_HOME=r"(C:\Program Files\Java\jre1.8.0_471)")
library(tabulapdf) # system requirements: Java
library(janitor)
library(dplyr)
library(tidyr)

# USFWS Klamath Falls National Fish Hatchery Report - 2024
pdf_path <- here::here("data-raw/20250619_FY2024 KFNFH Annual Report_Final Draft.pdf")

filename <- here::here("data-raw", "pdf-tables", "kfnfh_2024_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw |> saveRDS(filename)
} else {
  tables_raw <- readRDS(filename)
}

## Release totals
table_11_raw <- tables_raw[[13]] |>
  clean_names() |>
  mutate(stock_date = tolower(stock_date)) |>
  glimpse()

KFNFH_hatchery_release_2024 <- table_11_raw |>
  mutate(stock_date = as.character(stock_date),
         stock_date = na_if(stock_date, ""),
         date_anchor = if_else(!is.na(stock_date), stock_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(stock_date = if_else(is.na(stock_date), date_anchor, stock_date)) |>
  select(-date_anchor) |>
  # slice(1:18, 25, 27:28, 30:35, 41, 43:48) |> # remove totals
  drop_na(stocking_location) |> # equivalent way to remove totals
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = stock_date) |>
  glimpse()

# save data
KFNFH_hatchery_release_2024 |> usethis::use_data(overwrite = T)

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

# Larval Catch
# TODO: Join to other table
table_4_raw <- tables_raw[[5]] |>
  glimpse()

table_4 <- table_4_raw |>
  clean_names() |>
  slice(-10) |>
  transmute(fiscal_year = as.numeric(year_spring),
         larvae_collected = readr::parse_number(number_collected),
         primary_collection_method) |>
  glimpse()

KFNFH_historical_collection_release <- table_1 |>
  rename(sarp_tl_mm = tl_4_mm,
         fingerling_tl_mm = tl_6_mm,
         fry_tl_mm = tl_8_mm,
         salvage_sl_mm = sl_or_tl_mm) |>
  slice(-n()) |> # removing the total
  mutate(fiscal_year = as.numeric(fiscal_year),
         salvage_tl_mm = case_when(salvage_sl_mm == 129 ~ salvage_sl_mm,
                                   salvage_sl_mm == 126 ~ salvage_sl_mm,
                                   TRUE ~ NA),
         salvage_sl_mm = case_when(salvage_tl_mm == 129 ~ NA,
                                   salvage_tl_mm == 126 ~ NA,
                                   TRUE ~ salvage_sl_mm))|>
  # select(-c(sarp_tl_mm, fingerling_tl_mm, fry_tl_mm, salvage_sl_mm, salvage_tl_mm)) |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = fiscal_year) |>
  left_join(table_4, by = join_by(fiscal_year, larvae_collected)) |>
  glimpse()

# save data
KFNFH_historical_collection_release |> usethis::use_data(overwrite = T)

## Wild LRS Adults

table_2_raw <- tables_raw[[3]]

KFNFH_LRS_ESS_adult_collection_2024 <- table_2_raw |>
  clean_names() |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"))  |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  separate_wider_delim(total_length_spring_location,
                       delim = " ",
                       names = c("total_length", "spring_location"),
                       too_few = "align_end") |>
  rename(tl_mm = total_length,
         fl_mm = fork_length) |>
  relocate(hatchery_name, fiscal_year, .before = date) |>
  glimpse()

KFNFH_LRS_ESS_adult_collection_2024 |> usethis::use_data(overwrite = T)

## Inclubation and Hatch Results

table_3_raw <- tables_raw[[4]]

KFNFH_LRS_ESS_incubation_hatch_2024 <- table_3_raw |>
  clean_names() |>
  mutate(spawning_date = as.Date(spawning_date, format = "%m/%d/%Y"),
         hatch_date = as.Date(hatch_date, format = "%m/%d/%Y")) |>
  select(-x6) |>
  slice(-n()) |> # removing the totals
  rename(male_1 = male_3,
         male_2 = male_4) |>
  rename(egg_volume_mL = egg_volume_m_l,
         eggs_per_mL = number_eggs_m_l) |>
  separate_wider_delim(male_male, delim = " ", names = c("male_3", "male_4")) |>
  glimpse()

KFNFH_LRS_ESS_incubation_hatch_2024 |> usethis::use_data(overwrite = T)


## Rearing Performance

table_5_raw <- tables_raw[[6]] |>
  clean_names() |>
  glimpse()

KFNFH_adfluvial_early_rearing_2024 <- table_5_raw |>
  mutate(date= as.Date(date, format = "%m/%d/%Y")) |>
  slice(-n()) |>  # removing total
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  relocate(hatchery_name, fiscal_year, .before = date) |>
  glimpse()

KFNFH_adfluvial_early_rearing_2024 |> usethis::use_data(overwrite = T)

## Pond monitoring

table_6_raw <- tables_raw[[8]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

table_7_raw <- tables_raw[[9]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

table_8_raw <- tables_raw[[10]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

KFNFH_adfluvial_pond_growout <-
  bind_rows("FY2024 Table 6" = table_6_raw,
            "FY2024 Table 7" = table_7_raw,
            "FY2024 Table 8" = table_8_raw,
            .id="inventory_source") |>
  mutate(stocked_date = as.Date(stocked_date, format = "%m/%d/%Y"),
         harvest_date = as.Date(harvest_date, format = "%m/%d/%Y"),
         start_number = as.numeric(start_number),
         start_g_fish = as.numeric(g_fish),
         start_wt_g = as.numeric(g),
         start_wt_lb = as.numeric(lb),
         start_tl_mm = as.numeric(mm),
         end_number = as.numeric(end_number),
         end_g_fish = as.numeric(g_fish_2),
         end_wt_g = as.numeric(g_2),
         end_gt_lb = as.numeric(lb_2),
         end_tl_mm = as.numeric(mm_2),
         days = as.numeric(days),
         months = as.numeric(months),
         growth_mm_day = as.numeric(mm_day),
         weight_gain_lb = as.numeric(lb_3),
         harvest_mortality_number = as.numeric(number),
         survival_percent = as.numeric(percent)) |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  relocate(hatchery_name, fiscal_year, .before = inventory_source)

KFNFH_adfluvial_pond_growout |> usethis::use_data(overwrite = T)

## Sucker Pen Survival

table_9_raw <- tables_raw[[11]] |>
  clean_names() |>
  (\(x) {
    new_names <- paste(names(x), x[1, ], sep = "_") |>
      gsub("_NA", "", x = _) |>
      make_clean_names()
    x[-2, ] |>
      setNames(new_names)
  })()

table_10_raw <- tables_raw[[12]] |>
  clean_names() |>
  (\(x) {
    new_names <- paste(names(x), x[1, ], sep = "_") |>
      gsub("_NA", "", x = _) |>
      make_clean_names()
    x[-2, ] |>
      setNames(new_names)
  })()

pen_survival <- table_9_raw |>
  slice(2:12) |>
  separate(stock_tl_number, into = c("stock_tl_mm", "number_stocked"), sep = "\\s+", remove = TRUE) |>
  mutate(operation_year = as.numeric(operation),
         captured_year = as.numeric(x2_fish_cy),
         pen_id = x3_pen_id,
         stock_tl_mm = as.numeric(stock_tl_mm),
         number_stocked = as.numeric(number_stocked),
         harvest_tl_mm = as.numeric(harvest_tl),
         number_harvested = as.numeric(number),
         survival_percentage = as.numeric(x8_survival_percent)) |>
  select(operation_year, captured_year, pen_id, stock_tl_mm, number_stocked, harvest_tl_mm,
         number_harvested, survival_percentage) |>
  glimpse()

gerber_pen_survival <- table_10_raw |>
  slice(2:10) |>
  mutate(operation_year = as.numeric(operation),
         captured_year = as.numeric(fish),
         pen_id = x3_pen_id,
         stock_tl_mm = as.numeric(stock_tl),
         number_stocked = as.numeric(x5_number_stocked),
         harvest_tl_mm = as.numeric(harvest_tl),
         number_harvested = as.numeric(x7_number_harvested),
         survival_percentage = as.numeric(x8_survival_percent)) |>
  select(operation_year, captured_year, pen_id, stock_tl_mm, number_stocked, harvest_tl_mm,
         number_harvested, survival_percentage) |>
  glimpse()

KFNFH_pen_survival <-
  bind_rows("Net Pen" = pen_survival,
            "Gerber Net Pen" = gerber_pen_survival,
            .id = "pen_type") |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = pen_type)

KFNFH_pen_survival |> usethis::use_data(overwrite = T)

## Sucker PIT tag detection 2024

table_12_raw <-
  tables_raw[[14]] |>
  clean_names()

sucker_pit_tag_detection_2024 <- table_12_raw |>
  transmute(monitor_location,
            date_deployed = mdy(deployment_date),
            date_retrieved = mdy(retrevial_date),
            tags_total_unique = unique_tags,
            tags_sarp = sarp_tags,
            tags_salvage = salvage,
            tags_unknown = unknown) |>
  slice(-n())

sucker_pit_tag_detection_2024 |> usethis::use_data(overwrite = T)
