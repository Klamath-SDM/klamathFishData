library(tidyverse)
library(lubridate)
# Sys.setenv(JAVA_HOME=r"(C:\Program Files\Java\jre1.8.0_471)")
library(tabulapdf) # system requirements: Java
library(janitor)
library(dplyr)
library(tidyr)

## PARSE PDF FILES #############################################################

# USFWS Klamath Falls National Fish Hatchery Report - 2024
pdf_path <- here::here("data-raw/20250619_FY2024 KFNFH Annual Report_Final Draft.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2024_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2024 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2024 |> saveRDS(filename)
} else {
  tables_raw_2024 <- readRDS(filename)
}

# USFWS Klamath Falls National Fish Hatchery Report - 2023
pdf_path <- here::here("data-raw/20240801_FY2023 KFNFH Annual Report_Final.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2023_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2023 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2023 |> saveRDS(filename)
} else {
  tables_raw_2023 <- readRDS(filename)
}

# duplicative of 2024 tables, no need to import:
# - 2023 table 1
# - 2023 table 4
# - 2023 tables 8 and 9 - survival in pens
# - 2023 table 11 - partial year

# versions to import
# - 2023 table 2 = wild ESS LRS handled (equiv. of 2024 table 3)
# - 2023 table 3 = collection and stocking by culture unit (equiv. of 2024 table 5)
# - 2023 table 5-7 = concatenated pond tables (equiv. of 2024 6-8)
# - 2023 table 10 = fish distribution/transfer (equiv of 2024 11)

# USFWS Klamath Falls National Fish Hatchery Report - 2022
pdf_path <- here::here("data-raw/20240830_FY2022 KFNFH Annual Report_Final.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2022_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2022 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2022 |> saveRDS(filename)
} else {
  tables_raw_2022 <- readRDS(filename)
}

# duplicative of 2024 tables, no need to import:
# - 2022 table 1

# versions to import
# - 2022 tables 2-4: concatenated pond tables (equi. of 2024 6-8)
# - 2022 table 5: fish distribution/transfer (equiv of 2024 11)

# TODO: different tables (import?)
# - 2022 table 6: historical PIT tag detections

# USFWS Klamath Falls National Fish Hatchery Report - 2021
pdf_path <- here::here("data-raw/20240912_FY2021_KFNFH_Annual_Report_Final.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2021_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2021 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2021 |> saveRDS(filename)
} else {
  tables_raw_2021 <- readRDS(filename)
}

# versions to import
# - 2021 table 2: fish distribution/transfer (equiv of 2024 11)
# - 2021 tables 3-4: concatenated pond tables (equi. of 2024 6-8)

# TODO: different tables (import?)
# - 2021 tables 5-7: pond study pit tag results
# - 2021 table 8: net pen survival results


## PROCESS TABLES ##############################################################

### KFNFH Hatchery Release Totals ==============================================

# Summary of all fish distributions for repatriation or transfer from the
# Klamath Falls National Fish Hatchery. Reported by fiscal year in individual
# reports. Collect the tables for individual reports and concatenate.

#### 2024 (Table 11) -----------------------------------------------------------

table_11_raw <- tables_raw_2024[[13]] |>
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
  drop_na(stocking_location) |>
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = stock_date) |>
  glimpse()

#### 2023 (Table 10) -----------------------------------------------------------

table_10_2023 <- tables_raw_2023[[11]] |>
  clean_names()

KFNFH_hatchery_release_2023 <- table_10_2023 |>
  rename(stock_date = date) |>
  mutate(stock_date = as.character(stock_date),
         stock_date = na_if(stock_date, ""),
         date_anchor = if_else(!is.na(stock_date), stock_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(stock_date = if_else(is.na(stock_date), date_anchor, stock_date)) |>
  select(-date_anchor) |>
  drop_na(stocking_location) |>
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2023) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = stock_date) |>
  glimpse()

#### 2022 (Table 5) ------------------------------------------------------------

table_5_2022 <- tables_raw_2022[[4]] |>
  clean_names()

KFNFH_hatchery_release_2022 <- table_5_2022 |>
  rename(stock_date = date) |>
  mutate(stock_date = as.character(stock_date),
         stock_date = na_if(stock_date, ""),
         date_anchor = if_else(!is.na(stock_date), stock_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(stock_date = if_else(is.na(stock_date), date_anchor, stock_date)) |>
  select(-date_anchor) |>
  drop_na(stocking_location) |>
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2022) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = stock_date) |>
  mutate(lot = as.character(lot)) |>
  glimpse()

#### 2021 (Table 2) ------------------------------------------------------------

table_2_2021 <- tables_raw_2021[[2]] |>
  clean_names() |>
  drop_na(from_ponds) #|>
#  tidyr::fill(date, .direction = "down")

KFNFH_hatchery_release_2021 <- table_2_2021 |>
  rename(stock_date = date) |>
  mutate(stock_date = as.character(stock_date),
         stock_date = na_if(stock_date, ""),
         date_anchor = if_else(!is.na(stock_date), stock_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(stock_date = if_else(is.na(stock_date), date_anchor, stock_date)) |>
  select(-date_anchor) |>
  drop_na(stocking_location) |>
  mutate(stock_date = as.Date(stock_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2021) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = stock_date) |>
  mutate(lot = as.character(lot)) |>
  glimpse()


#### Concatenate ---------------------------------------------------------------

KFNFH_hatchery_release <- bind_rows(
  KFNFH_hatchery_release_2024,
  KFNFH_hatchery_release_2023,
  KFNFH_hatchery_release_2022,
  KFNFH_hatchery_release_2021
)

KFNFH_hatchery_release |> usethis::use_data(overwrite = T)

### Historical Collections and Releases Summary ================================

# Collections and releases from the Klamath Falls National Fish Hatchery since
# its inception in2016, by federal fiscal years. This table is reproduced in
# each year's report with updated numbers, so only the most recent report needs
# to be imported.

#### 2024 Historical Collection and Release (Table 1) --------------------------

table_1_raw <- tables_raw_2024[[2]] |>
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

#### 2024 Larval Catch (Table 4) -----------------------------------------------

table_4_raw <- tables_raw_2024[[5]] |>
  glimpse()

table_4 <- table_4_raw |>
  clean_names() |>
  slice(-10) |>
  transmute(fiscal_year = as.numeric(year_spring),
         larvae_collected = readr::parse_number(number_collected),
         primary_collection_method) |>
  glimpse()

#### Join Tables ---------------------------------------------------------------

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
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = fiscal_year) |>
  left_join(table_4, by = join_by(fiscal_year, larvae_collected)) |>
  glimpse()

# save data
KFNFH_historical_collection_release |> usethis::use_data(overwrite = T)

### Wild LRS Adults ============================================================

#### 2024 (Table 2) ------------------------------------------------------------

table_2_raw <- tables_raw_2024[[3]]

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
  mutate(pit_tag_last_5 = str_sub(pit_tag_suffix, -5, -1)) |>
  relocate(pit_tag_last_5, .after = pit_tag_suffix) |>
  glimpse()

KFNFH_LRS_ESS_adult_collection_2024 |> usethis::use_data(overwrite = T)

#### 2023 (Table 2) ------------------------------------------------------------

table_2_raw_2023 <- tables_raw_2023[[3]] |>
  clean_names()

KFNFH_LRS_ESS_adult_collection_2023 <-
  table_2_raw_2023 |>
  clean_names() |>
  separate_wider_delim(date_spring_name,
                       delim = " ",
                       names = c("date", "spring_name")) |>
  mutate(date = as.Date(date, format = "%m/%d/%Y"))  |>
  mutate(pit_tag_last_5_sex_fork_length_atfc_genetic_id =
           if_else(is.na(spawned_not_spawned), NA,
                   pit_tag_last_5_sex_fork_length_atfc_genetic_id)) |>
  separate_wider_delim(pit_tag_last_5_sex_fork_length_atfc_genetic_id,
                       delim = " ",
                       names = c("pit_tag_last_5",
                                 "sex",
                                 "fork_length",
                                 "atfc_genetic_id"),
                       too_many = "drop") |>
  rename(spring_location = spring_name,
         fl_mm = fork_length,
         gamete_use = spawned_not_spawned) |>
  mutate(sex = str_to_title(sex),
         gamete_use = str_to_title(gamete_use),
         fry_collected_for_aftc = (fry_collected_for_aftc == "Y"),
         fl_mm = as.numeric(fl_mm),
         fl_in = fl_mm / 25.4) |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2023) |>
  relocate(hatchery_name, fiscal_year, .before = date) |>
  relocate(fl_in, .after = fl_mm) |>
  select(-matches("^x.$")) |>
  glimpse()

#### Combined ------------------------------------------------------------------

KFNFH_LRS_ESS_adult_collection <- bind_rows(
  KFNFH_LRS_ESS_adult_collection_2024,
  KFNFH_LRS_ESS_adult_collection_2023
)

KFNFH_LRS_ESS_adult_collection |> usethis::use_data(overwrite = T)

### Incubation and Hatch Results ===============================================

#### 2024 (Table 3) ------------------------------------------------------------

table_3_raw <- tables_raw_2024[[4]]

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

#### Concatenate ---------------------------------------------------------------

KFNFH_LRS_ESS_incubation_hatch_2024 |> usethis::use_data(overwrite = T)

### Rearing Performance ========================================================

#### 2024 (Table 5) ------------------------------------------------------------

table_5_raw <- tables_raw_2024[[6]] |>
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

### Pond Monitoring ============================================================

#### 2024 (Tables 6-8) ---------------------------------------------------------

table_6_raw <- tables_raw_2024[[8]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

table_7_raw <- tables_raw_2024[[9]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

table_8_raw <- tables_raw_2024[[10]] |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  drop_na(pond)

KFNFH_adfluvial_pond_growout_2024 <-
  bind_rows("FY2024 Table 6" = table_6_raw,
            "FY2024 Table 7" = table_7_raw,
            "FY2024 Table 8" = table_8_raw,
            .id="inventory_source") |>
  transmute(pond, lot, inventory_source,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial"),
         stocked_date = as.Date(stocked_date, format = "%m/%d/%Y"),
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

#### 2023 (Tables 5-7) ---------------------------------------------------------

combine_first_row_into_column_names <- function(.) {
  {
    units <- as.character(unlist(.[1, ])) |> coalesce("")

    rename_with(
      .,
      ~ make.names(paste(.x, units, sep = "_"), unique = TRUE),
      everything()
    )
  }
}

table_5_raw_2023 <- tables_raw_2023[[6]] |>
  combine_first_row_into_column_names() |>
  slice(-1) |>
  clean_names() |>
  drop_na(pond) |>
  mutate(across(c(everything(), -c(stocked, harvested, pond, lot)), as.numeric)) |>
  mutate(across(c(stocked, harvested), mdy)) |>
  transmute(pond, lot, stocked_date = stocked,
            harvest_date = harvested,
            start_number = total_4,
            start_g_fish = average_5_g_fish,
            start_wt_g = weight_6_g,
            start_wt_lb = weight_7_lbs,
            start_tl_mm = tl_8_mm,
            end_number = total_10,
            end_g_fish = average_11_g_fish,
            end_wt_g = weight_12_g,
            end_wt_lb = weight_13_lbs,
            end_tl_mm = tl_14_mm,
            days, months,
            growth_mm_day = growth_17_mm_day,
            weight_gain_g = growth_18_g_fish_mon,
            weigth_gain_lb = weight_gain_lbs,
            harvest_mortality_number = harvest_morts,
            survival_percent = survival)


table_6_raw_2023 <- tables_raw_2023[[7]] |>
  combine_first_row_into_column_names() |>
  slice(-1) |>
  clean_names() |>
  drop_na(pond) |>
  mutate(across(c(everything(), -c(stocked, harvested, pond, lot)), as.numeric)) |>
  mutate(across(c(stocked, harvested), mdy)) |>
  transmute(pond, lot, stocked_date = stocked,
            harvest_date = harvested,
            start_number = total_4,
            start_g_fish = average_5_g_fish,
            start_wt_g = weight_6_g,
            start_wt_lb = weight_7_lbs,
            start_tl_mm = start_tl_8_mm,
            end_number = total_10,
            end_g_fish = average_11_g_fish,
            end_wt_g = weight_12_g,
            end_wt_lb = weight_13_lbs,
            end_tl_mm = start_tl_14_mm,
            days, months,
            growth_mm_day = growth_17_mm_day,
            weight_gain_g = growth_18_g_fish_mon,
            weigth_gain_lb = weight_gain_lbs,
            harvest_mortality_number = harvest_morts,
            survival_percent = survival)

table_7_raw_2023 <- tables_raw_2023[[8]] |>
  combine_first_row_into_column_names() |>
  slice(-1) |>
  clean_names() |>
  drop_na(pond) |>
  mutate(across(c(everything(), -c(stocked, harvested, pond, lot)), as.numeric)) |>
  mutate(across(c(stocked, harvested), mdy)) |>
  transmute(pond, lot, stocked_date = stocked,
            harvest_date = harvested,
            start_number = total_4,
            start_g_fish = average_5_g_fish,
            start_wt_g = weight_6_g,
            start_wt_lb = weight_7_lbs,
            start_tl_mm = tl_8_mm,
            end_number = total_10,
            end_g_fish = average_11_g_fish,
            end_wt_g = weight_12_g,
            end_wt_lb = weight_13_lbs,
            end_tl_mm = tl_14_mm,
            days = total_15_days,
            months = total_16_months,
            growth_mm_day = growth_17_mm_day,
            weight_gain_g = growth_18_g_fish_mon,
            weigth_gain_lb = weight_gain_lbs,
            harvest_mortality_number = harvest_morts,
            survival_percent = survival)

KFNFH_adfluvial_pond_growout_2023 <-
  bind_rows("FY2023 Table 5" = table_5_raw_2023,
            "FY2023 Table 6" = table_6_raw_2023,
            "FY2023 Table 7" = table_7_raw_2023,
            .id="inventory_source") |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2023,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial")) |>
  relocate(hatchery_name, fiscal_year, inventory_source, life_history, .before = pond)

#### 2022 (Tables 2-4) ---------------------------------------------------------

table_2_raw_2022 <- tables_raw_2022[[3]] |>
  clean_names() |>
  drop_na(pond) |>
  separate_wider_delim(growth_mm_day_total_growth_mm,
                       delim = " ",
                       names = c("growth_mm_day", "total_growth_mm")) |>
  transmute(inventory_source = "FY2022 Table 2",
            pond, lot,
            stocked_date = mdy(stocked_to_pond),
            start_number,
            start_tl_mm,
            harvest_date = mdy(harvest_date),
            end_number,
            end_tl_mm,
            days = total_days,
            growth_mm_day = as.numeric(growth_mm_day),
            # total_growth_mm,
            harvest_mortality_number = harvest_morts,
            survival_percent = overall_survival_percent
            )

# tables 3 and 4 were embedded as images. Converted to tabular data:

table_3_4_raw_2022 <-
  bind_rows(
    "FY2022 Table 3" = read_csv(here::here("data-raw", "pdf-tables",
                                           "kfnfh_2022_ocr_table3.csv")) |>
      mutate(Lot = as.character(Lot)),
    "FY2022 Table 4" = read_csv(here::here("data-raw", "pdf-tables",
                                           "kfnfh_2022_ocr_table4.csv")),
    .id = "inventory_source") |>
  clean_names() |>
  drop_na(lot) |>
  transmute(pond,
            lot = as.character(lot),
            stocked_date = mdy(stocked_to_pond),
            start_number = start_ft,
            start_tl_mm,
            harvest_date = mdy(harvest_date),
            end_number = end_ft,
            end_tl_mm,
            days = total_days,
            growth_mm_day = as.numeric(growth_mm_day),
            # total_growth_mm,
            harvest_mortality_number = harvest_morts,
            survival_percent = overall_survival_percent
  )

KFNFH_adfluvial_pond_growout_2022 <-
  bind_rows(table_2_raw_2022, table_3_4_raw_2022) |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2022,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial")) |>
  relocate(hatchery_name, fiscal_year, inventory_source, life_history, .before = pond)

#### 2021 (Tables 3-4) ---------------------------------------------------------

# TODO

table_3_2021 <- tables_raw_2021[[3]] |>
  clean_names() |>
  separate_wider_delim(harvest_date_pond,
                       delim = " ",
                       names = c("harvest_date", "pond"),
                       too_many = "merge") |>
  separate_wider_delim(number_stocked_percent_stocked,
                       delim = " ",
                       names = c("number_stocked", "percent_stocked")) |>
  transmute(pond,
            lot = as.character(lot),
            #stocked_date = NA,
            start_number = paper_number,
            start_tl_mm = tl_mm_7,
            harvest_date = mdy(harvest_date),
            harvest_mortality_number = actual_number - paper_number,
            end_number = actual_number,
            end_tl_mm = NA, # TODO: could calculate as weighted avg
            survival_percent = survival,
            # additional metrics not included in post-2021 version of the table
            # using term disposition for downstream stocking (after end)
            # to avoid ambiguity with stocking of the hatchery ponds (start)
            dispo_to = disposition,
            dispo_tl_mm = as.numeric(tl_mm_9),
            dispo_number = as.numeric(number_stocked),
            dispo_percent = as.numeric(percent_stocked),
            short_number = as.numeric(number_short),
            short_percent = as.numeric(percent_short),
            short_tl_mm = as.numeric(tl_mm_12),
            brood_number = as.numeric(number_brood),
            brood_percent = 100 - (coalesce(dispo_percent, 0) + coalesce(short_percent, 0)),
            ) |>
  drop_na(harvest_date)

table_4_2021 <- tables_raw_2021[[4]]|>
  clean_names() |>
  separate_wider_delim(harvest_date_pond,
                       delim = " ",
                       names = c("harvest_date", "pond"),
                       too_many = "merge") |>
  separate_wider_delim(number_stocked_percent_stocked,
                       delim = " ",
                       names = c("number_stocked", "percent_stocked"),
                       too_few = "align_start") |>
  separate_wider_delim(number_short_percent_short,
                       delim = " ",
                       names = c("number_short", "percent_short"),
                       too_few = "align_start") |>
  transmute(pond,
            lot = as.character(lot),
            #stocked_date = NA,
            start_number = paper_number,
            start_tl_mm = tl_mm_8,
            harvest_date = mdy(harvest_date),
            harvest_mortality_number = actual_number - paper_number,
            end_number = actual_number,
            end_tl_mm = NA, # TODO: could calculate as weighted avg
            survival_percent = survival,
            # additional metrics not included in post-2021 version of the table
            # using term disposition for downstream stocking (after end)
            # to avoid ambiguity with stocking of the hatchery ponds (start)
            dispo_to = disposition_stocked_to,
            dispo_tl_mm = as.numeric(tl_mm_10),
            dispo_number = as.numeric(number_stocked),
            dispo_percent = as.numeric(percent_stocked),
            short_number = as.numeric(number_short),
            short_percent = as.numeric(percent_short),
            short_tl_mm = as.numeric(tl_mm_13),
            brood_number = as.numeric(number_brood),
            brood_percent = 100 - (coalesce(dispo_percent, 0) + coalesce(short_percent, 0)),
            ) |>
  drop_na(harvest_date)

KFNFH_adfluvial_pond_growout_2021 <-
  bind_rows("FY2021 Table 3" = table_3_2021,
            "FY2021 Table 4" = table_4_2021,
            .id="inventory_source") |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2021,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial")) |>
  relocate(hatchery_name, fiscal_year, inventory_source, life_history, .before = pond)

#### Concatenate ---------------------------------------------------------------

KFNFH_adfluvial_pond_growout <- bind_rows(
  KFNFH_adfluvial_pond_growout_2024,
  KFNFH_adfluvial_pond_growout_2023,
  KFNFH_adfluvial_pond_growout_2022,
  KFNFH_adfluvial_pond_growout_2021)

KFNFH_adfluvial_pond_growout |> usethis::use_data(overwrite = T)

### Sucker Pen Survival ========================================================

#### 2024 (Tables 9-10) --------------------------------------------------------

table_9_raw <- tables_raw_2024[[11]] |>
  clean_names() |>
  (\(x) {
    new_names <- paste(names(x), x[1, ], sep = "_") |>
      gsub("_NA", "", x = _) |>
      make_clean_names()
    x[-2, ] |>
      setNames(new_names)
  })()

table_10_raw <- tables_raw_2024[[12]] |>
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

### Sucker PIT tag detection ===================================================

#### 2024 (Table 12) -----------------------------------------------------------

table_12_raw <-
  tables_raw_2024[[14]] |>
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

