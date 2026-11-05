# process-hatchery-data.R ------------------------------------------------------
#
# Builds the KFNFH hatchery data objects of the package from the annual report PDFs (FY2021-2025) and the
# hatchery Excel workbooks. Sections:
#
#   1. PARSE PDF FILES and PROCESS TABLES: the report tables of each year (FY2021-2024) and the FY2025 tables.
#      The code that built the first versions of the package objects is commented out where the final build
#      below replaces it ("SUPERSEDED"), so it can be reviewed before it is removed.
#   2. INPUTS FROM THE EXCEL WORKBOOKS AND THE REPORTS: mortality spreadsheets, production harvest, spawning
#      crosses, broodstock, the age/facility lookup, collections and releases, capacity, and the mortality events.
#      Values are read from the reports and the Excel files. The only hand-compiled file used is
#      data-raw/hatchery-resources/mortality_events.csv, which holds the descriptions of the mortality events
#      (a reading of the report narratives; the file records no author); every number in the events comes from the reports or workbooks.
#   3. FINAL OBJECTS: merges the FY2021-2024 report tables with FY2025 and the section 2 inputs, and saves the
#      package data objects.
#
# The objects are explored and documented in data-raw/hatchery_data_integration.Rmd, which does not build them.
#
# Run from the project root.

library(tidyverse)
library(lubridate)
# Sys.setenv(JAVA_HOME=r"(C:\Program Files\Java\jre1.8.0_471)")
library(tabulapdf) # system requirements: Java
library(janitor)
library(dplyr)
library(tidyr)
library(readxl)
library(here)

## PARSE PDF FILES #############################################################

# (the PDFs are in data-raw/hatchery-resources/; the extracted tables are cached in data-raw/pdf-tables/)

# USFWS Klamath Falls National Fish Hatchery Report - 2024
pdf_path <- here::here("data-raw/hatchery-resources/20250619_FY2024 KFNFH Annual Report_Final Draft.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2024_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2024 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2024 |> saveRDS(filename)
} else {
  tables_raw_2024 <- readRDS(filename)
}

# USFWS Klamath Falls National Fish Hatchery Report - 2023
pdf_path <- here::here("data-raw/hatchery-resources/20240801_FY2023 KFNFH Annual Report_Final.pdf")
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
pdf_path <- here::here("data-raw/hatchery-resources/20240830_FY2022 KFNFH Annual Report_Final.pdf")
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
pdf_path <- here::here("data-raw/hatchery-resources/20240912_FY2021_KFNFH_Annual_Report_Final.pdf")
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


# USFWS Klamath Falls National Fish Hatchery Report - 2025
pdf_path <- here::here("data-raw/hatchery-resources/FY25 KFNFH Annual Report.pdf")
filename <- here::here("data-raw", "pdf-tables", "kfnfh_2025_tables_raw.Rds")
if(!file.exists(filename)) {
  tables_raw_2025 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2025 |> saveRDS(filename)
} else {
  tables_raw_2025 <- readRDS(filename)
}

# FY2025 tables used below (index in the extracted list): 2 = Table 1 (historical collections and releases),
# 3 = Table 2 (wild adults), 4 = Table 3 (family groups), 5 = Table 4 (broodstock spawned), 8 = Table 7 (larval catch),
# 9 = Table 8 (early rearing), 11-13 = Tables 10-12 (ponds), 14-15 = Tables 13-14 (net pens), 16 = Table 15 (distributions)

## PROCESS TABLES ##############################################################

### KFNFH Hatchery Release Totals ==============================================

# Summary of all fish distributions for repatriation or transfer from the
# Klamath Falls National Fish Hatchery. Reported by fiscal year in individual
# reports. Collect the tables for individual reports and concatenate.

#### 2024 (Table 11) -----------------------------------------------------------

table_11_raw <- tables_raw_2024[[13]] |>
  clean_names() |>
  mutate(dispo_date = tolower(stock_date), .keep = "unused") |>
  glimpse()

KFNFH_hatchery_release_2024 <- table_11_raw |>
  rename(dispo_location = stocking_location,
         number_per_lb = number_lb) |>
  mutate(dispo_date = as.character(dispo_date),
         dispo_date = na_if(dispo_date, ""),
         date_anchor = if_else(!is.na(dispo_date), dispo_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(dispo_date = if_else(is.na(dispo_date), date_anchor, dispo_date)) |>
  select(-date_anchor) |>
  drop_na(dispo_location) |>
  mutate(dispo_date = as.Date(dispo_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2024) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = dispo_date) |>
  glimpse()

#### 2023 (Table 10) -----------------------------------------------------------

table_10_2023 <- tables_raw_2023[[11]] |>
  clean_names() |> glimpse()

KFNFH_hatchery_release_2023 <- table_10_2023 |>
  rename(dispo_date = date,
         dispo_location = stocking_location,
         number_per_lb = number_lb) |>
  mutate(dispo_date = as.character(dispo_date),
         dispo_date = na_if(dispo_date, ""),
         date_anchor = if_else(!is.na(dispo_date), dispo_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(dispo_date = if_else(is.na(dispo_date), date_anchor, dispo_date)) |>
  select(-date_anchor) |>
  drop_na(dispo_location) |>
  mutate(dispo_date = as.Date(dispo_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2023) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = dispo_date) |>
  glimpse()

#### 2022 (Table 5) ------------------------------------------------------------

table_5_2022 <- tables_raw_2022[[4]] |>
  clean_names()

KFNFH_hatchery_release_2022 <- table_5_2022 |>
  rename(dispo_date = date,
         dispo_location = stocking_location,
         number_per_lb = number_lb) |>
  mutate(dispo_date = as.character(dispo_date),
         dispo_date = na_if(dispo_date, ""),
         date_anchor = if_else(!is.na(dispo_date), dispo_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(dispo_date = if_else(is.na(dispo_date), date_anchor, dispo_date)) |>
  select(-date_anchor) |>
  drop_na(dispo_location) |>
  mutate(dispo_date = as.Date(dispo_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2022) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = dispo_date) |>
  mutate(lot = as.character(lot)) |>
  glimpse()

#### 2021 (Table 2) ------------------------------------------------------------

table_2_2021 <- tables_raw_2021[[2]] |>
  clean_names() |>
  drop_na(from_ponds) #|>
#  tidyr::fill(date, .direction = "down")

KFNFH_hatchery_release_2021 <- table_2_2021 |>
  rename(dispo_date = date,
         dispo_location = stocking_location,
         number_per_lb = number_lb) |>
  mutate(dispo_date = as.character(dispo_date),
         dispo_date = na_if(dispo_date, ""),
         date_anchor = if_else(!is.na(dispo_date), dispo_date, NA_character_),
         number_fish = number)|>
  fill(date_anchor, .direction = "down") |>
  mutate(dispo_date = if_else(is.na(dispo_date), date_anchor, dispo_date)) |>
  select(-date_anchor) |>
  drop_na(dispo_location) |>
  mutate(dispo_date = as.Date(dispo_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2021) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = dispo_date) |>
  mutate(lot = as.character(lot)) |>
  glimpse()

# New (intermediate object used by the final build below):
hatchery_release_2021_2024 <- bind_rows(
  KFNFH_hatchery_release_2024,
  KFNFH_hatchery_release_2023,
  KFNFH_hatchery_release_2022,
  KFNFH_hatchery_release_2021
)

# ### Historical Collections and Releases Summary ================================

# # Collections and releases from the Klamath Falls National Fish Hatchery since
# # its inception in2016, by federal fiscal years. This table is reproduced in
# # each year's report with updated numbers, so only the most recent report needs
# # to be imported.

# #### 2024 Historical Collection and Release (Table 1) --------------------------

# table_1_raw <- tables_raw_2024[[2]] |>
#   clean_names() |>
#   glimpse()

# table_1 <- table_1_raw |>
#   clean_names() |>
#   (\(x) {
#     new_names <- paste(names(x), x[1, ], sep = "_") |>
#       gsub("_NA", "", x = _) |>
#       make_clean_names()
#     x[-1, ] |>
#       setNames(new_names) |>
#       mutate(across(-1, ~parse_number(.x)))
#   })()

# #### 2024 Larval Catch (Table 4) -----------------------------------------------

# table_4_raw <- tables_raw_2024[[5]] |>
#   glimpse()

# table_4 <- table_4_raw |>
#   clean_names() |>
#   slice(-10) |>
#   transmute(fiscal_year = as.numeric(year_spring),
#          larvae_collected = readr::parse_number(number_collected),
#          primary_collection_method) |>
#   glimpse()

# #### Join Tables ---------------------------------------------------------------

# KFNFH_historical_collection_release <- table_1 |>
#   rename(sarp_tl_mm = tl_4_mm,
#          fingerling_tl_mm = tl_6_mm,
#          fry_tl_mm = tl_8_mm,
#          salvage_sl_mm = sl_or_tl_mm) |>
#   slice(-n()) |> # removing the total
#   mutate(fiscal_year = as.numeric(fiscal_year),
#          # TODO: check on these naming conventions
#          salvage_tl_mm = case_when(salvage_sl_mm == 129 ~ salvage_sl_mm,
#                                    salvage_sl_mm == 126 ~ salvage_sl_mm,
#                                    TRUE ~ NA),
#          salvage_sl_mm = case_when(salvage_tl_mm == 129 ~ NA,
#                                    salvage_tl_mm == 126 ~ NA,
#                                    TRUE ~ salvage_sl_mm))|>
#   mutate(hatchery_name = "KFNFH") |>
#   relocate(hatchery_name, .before = fiscal_year) |>
#   left_join(table_4, by = join_by(fiscal_year, larvae_collected)) |>
#   glimpse()

# # save data
# KFNFH_historical_collection_release |> usethis::use_data(overwrite = T)

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

# [SUPERSEDED - kept for review, remove once checked] adult collection: FY2025 is added in FINAL OBJECTS below
# #### Combined ------------------------------------------------------------------

# KFNFH_LRS_ESS_adult_collection <- bind_rows(
#   KFNFH_LRS_ESS_adult_collection_2024,
#   KFNFH_LRS_ESS_adult_collection_2023
# )

# KFNFH_LRS_ESS_adult_collection |> usethis::use_data(overwrite = T)

# New (intermediate object used by the final build below):
adult_collection_2023_2024 <- bind_rows(
  KFNFH_LRS_ESS_adult_collection_2024,
  KFNFH_LRS_ESS_adult_collection_2023
)

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

# [SUPERSEDED - kept for review, remove once checked] incubation: the FY2024 + FY2025 object (KFNFH_LRS_ESS_incubation_hatch) is built in FINAL OBJECTS below
# KFNFH_LRS_ESS_incubation_hatch_2024 |> usethis::use_data(overwrite = T)

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

# [SUPERSEDED - kept for review, remove once checked] early rearing: the FY2023-2025 object (KFNFH_adfluvial_early_rearing) is built in FINAL OBJECTS below
# KFNFH_adfluvial_early_rearing_2024 |> usethis::use_data(overwrite = T)

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

KFNFH_pond_growout_2024 <-
  bind_rows("FY2024 Table 6" = table_6_raw,
            "FY2024 Table 7" = table_7_raw,
            "FY2024 Table 8" = table_8_raw,
            .id="inventory_source") |>
  transmute(pond, lot, inventory_source,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial"),
         pond_stock_date = as.Date(stocked_date, format = "%m/%d/%Y"),
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
  transmute(pond, lot, pond_stock_date = stocked,
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
  transmute(pond, lot, pond_stock_date = stocked,
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
  transmute(pond, lot, pond_stock_date = stocked,
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

KFNFH_pond_growout_2023 <-
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
            pond_stock_date = mdy(stocked_to_pond),
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
            pond_stock_date = mdy(stocked_to_pond),
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

KFNFH_pond_growout_2022 <-
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
            #pond_stock_date = NA,
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
            #pond_stock_date = NA,
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

KFNFH_pond_growout_2021 <-
  bind_rows("FY2021 Table 3" = table_3_2021,
            "FY2021 Table 4" = table_4_2021,
            .id="inventory_source") |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2021,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial")) |>
  relocate(hatchery_name, fiscal_year, inventory_source, life_history, .before = pond)

# [SUPERSEDED - kept for review, remove once checked] pond grow-out: FY2025 cycles, the workbook check and the age lookup are added in FINAL OBJECTS below
# #### Concatenate ---------------------------------------------------------------

# KFNFH_pond_growout <- bind_rows(
#   KFNFH_pond_growout_2024,
#   KFNFH_pond_growout_2023,
#   KFNFH_pond_growout_2022,
#   KFNFH_pond_growout_2021)

# KFNFH_pond_growout |> usethis::use_data(overwrite = T)

# New (intermediate object used by the final build below):
pond_growout_reports_2021_2024 <- bind_rows(
  KFNFH_pond_growout_2024,
  KFNFH_pond_growout_2023,
  KFNFH_pond_growout_2022,
  KFNFH_pond_growout_2021) |> glimpse()

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

# [SUPERSEDED - kept for review, remove once checked] net pen survival: the version with FY2025 status rows is built in FINAL OBJECTS below
# KFNFH_pen_survival <-
#   bind_rows("Net Pen" = pen_survival,
#             "Gerber Net Pen" = gerber_pen_survival,
#             .id = "pen_type") |>
#   mutate(hatchery_name = "KFNFH") |>
#   relocate(hatchery_name, .before = pen_type)

# KFNFH_pen_survival |> usethis::use_data(overwrite = T)

# New (intermediate object used by the final build below):
pen_survival_reports_2024 <-
  bind_rows("Net Pen" = pen_survival,
            "Gerber Net Pen" = gerber_pen_survival,
            .id = "pen_type") |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = pen_type)

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


## 2. INPUTS FROM THE EXCEL WORKBOOKS AND THE REPORTS ###########################

## SETTINGS ####################################################################

resources_dir <- here("data-raw", "hatchery-resources")

xl <- function(...) file.path(resources_dir, ...)
pdf_dir <- resources_dir

reports <- c(
  `2021` = "20240912_FY2021_KFNFH_Annual_Report_Final.pdf",
  `2022` = "20240830_FY2022 KFNFH Annual Report_Final.pdf",
  `2023` = "20240801_FY2023 KFNFH Annual Report_Final.pdf",
  `2024` = "20250619_FY2024 KFNFH Annual Report_Final Draft.pdf",
  `2025` = "FY25 KFNFH Annual Report.pdf"
)

## HELPERS #####################################################################

excel_date <- function(x) as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
num <- function(x) suppressWarnings(as.numeric(x))
read_text_sheet <- function(path, sheet) {
  read_excel(path, sheet = sheet, col_types = "text", .name_repair = "minimal")
}
# Excel rounds .5 up (R's round() goes to even)
round_half_up <- function(x, digits = 0) floor(x * 10^digits + 0.5) / 10^digits
# m/d/Y without leading zeros (strftime's "%-m" is not portable)
us_date <- function(d) if_else(is.na(d), NA_character_, paste0(month(d), "/", day(d), "/", year(d)))
fiscal_year_of <- function(d) year(d) + if_else(month(d) >= 10, 1, 0)

# report text with ligatures and line breaks removed
report_text <- function(year) {
  pdftools_available <- requireNamespace("pdftools", quietly = TRUE)
  txt <- if (pdftools_available) {
    paste(pdftools::pdf_text(file.path(pdf_dir, reports[[as.character(year)]])), collapse = " ")
  } else {
    system2("pdftotext", c(shQuote(file.path(pdf_dir, reports[[as.character(year)]])), "-"), stdout = TRUE) |>
      paste(collapse = " ")
  }
  txt |>
    str_replace_all(c("\uFB01" = "fi", "\uFB00" = "ff", "\uFB03" = "ffi", "\uFB02" = "fl")) |>
    str_squish()
}

# cached PDF tables (process-hatchery-data.R caches 2021-2024; 2025 is cached here)
load_pdf_tables <- function(year) {
  cache <- here("data-raw", "pdf-tables", paste0("kfnfh_", year, "_tables_raw.Rds"))
  if (!file.exists(cache)) {
    tabulapdf::extract_tables(file.path(pdf_dir, reports[[as.character(year)]]),
                              method = "stream", output = "tibble") |>
      saveRDS(cache)
  }
  readRDS(cache)
}

## 2a. FROM THE EXCEL WORKBOOKS #################################################

### 2a.1 Mortality spreadsheets ========

read_wq_mortality <- function(path, sheet, fiscal_year) {
  read_excel(path, sheet = sheet, col_types = "text") |>
    rename_with(str_to_lower) |>
    transmute(
      fiscal_year = fiscal_year,
      pond = str_to_upper(pond),
      date = excel_date(date),
      mortality = num(morts)
    ) |>
    filter(!is.na(mortality)) |>
    mutate(month = as.character(month(date, label = TRUE, abbr = TRUE)),
           type = "pond") |>
    select(fiscal_year, month, pond, mortality, type)
}

# FY2024: daily per-pond water quality logs, 1 Jan - 22 Sep 2024 only
mortality_fy2024 <- c("JanToMar_WQ", "ApriltoJune_WQ", "JulytoSeptember_WQ") |>
  map(~ read_wq_mortality(xl("FY2024 Supporting Excel Tables", "Copy of 2024_KFNFH_WQ.xlsx"), .x, 2024)) |>
  list_rbind()

# FY2025: daily per-pond logs, full fiscal year
mortality_fy2025 <- c("OctobertoDecember_WQ", "JantoMar_WQ", "AprtoJun_WQ", "JulytoSept_WQ") |>
  map(~ read_wq_mortality(xl("FY2025 Supporting Excel Tables", "KFNFH_WQ_FY2025_2 (version 1).xlsx"), .x, 2025)) |>
  list_rbind()

# FY2023: pre-tabulated pond x month matrix and a per-event harvest mortality sheet
fy2023_wq_path <- xl("FY2023 Supporting Excel Tables", "Fiscal Year 23 WQ_Mortality reporting.xlsx")

pond_mortality_fy2023_wide <- read_text_sheet(fy2023_wq_path, "Pond Mortality FY 23")
names(pond_mortality_fy2023_wide)[1] <- "month"
names(pond_mortality_fy2023_wide)[ncol(pond_mortality_fy2023_wide)] <- "monthly_total"

mortality_fy2023 <- pond_mortality_fy2023_wide |>
  filter(!is.na(month), month != "Total") |>
  select(-monthly_total) |>
  pivot_longer(-month, names_to = "pond", values_to = "mortality") |>
  transmute(fiscal_year = 2023,
            month = str_sub(month, 1, 3),
            pond,
            mortality = num(na_if(mortality, "-")),
            type = "pond") |>
  filter(!is.na(mortality))

harvest_fy2023_wide <- read_text_sheet(fy2023_wq_path, "Harvest Morts FY 23")
harvest_header_row <- which(harvest_fy2023_wide[[1]] == "Pond")

harvest_fy2023 <- harvest_fy2023_wide[(harvest_header_row + 1):nrow(harvest_fy2023_wide), 1:3] |>
  set_names(c("pond", "harvest_date", "mortality")) |>
  filter(!is.na(pond)) |>
  transmute(fiscal_year = 2023,
            harvest_date = excel_date(harvest_date),
            month = as.character(month(harvest_date, label = TRUE, abbr = TRUE)),
            pond = str_to_upper(pond),
            mortality = num(mortality),
            type = "harvest") |>
  select(fiscal_year, month, pond, mortality, type)

# the lookup is joined below, after the lookup section, so it can be switched

### 2a.2 Production harvest ================
# One sheet per stocking season. Column layout drifts between seasons, so columns
# are matched by name pattern. When a season appears in more than one workbook
# (e.g. Fall 2022 in the FY2023 and FY2024 workbooks) the most recent workbook is used.

extract_col <- function(raw, pattern) {
  matched <- names(raw)[str_detect(str_squish(names(raw)), regex(pattern, ignore_case = TRUE))]
  if (length(matched) == 0) return(rep(NA_character_, nrow(raw)))
  if (length(matched) == 1) return(raw[[matched]])
  exec(coalesce, !!!as.list(raw[matched]))
}

read_production_harvest <- function(path, sheet, season) {
  raw <- read_excel(path, sheet = sheet, col_types = "text")

  tibble(
    pond = extract_col(raw, "^Pond$"),
    lot = extract_col(raw, "^Lot$"),
    stocked_date_raw = extract_col(raw, "^Stocked? Date$"),
    start_number_raw = extract_col(raw, "^Start #$"),
    start_tl_mm_raw = extract_col(raw, "^Start TL$"),
    harvest_date_raw = extract_col(raw, "^Harvest Date$"),
    end_number_raw = extract_col(raw, "^End # Live$|^End #$"),
    end_tl_mm_raw = extract_col(raw, "^End TL$"),
    days_in_pond_raw = extract_col(raw, "^Days in Pond$"),
    growth_mm_day_raw = extract_col(raw, "^Daily Growth Rate \\(mm\\)$|^Growth \\(mm/day\\)$"),
    harvest_morts_raw = extract_col(raw, "^Harv(est)? Morts$"),
    survival_percent_raw = extract_col(raw, "^Survival$"),
    stocked_to = extract_col(raw, "^Stocked ?[Tt]o(/#)?$")
  ) |>
    filter(!is.na(pond), pond != "Pond") |>
    transmute(
      season = season,
      season_type = str_extract(season, "^[A-Za-z]+"),
      season_year = as.numeric(str_extract(season, "\\d{4}")),
      pond = str_to_upper(pond),
      lot,
      stocked_date = excel_date(stocked_date_raw),
      start_number = num(start_number_raw),
      start_tl_mm = num(start_tl_mm_raw),
      harvest_date = excel_date(harvest_date_raw),
      end_number = num(end_number_raw),
      end_tl_mm = num(end_tl_mm_raw),
      days_in_pond = num(days_in_pond_raw),
      growth_mm_day = num(growth_mm_day_raw),
      harvest_morts = num(harvest_morts_raw),
      survival_percent = num(survival_percent_raw),
      stocked_to
    )
}

wb_2024 <- xl("FY2024 Supporting Excel Tables", "Production Harvest Data Sheet.xlsx")
wb_2025 <- xl("FY2025 Supporting Excel Tables", "FY2025 Production Harvest Data Sheet_20251107.xlsx")

production_harvest_sources <- tribble(
  ~path,   ~sheet,         ~season,
  wb_2024, "Fall 2021",    "Fall 2021",
  wb_2024, "Spring 2022",  "Spring 2022",
  wb_2024, "Fall 2022",    "Fall 2022",
  wb_2024, "SPRING 2023",  "Spring 2023",
  wb_2024, "FALL 2023",    "Fall 2023",
  wb_2024, "SPRING 2024",  "Spring 2024",
  wb_2025, "FALL 2024",    "Fall 2024",
  wb_2025, "Spring 2025",  "Spring 2025",
  wb_2025, "Fall 2025",    "Fall 2025"
)

production_harvest_raw <- production_harvest_sources |>
  pmap(read_production_harvest) |>
  list_rbind() |>
  mutate(survival_percent = if_else(survival_percent == 0 & is.na(end_number),
                                    NA_real_, survival_percent)) |>
  group_by(season) |>
  # some seasons record survival as a fraction (0-1) rather than a percent
  mutate(survival_percent = if (!all(is.na(survival_percent)) && max(survival_percent, na.rm = TRUE) <= 1) {
    survival_percent * 100
  } else {
    survival_percent
  }) |>
  ungroup() |>
  # fiscal year (Oct-Sep) from the harvest date; cycles not yet harvested get none
  mutate(fiscal_year = fiscal_year_of(harvest_date))

### 2a.3 Family groups =======
# Spawning crosses: wild East Side Springs LRS (FY2024, FY2025) from the "METADATA FOR
# ALL ESS LRS FISH SPAWNED" workbooks, and FY2025 captive broodstock (SNS, LRS) from
# FY25CaptiveBroodSpawning_FINAL.xlsx.

# The spawning block sits to the right of the adult table. Find it by the "Female" header.
spawn_block <- function(path, sheet) {
  raw <- read_text_sheet(path, sheet)
  start <- which(names(raw) == "Female")[1] - 1             # the date column precedes "Female"
  block <- raw[, start:ncol(raw)]
  names(block) <- make.unique(names(block), sep = "_")      # duplicates only matter within the block
  names(block)[1] <- "spawn_date"                           # "Date" / "Spawning Date" depending on the year
  block
}

male_columns <- function(block) names(block)[str_detect(names(block), "^Male")]

wild_2024 <- spawn_block(xl("FY2024 Supporting Excel Tables", "METADATA FOR ALL ESS LRS FISH SPAWNED (version 1).xlsx"),
                         "FY24 Tables Reorganized - MY")
wild_2024 <- wild_2024 |>
  filter(!is.na(Female), !is.na(spawn_date)) |>
  mutate(across(all_of(male_columns(wild_2024)), ~ na_if(.x, "-")))
male_cols_24 <- male_columns(wild_2024)

fg_wild_2024 <- wild_2024 |>
  mutate(n_males = rowSums(!is.na(pick(all_of(male_cols_24))))) |>
  transmute(
    `Fiscal Year` = 2024, Program = "Wild ESS Spawning", Species = "LRS",
    `Source Table` = "FY2024 Report, Table 3",
    spawning_date = excel_date(spawn_date),
    `Female ID` = Female,
    male_1 = .data[[male_cols_24[1]]], male_2 = .data[[male_cols_24[2]]],
    male_3 = .data[[male_cols_24[3]]], male_4 = .data[[male_cols_24[4]]],
    male_5 = NA_character_, male_6 = NA_character_,
    `Number of Males in Cross` = n_males,
    `Family Groups` = n_males,                                # FY2024 sheet has one family group per male
    `Incubation Vessel` = Incubator,
    `Egg Volume (mL)` = num(`Egg Volume (mL)`),
    `Eggs per mL` = num(`# eggs/mL`),
    `Total Eggs` = round_half_up(num(`Total Eggs`)),
    `1st Fry Count` = num(`Total Fry`),
    `1st Hatch Rate (%)` = round_half_up(num(`Hatch (%)`), 1),
    `2nd Fry Count` = NA_real_, `2nd Hatch Rate (%)` = NA_real_,
    hatch_date = excel_date(`Hatch Date`)
  )

wild_2025_raw <- spawn_block(xl("FY2025 Supporting Excel Tables", "FY25 METADATA FOR ALL ESS LRS FISH SPAWNED.xlsx"),
                             "FY25 Tables Reorganized ")
male_cols_25 <- male_columns(wild_2025_raw)
wild_2025 <- wild_2025_raw |>
  select(1:which(names(wild_2025_raw) == "Hatch (%)_1")) |>      # up to the 2nd hatch rate
  filter(!is.na(Female), Female != "Female") |>
  mutate(across(all_of(male_cols_25), ~ na_if(.x, "-")))

fg_wild_2025 <- wild_2025 |>
  mutate(n_males = rowSums(!is.na(pick(all_of(male_cols_25))))) |>
  transmute(
    `Fiscal Year` = 2025, Program = "Wild ESS Spawning", Species = "LRS",
    `Source Table` = "FY2025 Report, Table 3",
    spawning_date = excel_date(spawn_date),
    # one line item is not a named female cross: fish that escaped (fry only, no female ID)
    `Female ID` = if_else(Female == "Escaped",
                          "(unattributed \u2014 \"Escaped\" line item, not a named female cross)", Female),
    male_1 = .data[[male_cols_25[1]]], male_2 = .data[[male_cols_25[2]]],
    male_3 = .data[[male_cols_25[3]]], male_4 = .data[[male_cols_25[4]]],
    male_5 = .data[[male_cols_25[5]]], male_6 = .data[[male_cols_25[6]]],
    `Number of Males in Cross` = n_males,
    `Family Groups` = num(`Family Groups`),
    `Incubation Vessel` = NA_character_,
    `Egg Volume (mL)` = num(`Egg (mL)`),
    `Eggs per mL` = num(`# Eggs/mL`),
    `Total Eggs` = round_half_up(num(`Total Eggs`)),
    `1st Fry Count` = num(`1st Fry Count`),
    `1st Hatch Rate (%)` = round_half_up(num(`Hatch (%)`), 1),
    `2nd Fry Count` = num(`2nd Fry Count`),
    `2nd Hatch Rate (%)` = round_half_up(num(`Hatch (%)_1`), 1),
    hatch_date = as.Date(NA)
  )
# the "Escaped" line has its fry count in the 2nd count column of the sheet; keep it where it is

captive_path <- xl("FY2025 Supporting Excel Tables", "FY25CaptiveBroodSpawning_FINAL.xlsx")
captive_cross <- function(sheet, species, source_table) {
  raw <- read_text_sheet(captive_path, sheet)[, 1:12]      # the cross table; a PIT list sits to its right
  names(raw) <- make.unique(names(raw), sep = "_")
  raw |>
    filter(!is.na(Female), !is.na(Date)) |>
    transmute(
      `Fiscal Year` = 2025, Program = "Captive Broodstock Spawning", Species = species,
      `Source Table` = source_table,
      spawning_date = excel_date(Date),
      `Female ID` = Female,
      male_1 = Male, male_2 = Male_1, male_3 = NA_character_, male_4 = NA_character_,
      male_5 = NA_character_, male_6 = NA_character_,
      `Number of Males in Cross` = rowSums(!is.na(pick(Male, Male_1))),
      `Family Groups` = num(`Family Groups`),
      `Incubation Vessel` = NA_character_,
      `Egg Volume (mL)` = num(`Eggs (ml)`),
      `Eggs per mL` = round_half_up(num(`#Eggs/ml`), 2),
      `Total Eggs` = round_half_up(num(`Total Eggs`)),
      `1st Fry Count` = num(`1st Fry Count`),
      `1st Hatch Rate (%)` = round_half_up(num(`Hatch (%)`), 1),
      `2nd Fry Count` = num(`2nd Fry Count`),
      `2nd Hatch Rate (%)` = round_half_up(num(`Hatch (%)_1`), 1),
      hatch_date = as.Date(NA)
    )
}

# A few male IDs are written with only their last 4-5 characters in the FY2025 workbook. The FY2025
# report table has the full tag for most of them, so a short ID is expanded to the full 10-character PIT
# tag when exactly one tag in the workbook or the report tables ends with it (others stay short).
tables_2025 <- tables_raw_2025
fy25_workbook <- xl("FY2025 Supporting Excel Tables", "FY25 METADATA FOR ALL ESS LRS FISH SPAWNED.xlsx")
full_pit_tags <- c(
  excel_sheets(fy25_workbook) |>
    map(~ unlist(read_text_sheet(fy25_workbook, .x), use.names = FALSE)) |> unlist(),
  unlist(tables_2025, use.names = FALSE)
) |>
  str_subset("^[0-9A-F]{10}$") |>
  unique()

expand_pit <- function(id) {
  map_chr(id, function(x) {
    if (is.na(x) || nchar(x) >= 10) return(x)
    hits <- full_pit_tags[str_ends(full_pit_tags, x)]
    if (length(hits) == 1) hits else x
  })
}

family_groups_input <- bind_rows(
  fg_wild_2024,
  fg_wild_2025,
  captive_cross("SNS", "SNS", "FY2025 Report, Table 5"),
  captive_cross("LRS", "LRS", "FY2025 Report, Table 6")
) |>
  mutate(across(c(male_1, male_2, male_3, male_4, male_5, male_6), expand_pit)) |>
  rename(`Male ID 1` = male_1, `Male ID 2` = male_2, `Male ID 3` = male_3,
         `Male ID 4` = male_4, `Male ID 5` = male_5, `Male ID 6` = male_6,
         `Spawning / Cross Date` = spawning_date, `Hatch Date` = hatch_date) |>
  mutate(`Spawning / Cross Date` = us_date(`Spawning / Cross Date`),
         `Hatch Date` = us_date(`Hatch Date`)) |>
  select(`Fiscal Year`, Program, Species, `Source Table`, `Spawning / Cross Date`, `Female ID`,
         `Male ID 1`, `Male ID 2`, `Male ID 3`, `Male ID 4`, `Male ID 5`, `Male ID 6`,
         `Number of Males in Cross`, `Family Groups`, `Incubation Vessel`, `Egg Volume (mL)`,
         `Eggs per mL`, `Total Eggs`, `1st Fry Count`, `1st Hatch Rate (%)`,
         `2nd Fry Count`, `2nd Hatch Rate (%)`, `Hatch Date`)

# wild LRS crosses with a spawning date (same filter and renames as the exploration Rmd)
wild_lrs_family_groups_input <- family_groups_input |>
  rename(fiscal_year = `Fiscal Year`, program = Program, species = Species,
         source_table = `Source Table`, spawning_date = `Spawning / Cross Date`,
         female_id = `Female ID`, n_males = `Number of Males in Cross`,
         family_groups = `Family Groups`, incubation_vessel = `Incubation Vessel`,
         egg_volume_ml = `Egg Volume (mL)`, eggs_per_ml = `Eggs per mL`,
         total_eggs = `Total Eggs`, fry_count = `1st Fry Count`,
         hatch_rate_percent = `1st Hatch Rate (%)`, hatch_date = `Hatch Date`) |>
  mutate(spawning_date = mdy(spawning_date), hatch_date = mdy(hatch_date)) |>
  filter(program == "Wild ESS Spawning", species == "LRS", !is.na(spawning_date))

### 2a.4 Broodstock spawned =========================
# Number of captive broodstock spawned in spring 2025, by capture year and sex. This summary
# table is in the "All PIT" sheet of the captive broodstock workbook (columns
# "Capture Year (CY)", then SNS F/M and LRS F/M).

brood_raw <- read_text_sheet(captive_path, "All PIT")
cy_col <- which(names(brood_raw) == "Capture Year (CY)")
brood_block <- brood_raw[, cy_col:(cy_col + 4)]
names(brood_block) <- c("capture_year_raw", "SNS_F", "SNS_M", "LRS_F", "LRS_M")
brood_block <- brood_block[-1, ] |>                                   # first row is the F/M header
  filter(!is.na(capture_year_raw))

broodstock_spawned_input <- brood_block |>
  pivot_longer(-capture_year_raw, names_to = c("species", "sex"), names_sep = "_") |>
  pivot_wider(names_from = sex, values_from = value) |>
  transmute(capture_year_raw = if_else(capture_year_raw == "N/A", "N/A (not recorded)", capture_year_raw),
            species,
            females_spawned = num(F),
            males_spawned = num(M),
            total_spawned = females_spawned + males_spawned,
            capture_year = num(capture_year_raw)) |>
  # the csv lists SNS (2017-2021) first, then LRS (non-zero years)
  filter(!(females_spawned == 0 & males_spawned == 0)) |>
  arrange(desc(species), capture_year_raw) |>
  arrange(match(species, c("SNS", "LRS")))

### 2a.5 Age / facility lookup =================================
# Rule: one row per facility and fiscal year, `age` = the age class(es) of the fish held there.
#  * Net pens (nphistory.xlsx): age = operation year - capture year, starting_count = fish stocked.
#  * Ponds: every pond cycle (Production Harvest workbooks first, then the annual report tables for
#    cycles that are not in the workbooks) counts in the fiscal year it was stocked and in the fiscal year it
#    was harvested. The age class is that fiscal year minus the lot's collection year (4 = 4 or older);
#    East Side Springs and mixed lots are "other" and wild or salvage lots are "wild". starting_count is the
#    number of fish at the start of the first cycle harvested in the year.
#    These are the age classes the reports and workbooks support. The hand-compiled lookup this replaces
#    (data-raw/hatchery-resources/usfws_sarp_age_facility_lookup.csv) lists extra classes for ponds
#    that held more than one group in a year, and some other age conventions, from information provided by the
#    hatchery that is not in the workbooks or the reports (see data-raw/hatchery_data_integration.Rmd).

net_pen_lookup <- function() {
  path <- xl("FY2025 Supporting Excel Tables", "nphistory.xlsx")
  map(c("UKL", "Gerber"), ~ read_text_sheet(path, .x)) |>
    list_rbind() |>
    transmute(fiscal_year = num(`Operation Year`),
              capture_year = num(`Fish CY`),
              facility = str_to_upper(`Pen ID`),
              starting_count = num(`# Stocked`)) |>
    filter(!is.na(facility)) |>
    mutate(age_num = fiscal_year - capture_year) |>
    group_by(fiscal_year, facility) |>
    summarise(age = paste(sort(unique(pmin(age_num, 4))), collapse = "/"),
              starting_count = sum(starting_count), .groups = "drop")
}

# Age classes are written in a fixed order: numbers, then "wild", then "other"
order_age_classes <- function(x) {
  x <- unique(x)
  paste(c(sort(x[str_detect(x, "^\\d$")]), x[x == "wild"], x[x == "other"]), collapse = "/")
}

pond_cycle_ages <- function(cycles) {
  cycles <- cycles |>
    filter(str_detect(pond, "^[ABCP]\\d+$")) |>             # outdoor ponds in the lookup (not the refuge ponds)
    mutate(fy_stocked = fiscal_year_of(stocked_date),
           fy_harvested = fiscal_year_of(harvest_date),
           lot_year = num(str_extract(lot, "\\d{4}")),
           lot_kind = case_when(str_detect(lot, regex("wild|salv", ignore_case = TRUE)) ~ "wild",
                                str_detect(lot, "ESS|SNS|MIX|/|-") | is.na(lot_year) ~ "other",
                                TRUE ~ "year"))
  age_class <- function(fy, lot_year, lot_kind) {
    if_else(lot_kind == "year", as.character(pmin(fy - lot_year, 4)), lot_kind)
  }
  # a cycle counts where it was stocked and where it was harvested
  in_stock_year <- cycles |> filter(!is.na(fy_stocked)) |>
    transmute(fiscal_year = fy_stocked, facility = pond, stocked_date, harvest_date, start_number,
              age_class = age_class(fy_stocked, lot_year, lot_kind))
  in_harvest_year <- cycles |> filter(!is.na(fy_harvested), is.na(fy_stocked) | fy_harvested != fy_stocked) |>
    transmute(fiscal_year = fy_harvested, facility = pond, stocked_date, harvest_date, start_number,
              age_class = age_class(fy_harvested, lot_year, lot_kind))

  ages <- bind_rows(in_stock_year, in_harvest_year) |>
    group_by(fiscal_year, facility) |>
    summarise(age = order_age_classes(age_class), .groups = "drop")
  counts <- cycles |>
    filter(!is.na(fy_harvested), !is.na(start_number)) |>
    group_by(fiscal_year = fy_harvested, facility = pond) |>
    arrange(coalesce(stocked_date, harvest_date), .by_group = TRUE) |>
    summarise(starting_count = first(start_number), .groups = "drop")
  left_join(ages, counts, by = c("fiscal_year", "facility"))
}

## 2b. FROM THE PDF REPORTS #####################################################

### 2b.1 Collections and releases ===================
# Table 1 of the FY2025 report repeats the full history (FY2016-2025). Larvae collected is taken
# from Table 7 (historical larval catches), which agrees with the hatchery workbook
# "Larval Collection and Early Rearing Summary Data"; Table 1 differs for FY2024 only
# (30,142 vs 30,150).

header_rows_to_names <- function(x) {
  x <- clean_names(x)
  new_names <- paste(names(x), x[1, ], sep = "_") |>
    gsub("_NA", "", x = _) |>
    make_clean_names()
  x[-1, ] |> setNames(new_names)
}

table_1 <- header_rows_to_names(tables_2025[[2]]) |>
  filter(str_detect(fiscal_year, "^\\d{4}$")) |>
  mutate(salvage_length_unit = str_extract(sl_or_tl_mm, "(?<=\\[)[A-Z]+(?=\\])"),
         across(-c(fiscal_year, salvage_length_unit), ~ suppressWarnings(parse_number(.x))),
         fiscal_year = as.numeric(fiscal_year))

table_7 <- tables_2025[[8]] |>
  clean_names() |>
  transmute(fiscal_year = num(year_spring),
            larvae_collected = suppressWarnings(parse_number(number_collected))) |>
  drop_na(fiscal_year)

# broodstock on station and cohort years come from the report narrative
broodstock_text <- map(2021:2025, function(y) {
  txt <- report_text(y)
  on_station <- str_match(txt, "(approximately|over) ([\\d,]+) (?:captive broodstock|fish currently on station)")
  cohort <- str_match(txt, "(?:collection years \\(CY\\) |range from the )(\\d{4})-(\\d{4})")
  selected <- str_match(txt, "not counting the ([\\d,]+) broodstock selected")
  tibble(fiscal_year = y,
         broodstock_on_station_raw = if_else(is.na(on_station[1, 1]), NA_character_,
                                             paste(on_station[1, 2], on_station[1, 3])),
         broodstock_cohort_years = if_else(is.na(cohort[1, 1]), NA_character_,
                                           paste0("CY", cohort[1, 2], "-", cohort[1, 3])),
         new_broodstock_reported = num(str_remove(selected[1, 2], ",")))
}) |>
  list_rbind() |>
  mutate(broodstock_on_station = suppressWarnings(parse_number(broodstock_on_station_raw)))

collected_released_input <- table_1 |>
  select(-larvae_collected) |>
  left_join(table_7, by = "fiscal_year") |>
  transmute(fiscal_year,
            larvae_collected,
            sarp_release, sarp_tl_mm = tl_4_mm,
            fingerling_release, fingerling_tl_mm = tl_6_mm,
            fry_release, fry_tl_mm = tl_8_mm,
            salvage_release,
            salvage_length = sl_or_tl_mm,
            salvage_length_unit,
            total_released = rowSums(pick(sarp_release, fingerling_release, fry_release, salvage_release),
                                     na.rm = TRUE)) |>
  left_join(broodstock_text, by = "fiscal_year")

### 2b.2 Capacity ==================================
# Outdoor pond counts and acreage and indoor tank gallons from the facility description in each
# report. Pond inventories are entered here with the phrase from the report that supports them,
# and every phrase is checked against the report text so a wrong or moved citation fails loudly.

capacity_inventory <- tribble(
  ~fiscal_year, ~capacity_type, ~report, ~phrase,                                                                 ~ponds_0.03, ~ponds_0.125, ~ponds_0.25, ~n_ponds_override, ~acres_override,
  2021, "current", 2021, "46 smaller 0.03-acre ponds and four larger 0.25acre ponds",                                 46, 0, 4, NA, NA,
  2022, "current", 2022, "46 0.03-acre ponds and four 0.25-acre ponds",                                               46, 0, 4, NA, NA,
  2023, "current", 2023, "22 0.03-acre ponds in the P-series and four 0.25-acre ponds in the A-series",              22, 0, 4, NA, NA,
  # the four A-series ponds were demolished early in FY2024
  2024, "current", 2024, "22 0.03-acre ponds in the P-series and four 0.25-acre ponds in the A-series",              22, 0, 0, NA, NA,
  2025, "current", 2025, "22 0.03-acre ponds in the P-series and six 0.25-acre ponds in the C-series and four 0.125-acre ponds", 22, 4, 6, NA, NA,
  2028, "planned construction", 2025, "total of 33 production ponds, totaling approximately 8.5 acres",              NA, NA, NA, 33, 8.5
)

# compare ignoring spaces and hyphens (PDF text extraction splits words at line ends differently)
squash <- function(x) str_remove_all(x, "[^A-Za-z0-9.]")
phrase_in_report <- function(year, phrase) str_detect(squash(report_text(year)), fixed(squash(phrase)))

stopifnot(all(map2_lgl(capacity_inventory$report, capacity_inventory$phrase, phrase_in_report)))
# FY2024: the demolition of the A-series ponds must also be stated
stopifnot(phrase_in_report(2024, "the four 0.25-acre ponds in the A-series were also demolished"))

# indoor tank gallons: sum of "<count> [description] <size>-gallon" in the indoor facility description.
# The text is read from the first mention of the indoor facility (and, in FY2025, from the description of
# the second indoor building); later mentions of the same tanks (figure captions) are not counted again.
number_words <- c(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7, eight = 8, nine = 9,
                  ten = 10, eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15)
number_word_pattern <- paste(names(number_words), collapse = "|")

gallons_in <- function(txt) {
  # research racks: "four Research Racks ... each with a 180-gallon ... sump" (size is far from the count)
  rack_pattern <- paste0("\\b(", number_word_pattern, ")\\s+Research Racks[^.]*?(\\d+)-gallon")
  racks <- str_match_all(txt, rack_pattern)[[1]]
  rack_gallons <- sum(unname(number_words[racks[, 2]]) * num(racks[, 3]))
  txt <- str_remove_all(txt, rack_pattern)

  # the count is a number, "(24)" or a number word
  parts <- str_match_all(
    txt,
    paste0("(?:\\(?(\\d+)\\)?|\\b(", number_word_pattern, ")\\b)\\s+(?:[A-Za-z\\- ]*?)(\\d+)-?\\s?gallon(?!s per)"))[[1]]
  count <- coalesce(num(parts[, 2]), unname(number_words[parts[, 3]]))
  sum(count * num(parts[, 4])) + rack_gallons
}

indoor_gallons <- function(year) {
  txt <- report_text(year)
  window <- function(pattern, width) {
    start <- str_locate(txt, pattern)[1, 1]
    if (is.na(start)) NA_character_ else str_sub(txt, start, start + width)
  }
  first <- window("[Tt]he (original )?indoor (intensive )?(rearing )?facilit", 750)
  second <- window("intensive rearing facilities, constructed in 2021", 600)
  if (is.na(first)) return(NA_real_)
  gallons_in(first) + if (is.na(second)) 0 else gallons_in(second)
}

usfws_sarp_capacity_input <- capacity_inventory |>
  mutate(n_outdoor_ponds = coalesce(n_ponds_override, `ponds_0.03` + `ponds_0.125` + `ponds_0.25`),
         acreage_outdoor_ponds = coalesce(acres_override,
                                          round(`ponds_0.03` * 0.03 + `ponds_0.125` * 0.125 + `ponds_0.25` * 0.25, 2)),
         gallons_indoor = map_dbl(report, indoor_gallons),
         gallons_indoor = if_else(capacity_type == "current", gallons_indoor, NA_real_)) |>
  select(fiscal_year, n_outdoor_ponds, acreage_outdoor_ponds, gallons_indoor, capacity_type)

## 2c. MORTALITY EVENTS ########################################################
# The events are the disease, water quality, weather, predation and other events described in the narrative sections
# of the FY2021-2025 reports. Finding an event, naming its category and cause, and describing the affected
# population is a reading of the report narrative, so the descriptions of events E01-E28 are read from
# data-raw/hatchery-resources/mortality_events.csv and the descriptions of E29-E37 (FY2025) are written in the final
# build below. Everything else comes from the reports and workbooks:
#
#   2c.1 every event is tied to a phrase in the report that describes it (the script stops if a phrase is
#        not found, so a mis-cited event cannot go unnoticed);
#   2c.2 the loss (percent and number of fish) is computed from the net pen records (nphistory.xlsx),
#        the larval rearing tables (workbook and report table) and numbers in the report text;
#   2c.3 the severity tier is computed from the loss with a fixed rule.

event_descriptions <- read_csv(file.path(resources_dir, "mortality_events.csv"), show_col_types = FALSE)
names(event_descriptions) <- str_remove(names(event_descriptions), "^﻿")
report_texts <- map(2021:2025, report_text) |> set_names(2021:2025)

### 2c.1 Phrase in the report that describes each event ---------------------------
# (report year, regular expression). `report` is the first report that describes the event.

event_anchors <- tribble(
  ~event_id, ~report, ~anchor,
  "E01", 2023, "avian predators may have infiltrated the netting in 2019",
  "E02", 2021, "marked increase in total ammonia levels",
  "E03", 2021, "all perished during the anticipated hatch",
  "E04", 2021, "total losses within days of hatch",
  "E05", 2021, "unhatched Artemia cysts",
  "E06", 2021, "Costia outbreak during the middle of June",
  "E07", 2021, "emergency harvest of the Gerber Reservoir Net pen",
  "E08", 2021, "only 606 fish were recovered",
  "E09", 2022, "signs of tetany",
  "E10", 2022, "necrosis of the gill lamellae",
  "E11", 2022, "200\\+ fish lost during transport",
  "E12", 2022, "fathead minnows, tui chub, and blue chub",
  "E13", 2022, "low river flows led to ineffective catch rates",
  "E14", 2023, "extreme gill hyperplasia",
  "E15", 2023, "losing larval fish to tank overflow",
  "E16", 2023, "mortality event in Tank C14",
  "E17", 2023, "anoxic conditions forming",
  "E18", 2023, "nearly half of the fish in pen escaped",
  "E19", 2023, "73,869 eggs counted",
  "E20", 2023, "ovulation rates of the females",
  "E21", 2023, "lowest on record",
  "E22", 2024, "acute loss of several thousand fish",
  "E23", 2024, "Costia infections for the extended intensive rearing",
  "E24", 2024, "cumulative human error",
  "E25", 2024, "bacterial infection spread early in the holding period",
  "E26", 2024, "data was lost in parts of June and August",
  "E27", 2024, "cyanobacteria bloom was detected",
  "E28", 2024, "three small \\(~30 mm\\) perch were found dead"
)

anchor_check <- event_anchors |>
  mutate(found = map2_lgl(report, anchor, ~ str_detect(report_texts[[as.character(.x)]], .y)))
stopifnot("every event must be found in its report" = all(anchor_check$found),
          "every described event needs an anchor" = setequal(event_descriptions$`Event ID`, event_anchors$event_id))

### 2c.2 Loss from the data tables and the report text ----------------------------

# net pens (UKL): fish stocked and harvested per operation year
ukl_by_year <- read_text_sheet(xl("FY2025 Supporting Excel Tables", "nphistory.xlsx"), "UKL") |>
  transmute(operation_year = num(`Operation Year`), stocked = num(`# Stocked`), harvested = num(`# Harvested`)) |>
  filter(!is.na(operation_year), !is.na(stocked)) |>
  group_by(operation_year) |>
  summarise(stocked = sum(stocked), harvested = sum(harvested), .groups = "drop")
ukl <- function(year) ukl_by_year |> filter(operation_year == year)

# Gerber east pen, 2023: fish that were stocked and recovered (the rest escaped through holes in the net)
gere_2023 <- read_text_sheet(xl("FY2025 Supporting Excel Tables", "nphistory.xlsx"), "Gerber") |>
  filter(`Operation Year` == "2023", `Pen ID` == "GERE") |>
  transmute(stocked = num(`# Stocked`), harvested = num(`# Harvested`))

# larval rearing: FY2023 tank C14 (report table) and the FY2024 totals (workbook)
tank_c14 <- load_pdf_tables(2023)[[4]] |> filter(Culture == "C14")
larval_2024_total <- read_excel(xl("FY2024 Supporting Excel Tables",
                                   "2024 Larval Collection and Early Rearing Summary Data.xlsx"),
                                sheet = "Pond Performance Table 4 Report") |>
  filter(is.na(Date))

# FY2023 diet trial: survival of the Razorback (RZB) diet fingerlings, from the report table
rzb_fingerling <- load_pdf_tables(2023)[[14]]
rzb_survival <- parse_number(rzb_fingerling[[1]][str_detect(rzb_fingerling[[1]], "^Survival")])

# numbers that are only in the report text
eggs_2023  <- str_match(report_texts[["2023"]], "In total ([\\d,]+) eggs counted and transferred to the KFNFH and ([\\d,]+) fry were successfully hatched")
transport  <- str_match(report_texts[["2022"]], "was stocked 4/8/2022 with ([\\d,]+) fish with an estimated ([\\d,]+)\\+ fish lost")
c14_lost   <- str_match(report_texts[["2023"]], "mortality event in Tank C14 from July 2 through July 4 of ([\\d,]+) fish")
holding    <- str_match(report_texts[["2024"]], "survival during this second holding period was (\\d+) percent")
as_n <- function(x) num(str_remove_all(x, ","))

# at_risk / lost: fish at risk and fish lost; percent: used when the report gives a percent or a
# survival instead of counts; source: where the number comes from.
event_losses <- tribble(
  ~event_id, ~at_risk, ~lost, ~percent, ~source,
  "E01", ukl(2019)$stocked, ukl(2019)$stocked - ukl(2019)$harvested, NA, "nphistory.xlsx (UKL 2019)",
  "E03", NA, NA, 100, "report text: all perished",
  "E04", NA, NA, 100, "report text: total losses",
  # E07 (Gerber net pen emergency harvest, 2,175 fish moved): the report gives no loss, so no percent is computed
  "E08", ukl(2021)$stocked, NA, 100 * (1 - ukl(2021)$harvested / ukl(2021)$stocked), "nphistory.xlsx (UKL 2021)",
  "E11", as_n(transport[1, 2]) + as_n(transport[1, 3]), as_n(transport[1, 3]), NA, "report text: 200+ lost in transport",
  "E12", ukl(2022)$stocked, ukl(2022)$stocked - ukl(2022)$harvested, NA, "nphistory.xlsx (UKL 2022)",
  "E14", NA, NA, 100 - rzb_survival, "FY2023 Table 14 (RZB fingerling survival)",
  "E16", NA, as_n(c14_lost[1, 2]), 100 - num(tank_c14$Overall), "FY2023 table (tank C14 survival)",
  "E17", ukl(2023)$stocked, ukl(2023)$stocked - ukl(2023)$harvested, NA, "nphistory.xlsx (UKL 2023)",
  "E18", gere_2023$stocked, gere_2023$stocked - gere_2023$harvested, NA, "nphistory.xlsx (Gerber GERE 2023, apparent loss)",
  "E19", as_n(eggs_2023[1, 2]), as_n(eggs_2023[1, 2]) - as_n(eggs_2023[1, 3]), NA, "report text: eggs and fry",
  # the FY2024 report only says "several thousand fish" were lost, so there is no number to compute from
  "E22", NA, NA, NA, "not quantified in the report (several thousand fish)",
  "E23", num(larval_2024_total$Collected), num(larval_2024_total$Mortality), NA, "larval rearing workbook (FY2024 total)",
  "E24", NA, NA, num(larval_2024_total$`Unobserved Mortality (%)`), "larval rearing workbook (FY2024 total)",
  "E25", NA, NA, 100 - num(holding[1, 2]), "report text: 45 percent survival",
  "E26", NA, NA, 0, "report text: no sucker mortality",
  "E27", NA, NA, 0, "report text: no mortality attributed"
) |>
  mutate(percent_lost = round_half_up(coalesce(percent, 100 * lost / at_risk), 1),
         # fish lost are only reported for these events
         n_mortality = if_else(event_id %in% c("E01", "E11", "E12", "E16"), lost, NA_real_))

### 2c.3 Severity tier from the loss ---------------------------------------------
severity_tier <- function(percent_lost, mortality_event) {
  case_when(
    mortality_event == "N" & !is.na(percent_lost) & percent_lost == 0 ~ "Near-miss (no mortality observed)",
    mortality_event == "N" ~ "Non-mortality (escape/collection/reproductive)",
    is.na(percent_lost) ~ "Qualitative only (unquantified)",
    percent_lost > 70 ~ "Catastrophic (>70% loss)",
    percent_lost >= 30 ~ "Major (30-70% loss)",
    percent_lost >= 10 ~ "Moderate (10-30% loss)",
    TRUE ~ "Minor (<10% loss)"
  )
}

mortality_events_input <- event_descriptions |>
  left_join(event_losses |> select(event_id, percent_lost, n_mortality), by = c(`Event ID` = "event_id")) |>
  mutate(`% Lost (best estimate, numeric)` = if_else(is.na(percent_lost), NA_character_, sprintf("%.1f%%", percent_lost)),
         `Severity Tier` = severity_tier(percent_lost, `Mortality Event? (Y/N)`)) |>
  select(-percent_lost, -n_mortality)

mortality_events_summary_input <- mortality_events_input |>
  left_join(event_losses |> select(event_id, percent_lost, n_mortality), by = c(`Event ID` = "event_id")) |>
  transmute(id = `Event ID`,
            year = `Actual Year`,
            category = `Event Category`,
            mortality_event = `Mortality Event? (Y/N)`,
            percent_mortality = if_else(is.na(percent_lost), NA_character_, sprintf("%.2f%%", percent_lost)),
            n_mortality)



## LOOKUP, MORTALITY SPREADSHEETS AND PRODUCTION HARVEST ########################
# FY2021 pond cycles (and stock dates the workbooks lack) come from the report tables built above.

lookup_input <- bind_rows(
  net_pen_lookup() |> mutate(source = "nphistory.xlsx"),
  pond_cycle_ages(bind_rows(
    production_harvest_raw |> select(pond, lot, stocked_date, harvest_date, start_number),
    pond_growout_reports_2021_2024 |>
      select(pond, lot, stocked_date = pond_stock_date, harvest_date, start_number)) |>
      distinct(pond, stocked_date, harvest_date, .keep_all = TRUE)) |>
    mutate(source = "pond cycles")
) |>
  arrange(fiscal_year, facility)

mortality_spreadsheets_input <- bind_rows(mortality_fy2023, mortality_fy2024, mortality_fy2025, harvest_fy2023) |>
  left_join(lookup_input |> select(fiscal_year, facility, age, starting_count),
            by = c("fiscal_year", "pond" = "facility"))

production_harvest_input <- production_harvest_raw |>
  left_join(lookup_input |> select(fiscal_year, facility, age, starting_count),
            by = c("fiscal_year", "pond" = "facility"))




## 3. FINAL OBJECTS ############################################################
# Merges the FY2021-2024 report tables with the FY2025 report tables and the workbook inputs above, and saves the
# package data objects. The intermediate objects (hatchery_release_2021_2024, pond_growout_reports_2021_2024, ...)
# come from the PROCESS TABLES sections at the top of this script.

### Age / facility lookup ======================================================
# One source of fish age by facility and fiscal year, joined to every object that has a pond or facility.

KFNFH_age_facility_lookup <- lookup_input |>
  select(fiscal_year, facility, age, starting_count) |>
  mutate(facility = str_to_upper(facility)) |>
  rename(age_class = age) |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = fiscal_year)

# the join key must be unique or joins would duplicate rows
stopifnot(!anyDuplicated(KFNFH_age_facility_lookup[c("fiscal_year", "facility")]))

add_age <- function(data, facility_col = "pond") {
  data |>
    select(-any_of(c("age", "age_class", "starting_count"))) |>
    left_join(KFNFH_age_facility_lookup |>
                select(fiscal_year, facility, age_class, starting_count),
              by = c("fiscal_year", setNames("facility", facility_col)))
}


### Collection and release history ==================================================

parse_header_rows <- function(x) {
  x <- clean_names(x)
  new_names <- paste(names(x), x[1, ], sep = "_") |>
    gsub("_NA", "", x = _) |>
    make_clean_names()
  x[-1, ] |> setNames(new_names)
}

table_1_2025 <- parse_header_rows(tables_raw_2025[[2]]) |>
  filter(str_detect(fiscal_year, "^\\d{4}$")) |>
  mutate(salvage_basis = str_extract(sl_or_tl_mm, "(?<=\\[)[A-Z]+(?=\\])"),
         across(-c(fiscal_year, salvage_basis), ~ suppressWarnings(parse_number(.x))),
         fiscal_year = as.numeric(fiscal_year))

larval_catch_2025 <- tables_raw_2025[[8]] |>
  clean_names() |>
  transmute(fiscal_year = suppressWarnings(as.numeric(year_spring)),
            larvae_collected_table_7 = suppressWarnings(parse_number(number_collected)),
            primary_collection_method = primary_collection_method |>
              str_squish() |>
              str_replace("\\)\\s*/\\s*", ") / ")) |>
  drop_na(fiscal_year)

broodstock_cols <- collected_released_input |>
  select(fiscal_year, total_released, broodstock_on_station_raw,
         broodstock_cohort_years, new_broodstock_reported, broodstock_on_station)

KFNFH_historical_collection_release <- table_1_2025 |>
  transmute(hatchery_name = "KFNFH",
            fiscal_year,
            larvae_collected_table_1 = larvae_collected,
            sarp_release,
            sarp_tl_mm = tl_4_mm,
            fingerling_release,
            fingerling_tl_mm = tl_6_mm,
            fry_release,
            fry_tl_mm = tl_8_mm,
            salvage_release,
            salvage_sl_mm = if_else(salvage_basis %in% "SL", sl_or_tl_mm, NA_real_),
            salvage_tl_mm = if_else(salvage_basis %in% "TL", sl_or_tl_mm, NA_real_)) |>
  left_join(larval_catch_2025, by = "fiscal_year") |>
  left_join(broodstock_cols, by = "fiscal_year") |>
  # FY2025 Table 1 and Table 7 disagree for FY2024 (30,142 vs 30,150); Table 7 is used, see below
  mutate(larvae_collected = larvae_collected_table_7) |>
  relocate(larvae_collected, .after = fiscal_year) |>
  select(-larvae_collected_table_1, -larvae_collected_table_7)

KFNFH_historical_collection_release


### Pond grow-out ===============================================================

parse_pond_fy25 <- function(x, label) {
  x <- x |> mutate(across(everything(), as.character))
  names(x) <- c("pond_lot", "stocked", "blank", "start_number", "start_g_fish",
                "start_wt_lb", "start_wt_g", "start_tl_mm", "harvest_date",
                "end_number", "end_g_fish", "end_wt_lb", "end_wt_g", "end_tl_mm",
                "days", "col16", "col17", "harvest_mortality_number", "survival_percent")
  num <- function(v) suppressWarnings(as.numeric(str_remove_all(v, ",")))
  x |>
    mutate(stock_date = suppressWarnings(mdy(stocked))) |>
    filter(!is.na(stock_date)) |>          # drops the two header rows and the lot summary rows
    mutate(months_growth = str_squish(paste(replace_na(col16, ""), replace_na(col17, "")))) |>
    transmute(
      inventory_source = label,
      pond = word(pond_lot, 1),
      lot = str_squish(str_remove(pond_lot, "^\\S+")),
      pond_stock_date = stock_date,
      harvest_date = suppressWarnings(mdy(harvest_date)),
      start_number = num(start_number),
      start_g_fish = num(start_g_fish),
      start_wt_lb = num(start_wt_lb),
      start_wt_g = num(start_wt_g),
      start_tl_mm = num(start_tl_mm),
      end_number = num(end_number),
      end_g_fish = num(end_g_fish),
      end_wt_lb = num(end_wt_lb),
      end_wt_g = num(end_wt_g),
      end_tl_mm = num(end_tl_mm),
      days = num(days),
      months = num(word(months_growth, 1)),
      growth_mm_day = num(word(months_growth, 2)),
      harvest_mortality_number = num(harvest_mortality_number),
      survival_percent = num(survival_percent))
}

pond_2025_report <- bind_rows(
  parse_pond_fy25(tables_raw_2025[[11]], "FY2025 Table 10"),
  parse_pond_fy25(tables_raw_2025[[12]], "FY2025 Table 11"),
  parse_pond_fy25(tables_raw_2025[[13]], "FY2025 Table 12")) |>
  mutate(hatchery_name = "KFNFH",
         fiscal_year = 2025,
         life_history = if_else(str_detect(lot, "ESS"), "ESS", "adfluvial"))

pond_2025_report |> count(inventory_source)


# the workbook repeats in-progress cycles across seasons; cycle keys used to compare it with the report tables
ph <- production_harvest_input |>
  select(-any_of(c("age", "starting_count", "fiscal_year")))

key_stock <- function(d, stock, pond = "pond") ifelse(is.na(d[[stock]]), NA, paste(d[[pond]], d[[stock]]))
key_harv  <- function(d, harv, pond = "pond")  ifelse(is.na(d[[harv]]),  NA, paste(d[[pond]], d[[harv]]))

existing <- pond_growout_reports_2021_2024 |>
  mutate(source = "annual_report_table",
         weight_gain_lb = coalesce(weight_gain_lb, weigth_gain_lb)) |>  # typo column merged
  select(-weigth_gain_lb)

report_rows <- bind_rows(existing, pond_2025_report |> mutate(source = "annual_report_table"))

# a cycle reported as in progress (no harvest date) is superseded by a later completed record
report_rows <- report_rows |>
  mutate(cycle_key = key_stock(pick(everything()), "pond_stock_date")) |>
  group_by(cycle_key) |>
  mutate(superseded = !is.na(cycle_key) & is.na(harvest_date) & n() > 1 & any(!is.na(harvest_date))) |>
  ungroup()

superseded_n <- sum(report_rows$superseded)
report_rows <- report_rows |> filter(!superseded) |> select(-superseded)

# the same completed cycle is listed in two consecutive reports (e.g. a cycle harvested in Oct-Nov is in the table of the
# report that ends in the previous September and again in the next report): keep the later report's row
rows_before_dedup <- nrow(report_rows)
report_rows <- report_rows |>
  arrange(desc(fiscal_year)) |>
  mutate(duplicate_cycle = !is.na(pond_stock_date) & duplicated(pick(pond, lot, pond_stock_date, harvest_date))) |>
  filter(!duplicate_cycle) |>
  select(-duplicate_cycle)
duplicates_removed_n <- rows_before_dedup - nrow(report_rows)

# workbook rows that are not in the report tables
ph_new <- ph |>
  mutate(k_stock = key_stock(pick(everything()), "stocked_date"),
         k_harv = key_harv(pick(everything()), "harvest_date"))

in_report <- ph_new$k_stock %in% report_rows$cycle_key |
  ph_new$k_harv %in% key_harv(report_rows, "harvest_date")

workbook_only <- ph_new[!in_report, ]

workbook_only |> count(season)

fiscal_year_of <- function(d) year(d) + if_else(month(d) >= 10, 1, 0)

workbook_rows <- workbook_only |>
  transmute(hatchery_name = "KFNFH",
            fiscal_year = fiscal_year_of(harvest_date),
            inventory_source = paste("Production Harvest Data Sheet,", season),
            life_history = if_else(str_detect(as.character(lot), "ESS"), "ESS", "adfluvial"),
            pond, lot = as.character(lot),
            pond_stock_date = stocked_date,
            harvest_date,
            start_number, start_tl_mm, end_number, end_tl_mm,
            days = days_in_pond,
            growth_mm_day,
            harvest_mortality_number = harvest_morts,
            survival_percent,
            dispo_to = stocked_to,
            source = "production_harvest_workbook")

KFNFH_pond_growout <- bind_rows(report_rows |> select(-cycle_key), workbook_rows) |>
  # some report tables list ponds by number only ("0", "1", ...); the lookup uses "P0", "P1"
  mutate(pond = if_else(str_detect(pond, "^\\d+$"), paste0("P", pond), pond)) |>
  add_age("pond") |>
  relocate(hatchery_name, fiscal_year, inventory_source, life_history, source, pond, lot) |>
  arrange(fiscal_year, pond_stock_date, pond)

cat("in-progress records superseded by a later completed record:", superseded_n, "\n")
cat("duplicate cycles removed (kept the later report):", duplicates_removed_n, "\n")
KFNFH_pond_growout |> count(fiscal_year, source)


### Incubation and hatch results ================================================

fy24_csv <- wild_lrs_family_groups_input |> filter(fiscal_year == 2024)
fy24_pdf <- KFNFH_LRS_ESS_incubation_hatch_2024

stopifnot(nrow(fy24_csv) == nrow(fy24_pdf),
          all(fy24_csv$female_id == fy24_pdf$female),
          all(fy24_csv$total_eggs == fy24_pdf$total_eggs),
          all(fy24_csv$spawning_date == fy24_pdf$spawning_date))

incubation_2024 <- fy24_pdf |>
  mutate(fiscal_year = 2024,
         program = fy24_csv$program,
         species = fy24_csv$species,
         family_groups = fy24_csv$family_groups,
         male_5 = NA_character_,
         male_6 = NA_character_,
         second_fry_count = NA_real_,
         second_hatch_percent = NA_real_)

incubation_2025 <- wild_lrs_family_groups_input |>
  filter(fiscal_year == 2025) |>
  transmute(fiscal_year, program, species,
            spawning_date,
            female = female_id,
            male_1 = `Male ID 1`, male_2 = `Male ID 2`, male_3 = `Male ID 3`,
            male_4 = `Male ID 4`, male_5 = `Male ID 5`, male_6 = `Male ID 6`,
            family_groups,
            incubator = incubation_vessel,
            egg_volume_mL = egg_volume_ml,
            eggs_per_mL = eggs_per_ml,
            total_eggs,
            total_fry = fry_count,
            hatch_percent = hatch_rate_percent,
            second_fry_count = `2nd Fry Count`,
            second_hatch_percent = `2nd Hatch Rate (%)`,
            hatch_date)

KFNFH_LRS_ESS_incubation_hatch <- bind_rows(incubation_2024, incubation_2025) |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, fiscal_year, program, species, .before = spawning_date) |>
  relocate(male_5, male_6, family_groups, .after = male_4)

KFNFH_LRS_ESS_incubation_hatch |> count(fiscal_year)


### Adult collection and hatchery release, FY2025 ==============================

adults_2025 <- tables_raw_2025[[3]] |>
  clean_names() |>
  transmute(hatchery_name = "KFNFH",
            fiscal_year = 2025,
            date = mdy(date),
            pit_tag_suffix = pit_tag,
            pit_tag_last_5 = str_sub(pit_tag, -5, -1),
            fin_clip_id_number = fin_clip,
            sex = recode(sex, M = "Male", F = "Female"),
            gamete_use = gamate_use,
            fl_mm = as.numeric(fork_length),
            spring_location,
            family_groups)

KFNFH_LRS_ESS_adult_collection <- bind_rows(
  adult_collection_2023_2024 |> mutate(across(any_of("fl_mm"), as.numeric)),
  adults_2025) |>
  mutate(fl_in = coalesce(fl_in, fl_mm / 25.4))

KFNFH_LRS_ESS_adult_collection |> count(fiscal_year, sex)

release_2025 <- tables_raw_2025[[16]] |>
  clean_names() |>
  rename(dispo_date = stock_date,
         dispo_location = stocking_location,
         number_per_lb = number_lb,
         from_ponds = from_ponds_salvage_area) |>
  mutate(dispo_date = na_if(as.character(dispo_date), ""),
         date_anchor = dispo_date,
         number_fish = number) |>
  fill(date_anchor, .direction = "down") |>
  mutate(dispo_date = coalesce(dispo_date, date_anchor)) |>
  select(-date_anchor) |>
  drop_na(dispo_location) |>       # drops the monthly and annual total rows
  mutate(dispo_date = as.Date(dispo_date, format = "%m/%d/%Y"),
         hatchery_name = "KFNFH",
         fiscal_year = 2025) |>
  select(-number) |>
  relocate(number_fish, .after = lot) |>
  relocate(hatchery_name, fiscal_year, .before = dispo_date)

KFNFH_hatchery_release <- bind_rows(release_2025, hatchery_release_2021_2024) |>
  arrange(desc(fiscal_year), dispo_date)

KFNFH_hatchery_release |> group_by(fiscal_year) |> summarise(rows = n(), fish = sum(number_fish, na.rm = TRUE))


### Net pen survival ============================================================

KFNFH_pen_survival <- pen_survival_reports_2024 |>
  mutate(note = if_else(pen_type == "Gerber Net Pen" & operation_year == 2022 & is.na(pen_id),
                        "Low water - not operational", NA_character_)) |>
  bind_rows(tribble(
    ~hatchery_name, ~pen_type, ~operation_year, ~note,
    "KFNFH", "Net Pen", 2024, "Experimental use only - no production fish stocked",
    "KFNFH", "Net Pen", 2025, "No fish available - not operational",
    "KFNFH", "Gerber Net Pen", 2025, "No fish available - not operational")) |>
  arrange(pen_type, operation_year)

KFNFH_pen_survival |> filter(!is.na(note))


### Early rearing ===============================================================

rearing_2023 <- tables_raw_2023[[4]] |>
  slice(-(1:2)) |>
  filter(!is.na(Collection)) |>            # drops the two header rows and the total row
  transmute(date_raw = Collection,
            date = mdy(Collection),
            culture_unit = Culture,
            collected = parse_number(Collected),
            mortality = parse_number(`Observed...4`),
            stocked = parse_number(Stocked),
            survival_percent = as.numeric(Overall),
            observed_mortality_percent = as.numeric(`Observed...7`),
            unobserved_mortality_percent = as.numeric(Unobserved),
            fiscal_year = 2023)

rearing_2025 <- tables_raw_2025[[9]] |>
  clean_names() |>
  drop_na(date) |>                         # drops the total row
  transmute(date_raw = date,
            date = mdy(str_extract(date, "^\\d{1,2}/\\d{1,2}/\\d{4}")),
            culture_unit,
            collected = as.numeric(collected),
            mortality = as.numeric(number_mortality),
            stocked = as.numeric(number_stocked),
            survival_percent = as.numeric(tank_survival_percent),
            observed_mortality_percent = as.numeric(observed_mortality_percent),
            unobserved_mortality_percent = as.numeric(unobserved_mortality_percent),
            fiscal_year = 2025)

KFNFH_adfluvial_early_rearing <- bind_rows(
  rearing_2023,
  KFNFH_adfluvial_early_rearing_2024 |>
    mutate(date_raw = format(date, "%m/%d/%Y") |> str_remove_all("(?<=^|/)0")),
  rearing_2025) |>
  mutate(hatchery_name = "KFNFH",
         restock = str_detect(culture_unit, regex("^restock", ignore_case = TRUE))) |>
  relocate(hatchery_name, fiscal_year, date_raw, date, .before = culture_unit) |>
  arrange(fiscal_year, date)

KFNFH_adfluvial_early_rearing |>
  group_by(fiscal_year) |>
  summarise(rows = n(), collected = sum(collected),
            collected_excl_restock = sum(collected[!restock]),
            stocked = sum(stocked),
            survival = round(100 * stocked / collected, 1))


### Mortality events ============================================================

pct <- function(x) suppressWarnings(parse_number(x))

KFNFH_mortality_events <- mortality_events_input |>
  transmute(event_id = `Event ID`,
            year = `Actual Year`,
            period = `Date / Period`,
            location = `Location / Pond-Tank-Pen`,
            category = `Event Category`,
            mortality_event = `Mortality Event? (Y/N)` == "Y",
            cause_description = `Cause Description`,
            population_affected = `Population Affected (denominator)`,
            loss_as_reported = `Number/Percent Lost (as reported)`,
            percent_lost = pct(`% Lost (best estimate, numeric)`),
            confidence = Confidence,
            severity_tier = `Severity Tier`) |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = event_id)

KFNFH_mortality_events_summary <- mortality_events_summary_input |>
  transmute(hatchery_name = "KFNFH",
            event_id = id,
            year,
            category,
            mortality_event = mortality_event == "Y",
            percent_mortality = pct(percent_mortality),
            n_mortality)

stopifnot(all(KFNFH_mortality_events$event_id == KFNFH_mortality_events_summary$event_id))

# FY2025 events: report text, WQ workbook (daily sheets + Sheet1), Production Harvest workbook, FY2025 Table 8
report_2025_text <- pdftools::pdf_text(here::here("data-raw/hatchery-resources/FY25 KFNFH Annual Report.pdf")) |>
  paste(collapse = " ") |>
  str_replace_all(c("ﬁ" = "fi", "ﬀ" = "ff", "ﬃ" = "ffi", "ﬂ" = "fl")) |>
  str_squish()

wq_2025_path <- here::here("data-raw/hatchery-resources/FY2025 Supporting Excel Tables/KFNFH_WQ_FY2025_2 (version 1).xlsx")
wq_2025_sheets <- c("OctobertoDecember_WQ", "JantoMar_WQ", "AprtoJun_WQ", "JulytoSept_WQ")

# daily pond log: pond, date, dissolved oxygen, mortalities (Morts), staff comments, lot (CY)
wq_2025 <- map(wq_2025_sheets, \(s) {
  readxl::read_excel(wq_2025_path, sheet = s, col_types = "text", .name_repair = "minimal")[, 1:11] |>
    set_names(c("pond", "timestamp", "temp", "do", "morts", "date", "comments", "ph", "nh3", "id", "lot")) |>
    mutate(sheet = s)
}) |>
  list_rbind() |>
  mutate(pond = str_to_upper(pond),
         date = as.Date(suppressWarnings(as.numeric(date)), origin = "1899-12-30"),
         morts = suppressWarnings(as.numeric(morts)),
         do = suppressWarnings(as.numeric(do))) |>
  filter(!is.na(pond), !is.na(date))

# Sheet1: fish at the start and at harvest for the ponds harvested early in September 2025
harvest_2025 <- readxl::read_excel(wq_2025_path, sheet = "Sheet1") |>
  set_names(c("pond", "harvest_date", "initial", "harvested", "harvest_morts", "survival")) |>
  filter(!is.na(initial))

# Sheet1 "Survival" is (harvested + harvest morts) / initial
stopifnot(all(abs(harvest_2025$survival - (harvest_2025$harvested + harvest_2025$harvest_morts) / harvest_2025$initial) < 1e-6))

# E29: ESS2025 ponds C1 and C2
c1_c2 <- harvest_2025 |> filter(pond %in% c("C1", "C2"))
c1_c2_lost <- sum(c1_c2$initial) - sum(c1_c2$harvested + c1_c2$harvest_morts)
c1_c2_recorded <- wq_2025 |> filter(pond %in% c("C1", "C2"), lot == "ESS2025") |> summarise(sum(morts, na.rm = TRUE)) |> pull()
c1_c2_first <- wq_2025 |> filter(pond %in% c("C1", "C2"), morts >= 20) |> group_by(pond) |> summarise(first = min(date)) |> arrange(first)

# E34: pond C6 in July 2025. The report mentions "11 dead fish" on one morning with dissolved oxygen below 2 mg/L; the daily log shows a
# longer run: from the first July day with 10 or more dead fish to the last day the staff note stratification
c6_july <- wq_2025 |> filter(pond == "C6", date >= "2025-07-01", date <= "2025-07-31")
c6_window <- c(start = min(c6_july$date[c6_july$morts >= 10], na.rm = TRUE),
               end = max(c6_july$date[str_detect(c6_july$comments, regex("strat", ignore_case = TRUE))], na.rm = TRUE))
c6_event <- c6_july |> filter(date >= c6_window[["start"]], date <= c6_window[["end"]])
c6_lows <- c6_event$comments |> str_extract_all("(?i)(?:low|DO|strat\\w*)[^0-9]{0,12}(\\d?\\.\\d+)") |> unlist() |> str_extract("\\d?\\.\\d+$") |> as.numeric()
c6_start <- production_harvest_input |> filter(pond == "C6", season == "Fall 2025") |> pull(start_number)

# E33: pond P10 winter 2025 cycle (Production Harvest workbook, sheet "Spring 2025")
p10 <- production_harvest_input |> filter(pond == "P10", season == "Spring 2025")

# E35, E36: early rearing totals (FY2025 report Table 8, total row)
rearing_total_2025 <- tables_raw_2025[[9]] |> clean_names() |> filter(is.na(date))
# first and last collection date of the FY2025 larval tanks (Table 8; two-day entries use the first date)
rearing_dates_2025 <- tables_raw_2025[[9]] |> clean_names() |> drop_na(date) |>
  transmute(date = mdy(str_extract(date, "^\\d{1,2}/\\d{1,2}/\\d{4}"))) |>
  summarise(first = min(date), last = max(date))
fmt_day <- function(d) format(d, "%d %b %Y")

from_text <- function(pattern) str_match(report_2025_text, pattern)
num_text <- function(x) as.numeric(str_remove_all(x, ","))
otter <- from_text("approximately (\\d+) fish were lost, which was a ([\\d.]+)% loss")
merganser <- from_text("nearly ([\\d,]+) fingerlings from the 2024 lots which succumbed to the merganser")
dieoff <- from_text("(\\d+) dead fish, all with wounds to the caudal peduncle")
dieoff_2 <- from_text("dropped the Page \\d+ of \\d+ following day to (\\d+)|following day to (\\d+)")

severity_tier_flag <- function(percent_lost, mortality_event) {
  case_when(
    !mortality_event & !is.na(percent_lost) & percent_lost == 0 ~ "Near-miss (no mortality observed)",
    !mortality_event ~ "Non-mortality (escape/collection/reproductive)",
    is.na(percent_lost) ~ "Qualitative only (unquantified)",
    percent_lost > 70 ~ "Catastrophic (>70% loss)",
    percent_lost >= 30 ~ "Major (30-70% loss)",
    percent_lost >= 10 ~ "Moderate (10-30% loss)",
    TRUE ~ "Minor (<10% loss)"
  )
}
half_up <- function(x, digits = 0) floor(x * 10^digits + 0.5) / 10^digits

events_2025 <- tribble(
  ~event_id, ~year, ~period, ~location, ~category, ~cause_description, ~population_affected, ~loss_as_reported, ~percent_lost, ~n_mortality, ~confidence, ~loss_source, ~anchor,
  "E29", 2025, sprintf("From %s (%s) and %s (%s); harvested early on %s (%s) and %s (%s)",
                         fmt_day(c1_c2_first$first[1]), c1_c2_first$pond[1], fmt_day(c1_c2_first$first[2]), c1_c2_first$pond[2],
                         fmt_day(harvest_2025$harvest_date[harvest_2025$pond == "C1"]), "C1", fmt_day(harvest_2025$harvest_date[harvest_2025$pond == "C2"]), "C2"),
    "Tier 1 ponds C1 and C2 (ESS2025 lot)", "other",
    "Unknown. Fish Health sampled in Jul, Aug and Sep and found no causative agent; the report suggests the fish stopped eating a new diet (palatability). Stratified ponds with low morning dissolved oxygen, no-feed and half-ration days are logged in the same weeks",
    sprintf("ESS2025 lot in C1 and C2: %s fish at the start (C1 %s, C2 %s)", format(sum(c1_c2$initial), big.mark = ","),
            format(c1_c2$initial[c1_c2$pond == "C1"], big.mark = ","), format(c1_c2$initial[c1_c2$pond == "C2"], big.mark = ",")),
    sprintf("%s harvested + %s harvest-day deaths = %.1f%% survival to harvest (Sheet1); the daily logs record %s dead fish in the two ponds (unretrievable fish are not counted)",
            format(sum(c1_c2$harvested), big.mark = ","), format(sum(c1_c2$harvest_morts), big.mark = ","),
            100 * sum(c1_c2$harvested + c1_c2$harvest_morts) / sum(c1_c2$initial), format(c1_c2_recorded, big.mark = ",")),
    half_up(100 * c1_c2_lost / sum(c1_c2$initial), 1), c1_c2_lost, "Quantified",
    "KFNFH_WQ_FY2025_2 (version 1).xlsx, Sheet1 (initial, harvested, harvest morts) and sheets JulytoSept_WQ (Morts)",
    "significant mortality we started observing at the end of July, through August, and into September",
  "E30", 2025, "September 2025, day of and day after the early harvest", "Greenhouse tanks holding the harvested ESS2025 fish", "other",
    "Abnormal mortality one day after harvest: dead fish all had wounds to the caudal peduncle; backswimmers (insects) in the tanks, fish already weakened",
    "ESS2025 fish moved from the Tier 1 ponds into greenhouse tanks (number in the tanks not stated)",
    sprintf("%s dead fish found the first morning, %s the following day", dieoff[1, 2], dieoff_2[1, 2]),
    NA, num_text(dieoff[1, 2]) + num_text(dieoff_2[1, 2]), "Qualitative only (number of dead fish given, number at risk not)",
    "FY25 KFNFH Annual Report.pdf (text)", "dead fish, all with wounds to the caudal peduncle",
  "E31", 2024, "Mid-Oct 2024-early Jan 2025", "P-series ponds, broodstock lots (moved to ponds C5 and B3 in early January)", "predation",
    "River otters at the P-ponds (tracks and slides in the snow); trapping and an electric fence did not stop them",
    "Broodstock held in the P-series ponds",
    sprintf("About %s fish lost, a %s%% loss, 'significantly higher than normal mortality ... in an entire year'; pond P2 had 74%% survival", otter[1, 2], otter[1, 3]),
    as.numeric(otter[1, 3]), as.numeric(otter[1, 2]), "Quantified", "FY25 KFNFH Annual Report.pdf (text)",
    "approximately 164 fish were lost, which was a 9.6% loss",
  "E32", 2025, "Late February 2025 (a two-day window)", "Tier 1 ponds (no bird netting yet): fingerling and broodstock ponds", "predation",
    "Many mergansers on the unnetted Tier 1 ponds while the pond ice had melted and other waters were still frozen; delayed mortalities continued for weeks",
    "Fingerlings of the 2024 lots and broodstock",
    sprintf("Inventory losses of nearly %s fingerlings from the 2024 lots; broodstock survival was 97%% between March and April (23 fish missing)", merganser[1, 2]),
    NA, num_text(merganser[1, 2]), "Qualitative only (number of fish lost given, number at risk not)",
    "FY25 KFNFH Annual Report.pdf (text)", "nearly 23,655 fingerlings from the 2024 lots which succumbed to the merganser predation",
  "E33", 2025, sprintf("%s to %s (pond cycle)", fmt_day(p10$stocked_date), fmt_day(p10$harvest_date)), "P-series ponds, especially pond P10", "disease",
    "Cold-water bacterial infections, fin rot and fungal infections on moribund fish; Fish Health sampled, nothing determinate was identified; otters may have been vectors",
    sprintf("Lot ESS2023 in pond P10: %s fish at the start of the cycle", format(p10$start_number, big.mark = ",")),
    sprintf("Survival just under 70%% (workbook: %.1f%%)", p10$survival_percent),
    half_up(100 - p10$survival_percent, 1), p10$start_number - p10$end_number, "Quantified",
    "FY2025 Production Harvest Data Sheet_20251107.xlsx, sheet Spring 2025 (pond P10)",
    "cases of cold water bacterial infections and fin rot were observed in several P-ponds, especially pond P10",
  "E34", 2025, sprintf("%s to %s July 2025", format(c6_window[["start"]], "%d"), format(c6_window[["end"]], "%d")), "Pond C6 (Tier 1, SNS2025 lot)", "water_quality",
    sprintf("Dissolved oxygen stratification in the warm months; the staff log bottom dissolved oxygen of %.1f-%.1f mg/L on several days", min(c6_lows, na.rm = TRUE), max(c6_lows, na.rm = TRUE)),
    sprintf("SNS2025 lot in pond C6: %s fish at the start", format(c6_start, big.mark = ",")),
    sprintf("%s dead fish recorded in the daily log over %d days (more were logged as unretrievable). The report only describes 'a minor acute mortality event' with 11 dead fish on one morning with DO below 2 mg/L; the pond had good survival for the season",
            format(sum(c6_event$morts, na.rm = TRUE), big.mark = ","), as.integer(c6_window[["end"]] - c6_window[["start"]]) + 1L),
    half_up(100 * sum(c6_event$morts, na.rm = TRUE) / c6_start, 1), sum(c6_event$morts, na.rm = TRUE), "Quantified (recorded deaths only)",
    "KFNFH_WQ_FY2025_2 (version 1).xlsx, sheet JulytoSept_WQ (Morts, Comments) and FY2025 Production Harvest Data Sheet_20251107.xlsx, sheet Fall 2025 (start number)",
    "11 dead fish were collected from pond C6",
  "E35", 2025, sprintf("%s to %s (larval collections into the tanks)", fmt_day(rearing_dates_2025$first), fmt_day(rearing_dates_2025$last)), "Salvage Building larval rearing tanks", "disease",
    "Costia infections during intensive tank rearing (a known issue when fry stay in tanks longer) and an unexplained loss of fry 45-60 days after collection",
    sprintf("Wild larval LRS/SNS from the Williamson River (%s collected)", format(num_text(rearing_total_2025$collected), big.mark = ",")),
    sprintf("%s observed fry deaths = %.1f%% of the stock (the report text says 3,652; Table 8 and its total row say %s)", format(num_text(rearing_total_2025$number_mortality), big.mark = ","),
            100 * num_text(rearing_total_2025$number_mortality) / num_text(rearing_total_2025$collected), format(num_text(rearing_total_2025$number_mortality), big.mark = ",")),
    half_up(100 * num_text(rearing_total_2025$number_mortality) / num_text(rearing_total_2025$collected), 1), num_text(rearing_total_2025$number_mortality), "Quantified (observed mortality only - see E36 for the unobserved loss)",
    "FY25 KFNFH Annual Report.pdf, Table 8 (total row)", "approximately 5.1% of the entire stock",
  "E36", 2025, sprintf("%s to %s (larval collections into the tanks)", fmt_day(rearing_dates_2025$first), fmt_day(rearing_dates_2025$last)), "Salvage Building tanks", "other",
    "Tank overflow events that let fry through the 400 um screens at night; bubble curtains were installed in FY2025 to reduce them",
    "Wild-caught larval suckers held in tanks (same stock as E35)",
    sprintf("Unobserved mortality = %.1f%% of the stock, 'much lower than in years past'", num_text(rearing_total_2025$unobserved_mortality_percent)),
    num_text(rearing_total_2025$unobserved_mortality_percent), NA, "Quantified", "FY25 KFNFH Annual Report.pdf, Table 8 (total row)",
    "unobserved mortality, primarily from several noted tank overflow events, at approximately 0.9% of the entire stock",
  "E37", 2025, "Spring 2025 (weeks after hatch)", "Captive broodstock LRS fry in tanks", "other",
    "Captively produced LRS fry were smaller than wild fry; the Artemia nauplii were too large and some fish starved; after switching to Otohime A1 the SNS fry grew but the LRS fry did not survive",
    "Fry from the captive LRS spawn (Table 6)",
    "'Nearly total losses of the LRS fry within several weeks post hatch'; no number is given",
    NA, NA, "Qualitative only", "FY25 KFNFH Annual Report.pdf (text)", "nearly total losses of the LRS fry within several weeks post hatch"
) |>
  mutate(mortality_event = TRUE,
         severity_tier = severity_tier_flag(percent_lost, mortality_event),
         report_year = 2025,
         hatchery_name = "KFNFH")

# every FY2025 event must be described by the report phrase it cites
stopifnot("every FY2025 event phrase must be in the FY2025 report" =
            all(str_detect(report_2025_text, fixed(events_2025$anchor))))

# the report (fiscal year), the phrase in it that describes each event, and where the loss comes from
event_citations <- event_anchors |>
  transmute(event_id, report_year = report, report_phrase = str_remove_all(anchor, "\\\\")) |>
  left_join(event_losses |> select(event_id, loss_source = source), by = "event_id")

KFNFH_mortality_events <- bind_rows(
  KFNFH_mortality_events |> left_join(event_citations, by = "event_id"),
  events_2025 |> mutate(report_phrase = str_remove_all(anchor, "\\\\")) |>
    select(any_of(c(names(KFNFH_mortality_events), "report_year", "report_phrase", "loss_source")))
)

KFNFH_mortality_events_summary <- bind_rows(
  KFNFH_mortality_events_summary,
  events_2025 |> transmute(hatchery_name, event_id, year, category, mortality_event,
                           percent_mortality = percent_lost, n_mortality)
)

stopifnot(all(KFNFH_mortality_events$event_id == KFNFH_mortality_events_summary$event_id))


### Monthly pond mortality, capacity and broodstock =============================

KFNFH_mortality_pond_monthly <- mortality_spreadsheets_input |>
  mutate(hatchery_name = "KFNFH") |>
  add_age("pond") |>
  relocate(hatchery_name, .before = fiscal_year) |>
  rename(record_type = type)


KFNFH_capacity <- usfws_sarp_capacity_input |>
  mutate(hatchery_name = "KFNFH") |>
  relocate(hatchery_name, .before = fiscal_year)


KFNFH_broodstock_spawned <- broodstock_spawned_input |>
  transmute(hatchery_name = "KFNFH",
            spawn_fiscal_year = 2025,
            species,
            capture_year,
            capture_year_raw,
            females_spawned, males_spawned, total_spawned)


### Save ========================================================================

final_objects <- c(
  # merged or extended (existing names kept)
  "KFNFH_historical_collection_release",
  "KFNFH_pond_growout",
  "KFNFH_LRS_ESS_adult_collection",
  "KFNFH_hatchery_release",
  "KFNFH_pen_survival",
  # extended and renamed (the _2024 suffix is dropped)
  "KFNFH_adfluvial_early_rearing",
  "KFNFH_LRS_ESS_incubation_hatch",
  # new
  "KFNFH_age_facility_lookup",
  "KFNFH_mortality_events",
  "KFNFH_mortality_events_summary",
  "KFNFH_mortality_pond_monthly",
  "KFNFH_capacity",
  "KFNFH_broodstock_spawned")

do.call(usethis::use_data, c(lapply(final_objects, as.name), list(overwrite = TRUE)))

# objects that were renamed: remove the old files so both names do not ship
stale <- here("data", c("KFNFH_adfluvial_early_rearing_2024.rda", "KFNFH_LRS_ESS_incubation_hatch_2024.rda"))
file.remove(stale[file.exists(stale)])

# sucker_pit_tag_detection_2024 is saved in its own section above (unchanged)
