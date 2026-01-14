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
  tables_raw_2024 <- extract_tables(pdf_path, method = "stream", output = "tibble")
  tables_raw_2024 |> saveRDS(filename)
} else {
  tables_raw_2024 <- readRDS(filename)
}


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
KFNFH_annual_summary <- table_1 |>
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
KFNFH_annual_summary |> usethis::use_data(overwrite = T)
