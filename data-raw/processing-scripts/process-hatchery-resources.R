# process-hatchery-resources.R ------------------------------------------------
#
# Rebuilds the CSVs in data-raw/hatchery-resources/ that the exploration Rmd
# (data-raw/hatchery_data_exploration.Rmd) reads, directly from the source files:
#
#   1. the hatchery Excel workbooks ("FY20xx Supporting Excel Tables/"), then
#   2. the annual report PDFs (cached extracted tables in data-raw/pdf-tables/, and
#      the report text for narrative values).
#
# Nothing here overwrites the existing CSVs. Every rebuilt file is written to
# data-raw/hatchery-resources/reproduced/ and compared with the existing CSV at the
# end (see "Reproduction report"). Once the comparison has been reviewed, this script
# can be appended to process-hatchery-data.R (set `output_dir` to the resources folder
# to replace the existing CSVs).
#
# What can and cannot be reproduced (see the report printed at the end):
#
#   fully reproduced from Excel:  mortality_spreadsheets_combined.csv, production_harvest_combined.csv,
#                                 family_groups.csv, wild_lrs_family_groups.csv, broodstock_spawned.csv
#   fully reproduced from PDFs:   collected_released.csv
#
# Rule: a value is always taken from the reports or the Excel files. A curated CSV is read only for
# text that exists nowhere else (the event descriptions) and for comparison with the rebuilt file.
#   reproduced, one column differs: usfws_sarp_capacity.csv (indoor gallons: the curated file carries the
#                                 FY2025 total back to FY2022-24; the reports give less in earlier years)
#   partly reproduced:            usfws_sarp_age_facility_lookup.csv (net pens exactly; ponds by a rule that
#                                 gives the same age classes for ~73% of facility-years, a subset for most of
#                                 the rest; the FY2020 ponds, the unoperated FY2025 pens and the extra "other"/"wild" classes of
#                                 multi-use ponds are not in the workbooks or reports)
#   descriptive text curated, numbers rebuilt: mortality_events.csv and mortality_events_summary.csv. Finding
#                                 an event and naming its cause, category and affected population is a reading
#                                 of the report narrative and stays a curated input. Each event is tied to a
#                                 phrase in the report, its loss is recomputed from the net pen records, the
#                                 larval rearing tables and the report text, and its severity tier from a fixed
#                                 rule. A keyword search of the reports (candidates for review, recall) is a
#                                 start at finding events automatically but only flags about 40% of them.
#
# Run from the project root (uses here::here()).

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(here)

## SETTINGS ####################################################################

resources_dir <- here("data-raw", "hatchery-resources")
output_dir    <- file.path(resources_dir, "reproduced")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Values always come from the reports and Excel files, not from the curated CSVs. The curated
# lookup CSV is only used to compare against the rebuilt one, unless this is set to TRUE
# (to check the other columns of the mortality and production harvest files on their own).
use_curated_lookup <- FALSE

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

## 0. AGE / FACILITY LOOKUP (CURATED INPUT, REBUILT IN PART BELOW) ##############

lookup_curated <- read_csv(file.path(resources_dir, "usfws_sarp_age_facility_lookup.csv"),
                           show_col_types = FALSE) |>
  mutate(facility = str_to_upper(facility))

## 1. FROM THE EXCEL WORKBOOKS #################################################

### 1.1 Mortality spreadsheets (-> mortality_spreadsheets_combined.csv) ========

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

### 1.2 Production harvest (-> production_harvest_combined.csv) ================
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

### 1.3 Family groups (-> family_groups.csv, wild_lrs_family_groups.csv) =======
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
tables_2025 <- load_pdf_tables(2025)
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

family_groups_rebuilt <- bind_rows(
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
wild_lrs_family_groups_rebuilt <- family_groups_rebuilt |>
  rename(fiscal_year = `Fiscal Year`, program = Program, species = Species,
         source_table = `Source Table`, spawning_date = `Spawning / Cross Date`,
         female_id = `Female ID`, n_males = `Number of Males in Cross`,
         family_groups = `Family Groups`, incubation_vessel = `Incubation Vessel`,
         egg_volume_ml = `Egg Volume (mL)`, eggs_per_ml = `Eggs per mL`,
         total_eggs = `Total Eggs`, fry_count = `1st Fry Count`,
         hatch_rate_percent = `1st Hatch Rate (%)`, hatch_date = `Hatch Date`) |>
  mutate(spawning_date = mdy(spawning_date), hatch_date = mdy(hatch_date)) |>
  filter(program == "Wild ESS Spawning", species == "LRS", !is.na(spawning_date))

### 1.4 Broodstock spawned (-> broodstock_spawned.csv) =========================
# Number of captive broodstock spawned in spring 2025, by capture year and sex. This summary
# table is in the "All PIT" sheet of the captive broodstock workbook (columns
# "Capture Year (CY)", then SNS F/M and LRS F/M).

brood_raw <- read_text_sheet(captive_path, "All PIT")
cy_col <- which(names(brood_raw) == "Capture Year (CY)")
brood_block <- brood_raw[, cy_col:(cy_col + 4)]
names(brood_block) <- c("capture_year_raw", "SNS_F", "SNS_M", "LRS_F", "LRS_M")
brood_block <- brood_block[-1, ] |>                                   # first row is the F/M header
  filter(!is.na(capture_year_raw))

broodstock_spawned_rebuilt <- brood_block |>
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

### 1.5 Age / facility lookup (partly rebuilt) =================================
# Rule: one row per facility and fiscal year, `age` = the age class(es) of the fish held there.
#  * Net pens (nphistory.xlsx): age = operation year - capture year, starting_count = fish stocked.
#  * Ponds: every pond cycle (Production Harvest workbooks first, then the annual report tables for
#    cycles that are not in the workbooks) counts in the fiscal year it was stocked and in the fiscal year it
#    was harvested. The age class is that fiscal year minus the lot's collection year (4 = 4 or older);
#    East Side Springs and mixed lots are "other" and wild or salvage lots are "wild". starting_count is the
#    number of fish at the start of the first cycle harvested in the year.
#    Tested against the curated lookup this gives the same age classes for ~70% of facility-years and a
#    subset of the curated classes for most of the rest. The curated lookup lists extra classes for ponds
#    that held more than one group in a year, and some age conventions, from information provided by the
#    hatchery that is not in the workbooks or the reports.

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

## 2. FROM THE PDF REPORTS #####################################################

### 2.1 Collections and releases (-> collected_released.csv) ===================
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

collected_released_rebuilt <- table_1 |>
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

### 2.2 Capacity (-> usfws_sarp_capacity.csv) ==================================
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

usfws_sarp_capacity_rebuilt <- capacity_inventory |>
  mutate(n_outdoor_ponds = coalesce(n_ponds_override, `ponds_0.03` + `ponds_0.125` + `ponds_0.25`),
         acreage_outdoor_ponds = coalesce(acres_override,
                                          round(`ponds_0.03` * 0.03 + `ponds_0.125` * 0.125 + `ponds_0.25` * 0.25, 2)),
         gallons_indoor = map_dbl(report, indoor_gallons),
         gallons_indoor = if_else(capacity_type == "current", gallons_indoor, NA_real_)) |>
  select(fiscal_year, n_outdoor_ponds, acreage_outdoor_ponds, gallons_indoor, capacity_type)

## 3. MORTALITY EVENTS (-> mortality_events.csv, mortality_events_summary.csv) ###
# mortality_events.csv lists the disease, water quality, weather, predation and other events described in
# the narrative sections of the FY2021-2024 reports. Identifying an event, naming its category and cause,
# and describing the affected population is a reading of the text and cannot be rebuilt from a table, so
# those descriptive columns are read from the curated CSV. Everything else is rebuilt and checked:
#
#   3.1 every event is tied to a phrase in the report that describes it (the script stops if a phrase is
#       not found, so a mis-cited or edited event cannot go unnoticed);
#   3.2 the loss (percent and number of fish) is recomputed from the net pen records (nphistory.xlsx),
#       the larval rearing tables (workbook and report table) and numbers in the report text;
#   3.3 the severity tier is recomputed from the loss with a fixed rule;
#   3.4 a keyword search of the report text lists mortality passages that are not yet in the event list
#       (candidates for review) and measures how many of the events the search would have found.

mortality_events_curated <- read_csv(file.path(resources_dir, "mortality_events.csv"), show_col_types = FALSE)
names(mortality_events_curated) <- str_remove(names(mortality_events_curated), "^﻿")
mortality_events_summary_curated <- read_csv(file.path(resources_dir, "mortality_events_summary.csv"),
                                             show_col_types = FALSE)

report_texts <- map(2021:2025, report_text) |> set_names(2021:2025)

### 3.1 Phrase in the report that describes each event ---------------------------
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
          "every curated event needs an anchor" = setequal(mortality_events_curated$`Event ID`, event_anchors$event_id))

### 3.2 Loss from the data tables and the report text ----------------------------

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
  "E07", NA, NA, 0, "report text: emergency harvest, no loss",
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

### 3.3 Severity tier from the loss ---------------------------------------------
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

mortality_events_rebuilt <- mortality_events_curated |>
  left_join(event_losses |> select(event_id, percent_lost, n_mortality), by = c(`Event ID` = "event_id")) |>
  mutate(`% Lost (best estimate, numeric)` = if_else(is.na(percent_lost), NA_character_, sprintf("%.1f%%", percent_lost)),
         `Severity Tier` = severity_tier(percent_lost, `Mortality Event? (Y/N)`)) |>
  select(-percent_lost, -n_mortality)

mortality_events_summary_rebuilt <- mortality_events_rebuilt |>
  left_join(event_losses |> select(event_id, percent_lost, n_mortality), by = c(`Event ID` = "event_id")) |>
  transmute(id = `Event ID`,
            year = `Actual Year`,
            category = `Event Category`,
            mortality_event = `Mortality Event? (Y/N)`,
            percent_mortality = if_else(is.na(percent_lost), NA_character_, sprintf("%.2f%%", percent_lost)),
            n_mortality)

### 3.4 Keyword search of the reports (candidates and recall) ---------------------
# A sentence is a candidate event when it contains a mortality word AND a cause or setting word.
# The category is guessed from the cause words (the first group that matches).

mortality_words <- "mortalit|died|dead|die-off|perish|losses|lost|crash|killed|escap|survival"
category_words <- c(
  disease       = "Costia|columnaris|bacterial|infection|parasit|tetany|necrosis|hyperplasia|fish health|pathogen|fin rot",
  water_quality = "ammonia|dissolved oxygen|\\bDO\\b|anoxi|hypoxi|oxygen|algae|bloom|water quality|temperature",
  weather       = "wind|storm|drought|low water|low river|flood|\\bice\\b|snow|cold",
  predation     = "predator|predation|heron|pelican|otter|\\bbirds?\\b",
  equipment     = "\\bholes?\\b|overflow|malfunction|pump|power outage|equipment|escaped"
)
any_category <- paste(category_words, collapse = "|")

split_sentences <- function(txt) str_split(txt, "(?<=[a-z0-9\\)%]\\.)\\s+(?=[A-Z])")[[1]]

candidate_sentences <- imap(report_texts[as.character(2021:2024)], function(txt, year) {
  tibble(report = as.numeric(year), sentence = split_sentences(txt)) |>
    mutate(sentence_id = row_number(),
           mortality_term = str_detect(sentence, regex(mortality_words, ignore_case = TRUE)),
           cause_term = str_detect(sentence, regex(any_category, ignore_case = TRUE)),
           candidate = mortality_term & cause_term,
           category_guess = map_chr(sentence, function(x) {
             hit <- names(category_words)[map_lgl(category_words, ~ str_detect(x, regex(.x, ignore_case = TRUE)))]
             if (length(hit)) hit[1] else NA_character_
           }),
           years_mentioned = str_extract_all(sentence, "\\b20(1[7-9]|2[0-5])\\b") |> map_chr(~ paste(unique(.x), collapse = ", ")))
}) |> list_rbind()

# does the search flag the sentence that describes each event?
event_recall <- event_anchors |>
  mutate(sentence_flagged = map2_lgl(report, anchor, function(r, a) {
    hits <- candidate_sentences |> filter(report == r, str_detect(sentence, a))
    nrow(hits) > 0 && any(hits$candidate)
  }),
  category_agrees = map2_lgl(report, anchor, function(r, a) {
    hit <- candidate_sentences |> filter(report == r, str_detect(sentence, a))
    cat_curated <- mortality_events_curated$`Event Category`[match(event_id[1], mortality_events_curated$`Event ID`)]
    nrow(hit) > 0 && !is.na(hit$category_guess[1]) && hit$category_guess[1] == cat_curated
  }))
# (category_agrees is evaluated per row below, because the closure above only sees one event id at a time)
event_recall$category_agrees <- pmap_lgl(event_recall, function(event_id, report, anchor, ...) {
  hit <- candidate_sentences |> filter(report == !!report, str_detect(sentence, anchor))
  cat_curated <- mortality_events_curated$`Event Category`[mortality_events_curated$`Event ID` == event_id]
  nrow(hit) > 0 && !is.na(hit$category_guess[1]) && hit$category_guess[1] == cat_curated
})

# candidate sentences that are not an event sentence, for review
anchor_sentences <- event_anchors |>
  pmap(function(event_id, report, anchor) {
    candidate_sentences |> filter(report == !!report, str_detect(sentence, anchor)) |>
      transmute(report, sentence_id, event_id = !!event_id)
  }) |> list_rbind()

candidates_for_review <- candidate_sentences |>
  filter(candidate) |>
  anti_join(anchor_sentences, by = c("report", "sentence_id")) |>
  select(report, sentence_id, category_guess, years_mentioned, sentence)

write_csv(candidates_for_review, file.path(output_dir, "mortality_event_candidates_for_review.csv"))
write_csv(event_recall |> left_join(event_losses |> select(event_id, source), by = "event_id"),
          file.path(output_dir, "mortality_event_traceability.csv"))

## 4. JOIN THE LOOKUP AND WRITE THE FILES ######################################

# FY2021 pond cycles come from the report tables built in process-hatchery-data.R. When this script is
# appended to that file the objects already exist; when it is run on its own, build them here without
# saving anything.
if (!exists("KFNFH_pond_growout")) {
  base_lines <- readLines(here("data-raw", "processing-scripts", "process-hatchery-data.R"))
  base_lines <- sub("usethis::use_data\\(overwrite = T\\)", "invisible()", base_lines)
  base_env <- new.env(parent = globalenv())
  invisible(capture.output(suppressMessages(suppressWarnings(eval(parse(text = base_lines), envir = base_env)))))
  KFNFH_pond_growout <- base_env$KFNFH_pond_growout
}

lookup_rebuilt <- bind_rows(
  net_pen_lookup() |> mutate(source = "nphistory.xlsx"),
  pond_cycle_ages(bind_rows(
    production_harvest_raw |> select(pond, lot, stocked_date, harvest_date, start_number),
    # the annual report tables have cycles (FY2021, and stock dates the workbooks lack) that are not in the workbooks
    if (exists("KFNFH_pond_growout")) {
      KFNFH_pond_growout |>
        select(pond, lot, stocked_date = pond_stock_date, harvest_date, start_number)
    }) |>
      distinct(pond, stocked_date, harvest_date, .keep_all = TRUE)) |>
    mutate(source = "pond cycles")
) |>
  arrange(fiscal_year, facility)

lookup_for_join <- if (use_curated_lookup) lookup_curated else lookup_rebuilt

mortality_spreadsheets_rebuilt <- bind_rows(mortality_fy2023, mortality_fy2024, mortality_fy2025, harvest_fy2023) |>
  left_join(lookup_for_join |> select(fiscal_year, facility, age, starting_count),
            by = c("fiscal_year", "pond" = "facility"))

production_harvest_rebuilt <- production_harvest_raw |>
  left_join(lookup_for_join |> select(fiscal_year, facility, age, starting_count),
            by = c("fiscal_year", "pond" = "facility"))

outputs <- list(
  mortality_spreadsheets_combined = mortality_spreadsheets_rebuilt,
  production_harvest_combined     = production_harvest_rebuilt,
  family_groups                   = family_groups_rebuilt,
  wild_lrs_family_groups          = wild_lrs_family_groups_rebuilt,
  broodstock_spawned              = broodstock_spawned_rebuilt,
  collected_released              = collected_released_rebuilt,
  usfws_sarp_capacity             = usfws_sarp_capacity_rebuilt,
  usfws_sarp_age_facility_lookup  = lookup_rebuilt |> select(fiscal_year, facility, age, starting_count),
  mortality_events                = mortality_events_rebuilt,
  mortality_events_summary        = mortality_events_summary_rebuilt
)

iwalk(outputs, ~ write_csv(.x, file.path(output_dir, paste0(.y, ".csv"))))

## 5. REPRODUCTION REPORT ######################################################
# Compares every rebuilt CSV with the existing one: shape, columns, and the share of cells
# that are equal (numbers within a rounding tolerance).

compare_csv <- function(name, rebuilt, keys = NULL, tolerance = 0.051) {
  existing <- read_csv(file.path(resources_dir, paste0(name, ".csv")), show_col_types = FALSE,
                       col_types = cols(.default = "c"))
  names(existing) <- str_remove(names(existing), "^\ufeff")
  rebuilt <- rebuilt |> mutate(across(everything(), ~ as.character(.x)))
  shared <- intersect(names(existing), names(rebuilt))

  if (is.null(keys)) {                       # no key: compare row by row
    existing$.row <- seq_len(nrow(existing)); rebuilt$.row <- seq_len(nrow(rebuilt)); keys <- ".row"
  }
  joined <- full_join(existing, rebuilt, by = keys, suffix = c(".old", ".new"))
  cell_equal <- function(a, b) {
    na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
    numeric_ok <- !is.na(na) & !is.na(nb) & abs(na - nb) <= tolerance
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b) | numeric_ok
  }
  value_cols <- setdiff(shared, keys)
  by_col <- map_dbl(value_cols, ~ mean(cell_equal(joined[[paste0(.x, ".old")]], joined[[paste0(.x, ".new")]])))
  tibble(file = paste0(name, ".csv"),
         rows_existing = nrow(existing), rows_rebuilt = nrow(rebuilt),
         cols_missing_in_rebuilt = paste(setdiff(names(existing), c(names(rebuilt), ".row")), collapse = ", "),
         cols_extra_in_rebuilt = paste(setdiff(names(rebuilt), c(names(existing), ".row")), collapse = ", "),
         cells_equal_pct = round(100 * mean(by_col), 1),
         worst_column = if (length(by_col)) paste0(value_cols[which.min(by_col)], " (", round(100 * min(by_col), 1), "%)") else NA_character_)
}

comparison_keys <- list(usfws_sarp_age_facility_lookup = c("fiscal_year", "facility"),
                        usfws_sarp_capacity = "fiscal_year",
                        mortality_events = "Event ID",
                        mortality_events_summary = "id")

reproduction_report <- imap(outputs, ~ compare_csv(.y, .x, keys = comparison_keys[[.y]])) |> list_rbind()
reproduction_report |>
  mutate(not_reproducible = case_when(
    file == "usfws_sarp_age_facility_lookup.csv" ~ "FY2020 ponds, unoperated FY2025 pens, extra age classes, some counts (curated)",
    file == "usfws_sarp_capacity.csv" ~ "indoor gallons FY2022-24 and planned (curated)",
    file %in% c("mortality_events.csv", "mortality_events_summary.csv") ~ "event descriptions (curated); numbers and tiers rebuilt",
    TRUE ~ "")) |>
  write_csv(file.path(output_dir, "reproduction_report.csv"))

print(reproduction_report, width = Inf)

## 6. LOOKUP: WHAT MATCHES AND WHAT DOES NOT ####################################
# equal           same age classes (and the rebuilt value is the whole answer)
# derived subset  the rebuilt classes are all in the curated value, which lists extra classes
#                 ("other", "wild", a second age) that are not in the workbooks or the reports
# different       the rebuilt and curated classes disagree (mostly the age convention for the
#                 fiscal year of a cycle that spans two fiscal years)
# not derived     curated rows with no cycle or net pen record (FY2020 ponds, unoperated FY2025 pens)

split_age <- function(x) if (is.na(x)) character() else str_split(x, "/")[[1]]

lookup_comparison <- full_join(lookup_curated, lookup_rebuilt,
                               by = c("fiscal_year", "facility"), suffix = c("_curated", "_rebuilt")) |>
  mutate(series = case_when(str_detect(facility, "^(UKL|GER)") ~ "net pen",
                            TRUE ~ str_sub(facility, 1, 1)),
         status = case_when(
           is.na(age_rebuilt) ~ "not derived",
           is.na(age_curated) ~ "derived, not in curated lookup",
           age_curated == age_rebuilt ~ "equal",
           map2_lgl(age_rebuilt, age_curated, ~ all(split_age(.x) %in% split_age(.y))) ~ "derived subset",
           TRUE ~ "different"),
         starting_count_equal = starting_count_curated == starting_count_rebuilt)

write_csv(lookup_comparison, file.path(output_dir, "lookup_comparison.csv"))

lookup_comparison |>
  count(series, status) |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  print()
