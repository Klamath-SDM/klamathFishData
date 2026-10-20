# Model inputs: annual colonial waterbird colony counts (annual.birds), 1995-2024
#
# Sources (data-raw/helper-data/model-inputs):
#   - Evans et al. 2022, Supplemental Table S.1 (2009-2020; 2009-2014 credited to Evans et al. 2016)
#   - Banet et al. 2025, Table A1, Appendix A, page 24 (2021-2024)
# CLR = Clear Lake Reservoir, UKL = Upper Klamath Lake,
# AWPE = American White Pelican, DCCO = Double-crested Cormorant, CATE = Caspian Tern.
#
# Gaps (left NA, no values invented): 1995-2008 (no source found), 2016-2017 (dash in S.1),
# UKL 2021 (NA in A1). The original script filled 1995-2008 with unseeded rgamma() draws.
# Used UKL Bird counts and predation rates (corrected) Clear Lake as a reference (up to line 167)
# TODO once we clarify with Jim if we want random pick from mean and standard deviation into gamma parameters for earlier
# years, we can fill out 1995-2008

# TODO - decide if we want this to be a data objet itself in the package - if so we can move forward with documentation

library(tidyverse)
library(xml2)
library(pdftools)

input_dir <- "data-raw/helper-data/model-inputs"

species_lookup <- c(Pelicans = "AWPE", Cormorants = "DCCO", Terns = "CATE")
location_lookup <- c("Upper Klamath Lake" = "UKL", "Clear Lake Reservoir" = "CLR",
                     "Sheep Lake" = "Sheepy Lake", "Tule Lake" = "Tule Lake")

# Evans et al. 2022, Table S.1 (docx) -------------------------------------------
docx_dir <- tempfile()
unzip(file.path(input_dir, "Evans et al. 2022 Supplemental Tables.docx"),
      "word/document.xml", exdir = docx_dir)
docx <- read_xml(file.path(docx_dir, "word/document.xml"))
xml_ns_strip(docx)

s1_rows <- xml_find_first(docx, "//w:tbl") |>
  xml_find_all("./w:tr") |>
  map(\(row) xml_find_all(row, "./w:tc") |>
        map_chr(\(cell) str_squish(paste(xml_text(xml_find_all(cell, ".//w:t")), collapse = ""))))

# `*` = abandoned; dash/NC/blank = no count available -> NA
s1 <- s1_rows[-1] |>
  map(\(row) set_names(row, c("location", "species", 2009:2020))) |>
  map_dfr(as_tibble_row) |>
  mutate(location = na_if(location, "")) |>
  fill(location) |>
  mutate(location = str_remove(location, "[[:space:]]+[a-c]$")) |>
  pivot_longer(`2009`:`2020`, names_to = "year", values_to = "raw") |>
  mutate(year = as.integer(year),
         abandoned_flag = str_detect(raw, fixed("*")),
         count = suppressWarnings(as.integer(str_remove_all(raw, "[*,]"))),
         count_unit = "individuals",
         source_report = "Evans et al. 2022 (Supplemental)",
         source_table = "Table S.1",
         note = case_when(
           raw == "NC" ~ "NC: no colony site existed that year",
           raw %in% c("–", "-") ~ "dash: breeding birds present but counts not available",
           raw == "" ~ "blank: counts not available",
           abandoned_flag ~ "asterisk: birds attempted to nest but abandoned site"),
         note = if_else(year <= 2014 & !is.na(count),
                        coalesce(str_c(note, "counts credited to Evans et al. 2016", sep = "; "),
                                 "counts credited to Evans et al. 2016"),
                        note))

# Banet et al. 2025, Table A1 (pdf page 24) --------------------------------------
# Footnotes: 3 = NA, colony active but unsuccessful; 4 = individuals counted from OSU photos;
# 5 = breeding pairs (OSU)
a1 <- pdf_text(file.path(input_dir, "Banet_et_al_2025.pdf"))[24] |>
  read_lines() |>
  keep(\(line) str_detect(line, "(Pelicans|Cormorants|Terns)[[:space:]]{2,}[^[:space:]]")) |>
  tibble(line = _) |>
  mutate(species = str_extract(line, "Pelicans|Cormorants|Terns"),
         location = c("UKL", "CLR", "Sheepy Lake", "Tule Lake")[cumsum(species == "Pelicans")],
         cells = line |>
           str_remove("^.*?(Pelicans|Cormorants|Terns)[[:space:]]+") |>
           str_trim() |>
           str_split("[[:space:]]{2,}") |>
           map(\(x) set_names(x, 2021:2024))) |>
  select(location, species, cells) |>
  unnest_longer(cells, values_to = "raw", indices_to = "year") |>
  mutate(year = as.integer(year),
         footnote = str_extract(raw, "(?<=[[:space:]])[0-9]$"),
         value = str_remove(raw, "[[:space:]][0-9]$") |> str_remove_all(","),
         count = suppressWarnings(as.integer(value)),
         abandoned_flag = FALSE,
         count_unit = case_when(footnote == "5" ~ "pairs",
                                footnote == "4" ~ "individuals",
                                .default = "unspecified"),
         source_report = "Banet et al. 2025",
         source_table = "Table A1 (Appendix A, p. 24)",
         note = case_when(
           footnote == "3" ~ "NA: birds not at historical sites at June 2021 survey; colony appeared active but unsuccessful",
           footnote == "4" ~ "count of individuals by one Real Time Research observer from photos provided by Oregon State University",
           footnote == "5" ~ "count of breeding pairs (individuals in nesting posture) by Oregon State University",
           value == "NA" ~ "NA in report: colony may have been active, counts not available"))

# Long source table --------------------------------------------------------------
colony_counts_long <- s1 |>
  mutate(location = recode(location, !!!location_lookup)) |>
  bind_rows(a1) |>
  mutate(species = recode(species, !!!species_lookup)) |>
  select(year, location, species, count, count_unit, abandoned_flag,
         source_report, source_table, note) |>
  arrange(year, location, species)

# Annual table, one row per year 1995-2024 (no imputation) ---------------------------
annual_birds <- colony_counts_long |>
  filter(location %in% c("CLR", "UKL"), species %in% c("AWPE", "DCCO")) |>
  unite("series", location, species) |>
  pivot_wider(id_cols = year, names_from = series, values_from = count) |>
  right_join(tibble(year = 1995:2024), by = "year") |>
  arrange(year) |>
  select(Year = year, CLR_AWPE, CLR_DCCO, UKL_AWPE, UKL_DCCO)

# Check against values verified from the reports -----------------------------------
expected <- tribble(
  ~Year, ~CLR_AWPE, ~CLR_DCCO, ~UKL_AWPE, ~UKL_DCCO,
  2009, 2325, 172, 438, 1538,
  2010, 722, 0, 81, 293,
  2011, 128, 77, 14, 373,
  2012, 510, 286, 255, 885,
  2013, 1175, 82, 152, 1071,
  2014, 296, 197, 247, 1133,
  2015, 503, 71, 492, 515,
  2016, NA, NA, NA, NA,
  2017, NA, NA, NA, NA,
  2018, 631, 162, 510, 544,
  2019, 58, 136, 129, 710,
  2020, 1166, 200, 391, 416,
  2021, 1094, 110, NA, NA,
  2022, 509, 114, 97, 158,
  2023, 1621, 163, 232, 370,
  2024, 1422, 182, 312, 301) |>
  mutate(across(everything(), as.integer))

mismatches <- expected |>
  pivot_longer(-Year, values_to = "expected") |>
  left_join(pivot_longer(annual_birds, -Year, values_to = "extracted"), by = c("Year", "name")) |>
  filter(expected != extracted | is.na(expected) != is.na(extracted))
stopifnot(nrow(annual_birds) == 30, nrow(mismatches) == 0)

# Save (review first) ------------------------------------------------------------
# write_csv(colony_counts_long, file.path(input_dir, "colony_counts_long.csv"))
# write_csv(annual_birds, file.path(input_dir, "annual_birds.csv"))
