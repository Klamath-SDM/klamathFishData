# Context: Model inputs for sucker DSM
# annual colonial waterbird colony counts (annual.birds), 1995-2024
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

library(tidyverse)
library(xml2)
library(pdftools)

input_dir <- "data-raw/helper-data/model-inputs"

# report labels -> readable names (S.1 says "Sheep Lake", A1 says "Sheepy Lake")
species_names <- c(Pelicans = "American White Pelican",
                   Cormorants = "Double-crested Cormorant",
                   Terns = "Caspian Tern")
location_names <- c("Sheep Lake" = "Sheepy Lake")
species_pattern <- str_c(names(species_names), collapse = "|")

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
  mutate(location = str_remove(location, "[[:space:]]+[a-c]$") |> recode(!!!location_names),
         species = recode(species, !!!species_names)) |>
  pivot_longer(`2009`:`2020`, names_to = "year", values_to = "raw") |>
  mutate(year = as.integer(year),
         colony_abandoned = str_detect(raw, fixed("*")),
         count = suppressWarnings(as.integer(str_remove_all(raw, "[*,]"))),
         count_unit = "individuals",
         count_status = case_when(raw == "NC" ~ "no_colony",
                                  raw %in% c("–", "-") ~ "present_not_counted",
                                  raw == "" ~ "not_available",
                                  .default = "counted"),
         count_method = "unspecified",
         source = if_else(year <= 2014,
                          "Evans et al. 2022 (Supplemental) Table S.1; counts for 2009-2014 from Evans et al. 2016",
                          "Evans et al. 2022 (Supplemental) Table S.1")) |>
  select(year, location, species, count, count_unit, count_status,
         colony_abandoned, count_method, source)

# Banet et al. 2025, Table A1 (pdf page 24) --------------------------------------
# Footnotes: 3 = NA, colony active but unsuccessful; 4 = individuals counted from OSU photos;
# 5 = breeding pairs (OSU)
a1 <- pdf_text(file.path(input_dir, "Banet_et_al_2025.pdf"))[24] |>
  read_lines() |>
  keep(\(line) str_detect(line, str_c("(", species_pattern, ")[[:space:]]{2,}[^[:space:]]"))) |>
  tibble(line = _) |>
  mutate(species = str_extract(line, species_pattern),
         location = c("Upper Klamath Lake", "Clear Lake Reservoir",
                      "Sheepy Lake", "Tule Lake")[cumsum(species == "Pelicans")],
         cells = line |>
           str_remove(str_c("^.*?(", species_pattern, ")[[:space:]]+")) |>
           str_trim() |>
           str_split("[[:space:]]{2,}") |>
           map(\(x) set_names(x, 2021:2024))) |>
  select(location, species, cells) |>
  mutate(species = recode(species, !!!species_names)) |>
  unnest_longer(cells, values_to = "raw", indices_to = "year") |>
  mutate(year = as.integer(year),
         footnote = str_extract(raw, "(?<=[[:space:]])[0-9]$"),
         value = str_remove(raw, "[[:space:]][0-9]$") |> str_remove_all(","),
         count = suppressWarnings(as.integer(value)),
         colony_abandoned = FALSE,
         count_unit = case_when(footnote == "5" ~ "pairs",
                                footnote == "4" ~ "individuals",
                                .default = "unspecified"),
         count_status = if_else(is.na(count), "not_available", "counted"),
         count_method = case_when(footnote == "5" ~ "osu_pair_count",
                                  footnote == "4" ~ "osu_photo_count",
                                  .default = "unspecified"),
         source = "Banet et al. 2025 Table A1 (Appendix A, p. 24)") |>
  select(year, location, species, count, count_unit, count_status,
         colony_abandoned, count_method, source)

# Clean, readable table for the package (lowercase names, no acronyms) -------------------
# count_status: counted | present_not_counted (S.1 dash) | no_colony (S.1 NC) | not_available
# count_method: unspecified | osu_photo_count (A1 footnote 4) | osu_pair_count (A1 footnote 5)
bird_colony_counts <- bind_rows(s1, a1) |>
  mutate(location = str_to_lower(location),
         species = str_to_lower(species)) |>
  arrange(year, location, species)


# save clean data
usethis::use_data(bird_colony_counts, overwrite = TRUE)
