# ============================================================
# Extract Table 5 from:
#   "Klamath River Fall Chinook Salmon Age-Specific Escapement,
#    River Harvest, and Run Size Estimates, 2025 Run"
#   Klamath River Technical Team, May 20, 2026
#
# Requires:  pdftools  (install.packages("pdftools"))
#            dplyr     (install.packages("dplyr"))
#            tidyr     (install.packages("tidyr"))
# ============================================================

library(pdftools)
library(dplyr)
library(tidyr)

# ── 1. Download + read PDF ────────────────────────────────────────────────────

pdf_url <- paste0(
  "https://www.pcouncil.org/documents/2026/06/",
  "2026-run-klamath-river-fall-chinook-salmon-age-specific-escapement-",
  "river-harvest-and-run-size-estimates-2026-run-may-20-2026.pdf"
)

# pdf_text() returns one character string per page
# Table 5 is on page 14 of the PDF
cat("Downloading PDF...\n")
pages <- pdf_text(pdf_url)
page14 <- pages[[14]]
cat("Page 14 extracted (", nchar(page14), " characters)\n\n")

# ── 2. Split into lines and locate Table 5 ────────────────────────────────────

lines <- strsplit(page14, "\n")[[1]]

# Find the header line and the footnote that ends the table
start_idx <- grep("Escapement & Harvest", lines, fixed = TRUE)
end_idx   <- grep("^\\s*\\*\\s+New survey reach", lines)

# Fallback: grab everything after header until end of page
if (length(end_idx) == 0) end_idx <- length(lines) + 1

table_lines <- lines[(start_idx + 1):(end_idx - 1)]

# ── 3. Parse data rows ────────────────────────────────────────────────────────
# Each data row ends with 6 integers (age2, age3, age4, age5, total_adults, total_run)
# Section header lines (e.g. "Hatchery Spawners", "Natural Spawners") have no numbers

# Regex: label = everything before the trailing 6 numbers
data_pattern <- paste0(
  "^(.+?)\\s+",          # sector label (non-greedy)
  "([\\d,]+)\\s+",       # age-2
  "([\\d,]+)\\s+",       # age-3
  "([\\d,]+)\\s+",       # age-4
  "([\\d,]+)\\s+",       # age-5
  "([\\d,]+)\\s+",       # total adults
  "([\\d,]+)\\s*$"       # total run
)

# Helper: strip commas and convert to integer
to_int <- function(x) as.integer(gsub(",", "", trimws(x)))

rows <- list()
current_section <- NA_character_

for (line in table_lines) {
  line <- trimws(line)
  if (nchar(line) == 0) next

  m <- regmatches(line, regexec(data_pattern, line, perl = TRUE))[[1]]

  if (length(m) == 8) {
    # Data row
    rows[[length(rows) + 1]] <- data.frame(
      section      = current_section,
      sector       = trimws(m[2]),
      age2         = to_int(m[3]),
      age3         = to_int(m[4]),
      age4         = to_int(m[5]),
      age5         = to_int(m[6]),
      total_adults = to_int(m[7]),
      total_run    = to_int(m[8]),
      stringsAsFactors = FALSE
    )
  } else {
    # Section header — update current section label
    # Skip lines that are just whitespace/punctuation
    if (grepl("[A-Za-z]", line)) {
      current_section <- line
    }
  }
}

table5_wide <- bind_rows(rows)

# ── 4. Clean up sector labels (strip footnote markers * and a/) ───────────────

table5_wide <- table5_wide |>
  mutate(
    sector = gsub("\\*|a/", "", sector) |> trimws()
  )

# ── 5. Select columns, rename, assign section / subsection / year ─────────────

# Subtotal and grand-total rows to exclude
drop_pattern <- paste(
  "subtotal", "Total Spawning Escapement", "Total Harvest",
  "Harvest and escapement",
  "dropoff mortality", "Ich disease monitoring",
  sep = "|"
)

# Section / subsection lookup based on the 'section' column captured during parsing
#
#   section (raw from parser)  →  section label       subsection label
#   ─────────────────────────────────────────────────────────────────────
#   Hatchery Spawners          →  Spawning Escapement   Hatchery Spawners
#   Natural Spawners           →  Spawning Escapement   Natural Spawners
#   Recreational Harvest       →  Harvest              Recreational Harvest
#   Tribal Harvest             →  Harvest              Tribal Harvest
#   Totals                     →  In-River Run         In-River Run

section_map <- tribble(
  ~section_raw,            ~section,             ~subsection,
  "Hatchery Spawners",     "Spawning Escapement", "Hatchery Spawners",
  "Natural Spawners",      "Spawning Escapement", "Natural Spawners",
  "Recreational Harvest",  "Harvest",            "Recreational Harvest",
  "Tribal Harvest",        "Harvest",            "Tribal Harvest",
  "Totals",                "In-River Run",       "In-River Run"
)

table5_filtered <- table5_wide |>
  # Drop subtotal / grand-total rows
  filter(!grepl(drop_pattern, sector, ignore.case = TRUE)) |>
  # Keep only Total River Run from the "Totals" section
  filter(!(section == "Totals" & sector != "Total River Run")) |>
  # Keep only the three columns of interest plus identifiers
  select(section_raw = section, location = sector, Grilse = age2,
         Adult = total_adults, `Total Run` = total_run) |>
  # Join section / subsection labels
  left_join(section_map, by = "section_raw") |>
  select(-section_raw) |>
  # Add year
  mutate(year = 2025L)

# ── 6. Pivot to long format ───────────────────────────────────────────────────

krtt_2025_data <- table5_filtered |>
  pivot_longer(
    cols      = c(Grilse, Adult, `Total Run`),
    names_to  = "lifestage",
    values_to = "value"
  ) |>

  mutate(species = "fall chinook salmon") |>
  select(location, subsection, section, lifestage, year, value, species)

# ── 7. Sanity checks ──────────────────────────────────────────────────────────

# total_run_check <- table5_long |>
#   filter(section == "In-River Run", lifestage == "Total Run") |>
#   pull(value)
#
# stopifnot(
#   nrow(table5_long) > 0,
#   length(total_run_check) == 1,
#   total_run_check == 70022
# )


# PAGE 30, APPENDIX G TABLE FOR 2024 DATA -------------

pages   <- pdf_text(pdf_url)
page30  <- pages[[30]]   # Appendix G is on page 30
cat("Page 30 extracted (", nchar(page30), " characters)\n\n")

# ── 2. Split into lines and locate Appendix G table ───────────────────────────

lines <- strsplit(page30, "\n")[[1]]

start_idx <- grep("Escapement & Harvest", lines, fixed = TRUE)
end_idx   <- grep("^\\s*\\*\\s+New survey reach", lines)

if (length(end_idx) == 0) end_idx <- length(lines) + 1

table_lines <- lines[(start_idx + 1):(end_idx - 1)]

# ── 3. Parse data rows ────────────────────────────────────────────────────────

data_pattern <- paste0(
  "^(.+?)\\s+",
  "([\\d,]+)\\s+",   # age-2
  "([\\d,]+)\\s+",   # age-3
  "([\\d,]+)\\s+",   # age-4
  "([\\d,]+)\\s+",   # age-5
  "([\\d,]+)\\s+",   # total adults
  "([\\d,]+)\\s*$"   # total run
)

to_int <- function(x) as.integer(gsub(",", "", trimws(x)))

rows <- list()
current_section <- NA_character_

for (line in table_lines) {
  line <- trimws(line)
  if (nchar(line) == 0) next

  m <- regmatches(line, regexec(data_pattern, line, perl = TRUE))[[1]]

  if (length(m) == 8) {
    rows[[length(rows) + 1]] <- data.frame(
      section      = current_section,
      sector       = trimws(m[2]),
      age2         = to_int(m[3]),
      age3         = to_int(m[4]),
      age4         = to_int(m[5]),
      age5         = to_int(m[6]),
      total_adults = to_int(m[7]),
      total_run    = to_int(m[8]),
      stringsAsFactors = FALSE
    )
  } else {
    if (grepl("[A-Za-z]", line)) current_section <- line
  }
}

table_wide <- bind_rows(rows)

# ── 4. Clean up footnote markers ──────────────────────────────────────────────

table_wide <- table_wide |>
  mutate(sector = gsub("\\*|a/", "", sector) |> trimws())

# ── 5. Select, rename, filter, and assign section / subsection / year ─────────

drop_pattern <- paste(
  "subtotal", "Total Spawner Escapement", "Total Harvest",
  "Harvest and escapement",
  "dropoff mortality", "Ich disease monitoring",
  sep = "|"
)

section_map <- tribble(
  ~section_raw,            ~section,              ~subsection,
  "Hatchery Spawners",     "Spawning Escapement", "Hatchery Spawners",
  "Natural Spawners",      "Spawning Escapement", "Natural Spawners",
  "Recreational Harvest",  "Harvest",             "Recreational Harvest",
  "Tribal Harvest",        "Harvest",             "Tribal Harvest",
  "Totals",                "In-River Run",        "In-River Run"
)

table_filtered <- table_wide |>
  filter(!grepl(drop_pattern, sector, ignore.case = TRUE)) |>
  filter(!(section == "Totals" & sector != "Total River Run")) |>
  select(section_raw = section, location = sector, Grilse = age2,
         Adult = total_adults, `Total Run` = total_run) |>
  left_join(section_map, by = "section_raw") |>
  select(-section_raw) |>
  mutate(year = 2024L)

# ── 6. Pivot to long format ───────────────────────────────────────────────────

krtt_2024_data <- table_filtered |>
  pivot_longer(
  cols      = c(Grilse, Adult, `Total Run`),
  names_to  = "lifestage",
  values_to = "value"
) |>

  mutate(species = "fall chinook salmon") |>
  select(location, subsection, section, lifestage, year, value, species)


## Combine 2024 and 2025 data
fall_run_2024_2025 <- bind_rows(krtt_2024_data, krtt_2025_data) |> glimpse()
