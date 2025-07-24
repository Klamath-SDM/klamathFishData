library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(janitor)

## Trying reading PDD directly in R ----

# read text from the PDF (page 1 only)
text <- pdf_text("data-raw/2022_Klamath_Basin_Megatable_20230216.pdf")
lines <- str_split(text[1], "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]

is_header_line <- function(x) {
  length(str_extract_all(x, "\\d{1,3}(,\\d{3})*")[[1]]) == 0
}

lines_fixed <- c()
i <- 1
while (i <= length(lines)) {
  if (is_header_line(lines[i]) && i < length(lines) && !is_header_line(lines[i + 1])) {
    lines_fixed <- c(lines_fixed, paste(lines[i], lines[i + 1]))
    i <- i + 2
  } else {
    lines_fixed <- c(lines_fixed, lines[i])
    i <- i + 1
  }
}

records <- list()

for (line in lines_fixed) {
  nums_raw <- str_extract_all(line, "\\d{1,3}(,\\d{3})*")[[1]]
  nums_clean <- gsub(",", "", nums_raw)

  nums_numeric <- as.numeric(nums_clean)
  if (length(nums_numeric) < 9) {
    nums_numeric <- c(nums_numeric, rep(NA, 9 - length(nums_numeric)))
  } else if (length(nums_numeric) > 9) {
    nums_numeric <- nums_numeric[1:9]
  }

  location_cleaned <- line
  for (n in nums_raw[1:length(nums_numeric)]) {
    location_cleaned <- str_replace(location_cleaned, fixed(n), "")
  }
  location <- str_squish(location_cleaned)

  records[[length(records) + 1]] <- c(Location = location, nums_numeric)
}

# create df
pdf_raw_data <- as.data.frame(do.call(rbind, records), stringsAsFactors = FALSE)
colnames(pdf_raw_data) <- c("Location",
                  "Grilse_1978", "Adults_1978", "Total_1978",
                  "Grilse_1979", "Adults_1979", "Total_1979",
                  "Grilse_1980", "Adults_1980", "Total_1980")

# convert numeric columns
pdf_raw_data[, -1] <- lapply(pdf_raw_data[, -1], as.numeric)

# reshape
raw_data_long <- pdf_raw_data |>
  pivot_longer(cols = -Location,
               names_to = c("Metric", "Year"),
               names_sep = "_",
               values_to = "Value") |>
  pivot_wider(names_from = Metric, values_from = Value) |>
  arrange(Year, Location)

print(unique(raw_data_long$Location))
print(head(raw_data_long))


# tabula approach ----

raw_tabula <- read_csv("data-raw/tabula-megatable-test.csv", skip = 1, col_names = FALSE)

# Step 2: Clean column names and handle merged/multilevel headers manually
# We'll manually assign meaningful column names
colnames(raw_tabula) <- c("section", "1978_grilse", "blank1", "1978_adults", "blank2", "1978_totals",
                   "1979_grilse", "1979_adults", "1979_totals",
                   "blank3", "blank4", "1980_grilse", "blank5", "1980_adults", "1980_totals")

# remove blank or header rows
tabula_cleaned <- raw_tabula |>
  filter(!is.na(section) & !str_detect(section, "Grilse|Adults|Totals|NA")) |>
  mutate(across(everything(), ~na_if(., "--"))) |>
  mutate(across(everything(), ~na_if(., "b/"))) |>
  mutate(across(everything(), ~na_if(., "c/"))) |>
  mutate(across(contains("grilse") | contains("adults") | contains("totals"), ~parse_number(.)))

# metadata columns to store structure
tabula_cleaned <- tabula_cleaned |>
  mutate(section_main = case_when(str_detect(section, "Spawner") ~ "Spawner Escapement",
                                  str_detect(section, "Harvest") ~ "In-River Harvest",
                                  str_detect(section, "In-river Run") ~ "In-River Run",
                                  TRUE ~ NA_character_),
         section_type = case_when(str_detect(section, "Hatchery") ~ "Hatchery Spawners",
                                  str_detect(section, "Natural Spawners") ~ "Natural Spawners",
                                  str_detect(section, "Angler") ~ "Angler Harvest",
                                  str_detect(section, "Indian") ~ "Indian Net Harvest",
                                  str_detect(section, "Total") ~ "Total",
                                  TRUE ~ section_type)) |>
  fill(section_main, .direction = "down")  |>
  fill(section_type, .direction = "down")  |>
  select(section_main, section_type, section, starts_with("1978"), starts_with("1979"), starts_with("1980"))

#pivot longer
long_format <- cleaned |>
  pivot_longer(cols = starts_with("19"), names_to = c("year", "age_class"), names_sep = "_", values_to = "count") |>
  filter(!is.na(count))


# Utilizing AI tools option ----

raw_escapement <- read_csv("data-raw/Klamath_Spawner_Escapement_processed_v2.csv")

# checks
colnames(raw_escapement)
unique(raw_escapement$location)
unique(raw_escapement$estimate_type)
unique(raw_escapement$year)



# checks

raw_escapement |>
  filter(year %in% c("1978", "1979", "1980"),
         estimate_type == "In-river Harvest") |>
  view()

# Trying tabula without subheaders - approach above has too many inconsistencies ----

raw_escapement_tabula <- read_csv("data-raw/tabula-2022_no_categories.csv", col_names = TRUE, skip = 1) |> glimpse()



# reading PDF with code ----

library(pdftools)
library(tidyverse)

pdf_text <- pdf_text("data-raw/page1_megatable_raw.pdf")
lines <- strsplit(pdf_text[1], "\n")[[1]]

# === Step 2: Fix broken lines (e.g., location name on one line, parentheses on next) ===
fix_multiline_locations <- function(lines) {
  fixed <- c()
  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    if (str_detect(str_trim(line), "^\\(.*\\)")) {
      # Append to previous line if it starts with parentheses
      if (length(fixed) > 0) {
        fixed[length(fixed)] <- paste(fixed[length(fixed)], line)
      }
    } else {
      fixed <- c(fixed, line)
    }
    i <- i + 1
  }
  return(fixed)
}

trimmed_lines <- str_trim(lines)

# SPAWNING ESCAPEMENT SECTION ----

start_idx <- which(trimmed_lines == "SPAWNER ESCAPEMENT") + 1
end_idx <- which(trimmed_lines == "IN-RIVER HARVEST") - 1

# Safety check
if (length(start_idx) == 0 || length(end_idx) == 0) {
  stop("Could not find one or both section headers. Check line spacing and capitalization.")
}

# Extract only the spawner escapement block
spawner_block <- trimmed_lines[start_idx:end_idx]

# Recombine multiline names like before
joined_lines <- c()
i <- 1
while (i <= length(spawner_block)) {
  line1 <- str_trim(spawner_block[i])
  line2 <- if (i + 1 <= length(spawner_block)) str_trim(spawner_block[i + 1]) else ""

  # If next line is a parenthetical and contains numbers, it's the main data line
  if (str_detect(line2, "^\\(.*\\)") && str_detect(line2, "[0-9]")) {
    full_line <- paste(line1, line2)
    joined_lines <- c(joined_lines, full_line)
    i <- i + 2  # skip the next line
  } else {
    joined_lines <- c(joined_lines, line1)
    i <- i + 1
  }
}

# === Step 3: Filter only Spawner Escapement section ===
spawner_lines <- joined_lines[which(
  grepl("Iron Gate Hatchery|Trinity River Hatchery|Subtotals|Trinity River basin|Salmon River|Scott River|Shasta River|Bogus Creek|Main Stem Klamath River|Misc\\.|Reservation tribs|Total Spawner Escapement", joined_lines)
)]

# === Step 4: Parse each row into tidy format ===
# parse_row <- function(line) {
#   line <- str_replace_all(line, "\\s*[a-d]/", "")
#
#   # Extract number-like tokens and "--"
#   raw_values <- str_extract_all(line, "[0-9,]+|--")[[1]]
#
#   # Clean and convert to numeric, treat "--" as NA
#   values <- str_replace_all(raw_values, ",", "")
#   values <- ifelse(values == "--", NA, values)
#   values <- suppressWarnings(as.numeric(values))
#
#   # Pad or trim to exactly 9 values (3 years × 3 categories)
#   if (length(values) < 9) values <- c(values, rep(NA, 9 - length(values)))
#   if (length(values) > 9) values <- values[1:9]
#
#   # Extract location (text before numbers)
#   name <- str_trim(str_extract(line, "^[^0-9\\-–]+"))
#
#   # Tidy output
#   tibble(Location = name,
#          Year = rep(c(1978, 1979, 1980), each = 3),
#          Category = rep(c("Grilse", "Adults", "Total"), times = 3),
#          Value = values)
#   }

### TRY
parse_row <- function(line) {
  # Remove any footnotes like "c/", "d/" (but preserve "--")
  clean_line <- str_replace_all(line, "\\s*[a-d]/", "")

  # Extract the location name — all text up to the first number or "--"
  name <- str_trim(str_extract(clean_line, "^.*?(?=\\s+[0-9]|\\s+--)"))

  # Remove location name from the string
  numbers_only <- str_remove(clean_line, fixed(name))

  # Extract numbers and "--"
  raw_values <- str_extract_all(numbers_only, "[0-9,]+|--")[[1]]

  # Clean values: convert "--" to NA, remove commas
  values <- str_replace_all(raw_values, ",", "")
  values <- ifelse(values == "--", NA, values)
  values <- suppressWarnings(as.numeric(values))

  # Ensure we always have exactly 9 values
  values <- values[1:min(9, length(values))]
  if (length(values) < 9) values <- c(values, rep(NA, 9 - length(values)))

  # Return tidy row
  tibble(
    Location = name,
    Year = rep(c(1978, 1979, 1980), each = 3),
    Category = rep(c("Grilse", "Adults", "Total"), times = 3),
    Value = values
  )
}

# Apply parsing to each line
spawner_df <- map_dfr(spawner_lines, parse_row)

## making sure that the subtotal fields are specified what they correspond to

assign_subsections <- function(df) {
  current_subsection <- NA
  subsection_col <- character(nrow(df))

  for (i in seq_len(nrow(df))) {
    loc <- df$Location[i]

    if (str_detect(loc, "Hatchery")) {
      current_subsection <- "Hatchery Spawners"
    } else if (str_detect(loc, "Reservation tribs|River|Creek|Misc|basin")) {
      current_subsection <- "Natural Spawners"
    } else if (str_detect(loc, "Total Spawner Escapement")) {
      current_subsection <- "Total"
    } else if (str_detect(loc, "Subtotal")) {
      # Only wrap if current_subsection is not already a subtotal
      if (!str_detect(current_subsection, "^Subtotal")) {
        current_subsection <- paste0("Subtotal (", current_subsection, ")")
      }
    }

    subsection_col[i] <- current_subsection
  }

  df$Subsection <- subsection_col
  return(df)
}

spawner_df <- spawner_df |>
  mutate(Section = "Spawner Escapement") |>
  assign_subsections()
# end of subtotal fields fix


# Optional: clean up "Subtotal" label
spawner_df <- spawner_df |>
  mutate(Location = ifelse(str_detect(Location, "Subtotals"), "Subtotal", Location))


# Add Section and Subsection tags ===
spawner_df <- spawner_df |>
  mutate(Section = "Spawner Escapement",
         Subsection = case_when(
           str_detect(Location, "Hatchery") ~ "Hatchery Spawners",
           str_detect(Location, "Subtotals") ~ "Subtotal",
           str_detect(Location, "Total Spawner Escapement") ~ "Total",
           TRUE ~ "Natural Spawners"))

#TODO from this point on, everything look good except that Trinity River basin numbers are skipping Grilse value
# up to this point the Spawner Scapement data is pretty clean once Trinity River basin is worked out

# IN-HARVEST SECTION ----
#
# # === Step 1: Get lines from IN-RIVER HARVEST section ===
# start_idx_hrv <- which(trimmed_lines == "IN-RIVER HARVEST") + 1
# end_idx_hrv <- which(trimmed_lines == "IN-RIVER RUN") - 1
# harvest_block <- trimmed_lines[start_idx_hrv:end_idx_hrv]
#
# # === Step 2: Fix broken rows where name is on one line and numbers on the next ===
# joined_lines_hrv <- c()
# i <- 1
# while (i <= length(harvest_block)) {
#   line1 <- str_trim(harvest_block[i])
#
#   # Skip column headers (repeating Grilse, Adults, Totals row)
#   if (str_detect(line1, "^Angler Harvest\\s+Grilse\\s+Adults") ||
#       str_detect(line1, "Grilse\\s+Adults\\s+Totals\\s+Grilse")) {
#     i <- i + 1
#     next
#   }
#
#   # Check if this line contains NO numbers, and next line does → join them
#   line2 <- if (i + 1 <= length(harvest_block)) str_trim(harvest_block[i + 1]) else ""
#
#   if (!str_detect(line1, "\\d{3,}") && str_detect(line2, "\\d{3,}")) {
#     merged <- paste(line1, line2)
#     joined_lines_hrv <- c(joined_lines_hrv, merged)
#     i <- i + 2
#   } else {
#     joined_lines_hrv <- c(joined_lines_hrv, line1)
#     i <- i + 1
#   }
# }
#
# # === Step 3: Filter only lines of interest ===
# harvest_lines <- joined_lines_hrv[which(
#   grepl("Klamath River \\(below Hwy 101 bridge\\)|Trinity River basin \\(above Willow Creek\\)|Balance of Klamath system|Klamath River \\(Hwy 101 to Trinity mouth\\)|Trinity River \\(Hoopa Reservation\\)|Subtotals|Total In-river Harvest", joined_lines_hrv)
# )]
#
# # === Step 4: Parse each row into tidy format ===
# parse_row_harvest <- function(line) {
#   line <- str_replace_all(line, "\\s*[a-d]/", "")
#
#   # Extract location
#   name <- str_trim(str_extract(line, "^.*?(?=\\s+[0-9]|\\s+--)"))
#
#   # Extract numbers
#   values_only <- str_remove(line, fixed(name))
#   raw_values <- str_extract_all(values_only, "[0-9,]+|--")[[1]]
#   values <- str_replace_all(raw_values, ",", "")
#   values <- ifelse(values == "--", NA, values)
#   values <- suppressWarnings(as.numeric(values))
#   values <- values[1:min(9, length(values))]
#   if (length(values) < 9) values <- c(values, rep(NA, 9 - length(values)))
#
#   tibble(
#     Location = name,
#     Year = rep(c(1978, 1979, 1980), each = 3),
#     Category = rep(c("Grilse", "Adults", "Total"), times = 3),
#     Value = values
#   )
# }
#
# harvest_df <- map_dfr(harvest_lines, parse_row_harvest)
#
# # === Step 5: Assign Subsections based on order ===
# assign_subsections_harvest <- function(df) {
#   current_subsection <- NA
#   subsection_col <- character(nrow(df))
#
#   for (i in seq_len(nrow(df))) {
#     loc <- df$Location[i]
#
#     if (str_detect(loc, "Trinity River basin|Balance of Klamath system|Klamath River \\(below Hwy 101 bridge\\)")) {
#       current_subsection <- "Angler Harvest"
#     } else if (str_detect(loc, "Trinity River \\(Hoopa Reservation\\)|Klamath River \\(Hwy 101 to Trinity mouth\\)")) {
#       current_subsection <- "Indian Net Harvest"
#     } else if (str_detect(loc, "Subtotal")) {
#       if (!str_detect(current_subsection, "^Subtotal")) {
#         current_subsection <- paste0("Subtotal (", current_subsection, ")")
#       }
#     } else if (str_detect(loc, "Total In-river Harvest")) {
#       current_subsection <- "Total"
#     }
#
#     subsection_col[i] <- current_subsection
#   }
#
#   df$Subsection <- subsection_col
#   return(df)
# }
#
# # === Step 6: Finalize dataframe ===
# harvest_df <- harvest_df |>
#   mutate(Section = "In-River Harvest") |>
#   assign_subsections_harvest() |>
#   mutate(Location = ifelse(str_detect(Location, "Subtotals"), "Subtotal", Location))


#TODO this section still needs work, names that contain numbers are not being read properly



### In-harvest - BEST APPROACH  -----
# Set Java memory limit (optional but helpful for large PDFs)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
# Define the PDF file path (download it manually first)
pdf_path <- "data-raw/2022_Klamath_Basin_Megatable_20230216.pdf"

# Try stream method with tibble output
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

megatable_2 <- tables_stream[[2]]  # or tables_stream[[1]] depending on quality

megatable_clean_2 <- megatable_2 |>
  filter(if_any(everything(), ~ . != "")) |>
  janitor::clean_names() |>
  slice(3:14) |>
  # separate(x74_906_6_761, into = c("totals_1978", "grilse_1979"), sep = "\\s+", convert = TRUE) |> view()
  extract(x74_906_6_761,
          into = c("totals_1978", "grilse_1979"),
          regex = "^(-- d/|\\d{1,3}(?:,\\d{3})*)\\s+(\\d{1,3}(?:,\\d{3})*)$",
          remove = TRUE) |>
  mutate(across(where(is.character), ~ na_if(., "--"))) |>
  mutate(across(c(totals_1978, grilse_1979), ~ readr::parse_number(.)))

# Slice and rename columns
in_harvest_df <- megatable_clean_2 |>
  slice(2:12) |>
  rename(location = subtotals,
         grilse_1978 = x16_414,
         adults_1978 = x58_492,
         adults_1979 = x30_637,
         totals_1979 = x37_398,
         grilse_1980 = x26_982,
         adults_1980 = x21_483,
         totals_1980 = x48_465,) |>
  select(location, grilse_1978, adults_1978, totals_1978, grilse_1979, adults_1979,
         totals_1979, grilse_1980, adults_1980, totals_1980)
  # mutate(across(-location, ~ na_if(., "--")))

# assign subsection from group headers
in_harvest_df_clean <- in_harvest_df |>
  mutate(subsection = case_when(
    str_detect(location, "Angler Harvest") ~ "Angler Harvest",
    str_detect(location, "Tribal Net Harvest") ~ "Tribal Net Harvest",
    TRUE ~ NA_character_)) |>
  fill(subsection, .direction = "down") |>
  filter(!location %in% c("Angler Harvest", "Tribal Net Harvest  e/"),
                          # "Subtotals", "Total In-river Harvest"),
         !is.na(location)) |>
  mutate(section = "In-River Harvest")

# pivot longer
in_harvest_long <- in_harvest_df_clean |>
  mutate(across(matches("_(1978|1979|1980)$"), as.character)) |>
  pivot_longer(cols = matches("_(1978|1979|1980)$"),
               names_to = c("category", "year"),
               names_sep = "_",
               values_to = "value") |>
  mutate(year = as.integer(year),
         category = str_to_title(category),
         # value = parse_number(value)
         value = readr::parse_number(value)) |>
  view()
