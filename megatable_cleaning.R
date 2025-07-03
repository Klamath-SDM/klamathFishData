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
               values_to = "Value") %>%
  pivot_wider(names_from = Metric, values_from = Value) %>%
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

# TODO - this might be the cleanest option, some additional checks and wrangling should be done
