library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
library(purrr)


##  ---- SPAWNER ----
pdf_path <- "data-raw/2023.Spring.Chinook.Megatable.v.28-Mar-2024.pdf"
tables_stream <- extract_tables(pdf_path, method = "stream", output = "tibble")

# Same approach than fall run megatable ----

#for some reason it is not reading the table the same way - River Harvest is too empty, that might be why
spawner_1 <- tables_stream[[1]]
spawner_2 <- tables_stream[[2]]
spawner_3 <- tables_stream[[3]]

spawner_4 <- tables_stream[[4]]
spawner_5 <- tables_stream[[5]]
spawner_6 <- tables_stream[[6]]

spawner_13 <- tables_stream[[13]]
spawner_14 <- tables_stream[[14]]
spawner_15 <- tables_stream[[15]]

spawner_20 <- tables_stream[[20]]


### Alternative approach 1 ----

# Extract text from page 1
page_text <- pdf_text(pdf_path)[1]

# View raw text
cat(page_text)

page_text <- str_split(page_text, "\n")[[1]] |>
  trimws() |>
  discard(~ .x == "")

start_idx <- which(str_detect(page_text, regex("River Harvest", ignore_case = TRUE))) + 1
end_idx <- which(str_detect(page_text, regex("^Totals|^Total Run-size Estimates", ignore_case = TRUE))) - 1

# Extract just the lines from the In-River Run section
run_lines <- page_text[start_idx:end_idx]

# Preview
run_lines

run_data <- str_split_fixed(run_lines, "\\s{2,}", n = 7)
run_df <- as.data.frame(run_data, stringsAsFactors = FALSE)

# Preview
head(run_df)

start_year <- 1980
colnames(run_df) <- c("location",
                      paste0("grilse_", start_year),
                      paste0("adults_", start_year),
                      paste0("totals_", start_year),
                      paste0("grilse_", start_year + 1),
                      paste0("adults_", start_year + 1),
                      paste0("totals_", start_year + 1))

# Tidy it
run_clean <- run_df |>
  mutate(across(-location, ~ readr::parse_number(str_remove(.x, "\\s*[a-zA-Z/]+$")))) |>
  mutate(section = "In-River Run") |>
  pivot_longer(
    cols = -c(location, section),
    names_to = c("category", "year"),
    names_sep = "_",
    values_to = "value"
  ) |>
  mutate(
    year = as.integer(year),
    category = str_to_title(category)
  )

