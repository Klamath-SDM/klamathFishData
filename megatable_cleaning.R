library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)

# Extract PDF text
megatable_text <- pdf_text("data-raw/2022_Klamath_Basin_Megatable_20230216.pdf")


is_header_line <- function(x) {
  length(str_extract_all(x, "\\d{1,3}(,\\d{3})*")[[1]]) == 0
}

# Reconstruct locations like:
# Line 17: "Main Stem Klamath River"
# Line 18: "(excluding IGH)  300  1700  2000 ..."
lines_fixed <- c()
i <- 1
while (i <= length(lines)) {
  if (is_header_line(lines[i]) && i < length(lines) && !is_header_line(lines[i+1])) {
    lines_fixed <- c(lines_fixed, paste(lines[i], lines[i+1]))
    i <- i + 2
  } else {
    lines_fixed <- c(lines_fixed, lines[i])
    i <- i + 1
  }
}

lines <- str_split(megatable_text[1], "\n")[[1]]
lines <- str_trim(lines)
lines <- lines[lines != ""]  # Remove blank lines

extract_9_numbers <- function(line) {
  nums <- str_extract_all(line, "\\d{1,3}(,\\d{3})*")[[1]]
  if (length(nums) == 9) {
    return(as.numeric(gsub(",", "", nums)))
  } else {
    return(NULL)
  }
}
# loop through lines and extract location + numbers
records <- list()

for (line in lines) {
  nums <- extract_9_numbers(line)
  if (!is.null(nums)) {
    location <- str_trim(str_remove(line, "\\s*(\\d{1,3}(,\\d{3})?\\s+){8}\\d{1,3}(,\\d{3})?\\s*$"))
    records[[length(records) + 1]] <- c(Location = location, nums)
  }
}

# convert to df
megatable_1 <- as.data.frame(do.call(rbind, records), stringsAsFactors = FALSE)
colnames(megatable_1) <- c("Location",
                  "Grilse_1978", "Adults_1978", "Total_1978",
                  "Grilse_1979", "Adults_1979", "Total_1979",
                  "Grilse_1980", "Adults_1980", "Total_1980")

# convert number columns to numeric
megatable_1[, -1] <- lapply(megatable_1[, -1], as.numeric)

# reshape
megatable_1_long <- megatable_1 |>
  pivot_longer(cols = -Location,
               names_to = c("Metric", "Year"),
               names_sep = "_",
               values_to = "Value") |>
  pivot_wider(names_from = Metric, values_from = Value) |>
  arrange(Year, Location)

print(head(megatable_1_long))

megatable_1_long_clean <- megatable_1_long |>  # maybe we can just calculate mannually?
  filter(Location != "Subtotals") |> glimpse()


