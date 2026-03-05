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
library(readxl)

source("data-raw/processing-scripts/read_fall_megatable_pdf.R")
source("data-raw/processing-scripts/read_spring_megatable_pdf.R")


# Fall Run megatable -----------------------------------
fall_harvest_clean <- filter(fall_megatable, section == "In-River Harvest") |>
  filter(!location %in% c("Subtotals", "Total In-river Harvest", "Angler Harvest Subtotals:",
                          "Indian Net Harvest Subtotals:", "Tribal  Harvest Subtotals:")) |>
  mutate(origin = "unknown") |> # TODO look into fisheries regulation literature
  glimpse()

# Spring Run megatable -----------------------------------
spring_harvest_clean <- filter(spring_harvest_clean, section == "River Harvest") |>
  glimpse()










