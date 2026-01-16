# Script for processing sucker juvenile data
library(tidyverse)
# Sucker juvenile survival estimates
# These data were scraped (manually) from Table 9 and 10 of Martin et al 2024 (https://doi.org/10.3133/ofr20241013)

sucker_juvenile_survival <- read_csv(here::here("data-raw", "tables_with_data","sucker_juvenile_survival.csv"))

usethis::use_data(sucker_juvenile_survival, overwrite = T)
