library(pdftools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
library(purrr)


pdf_path <- "data-raw/Avian_Predation_on_UKB_Suckers_Summary_Report_2021_2023.pdf"
tables <- extract_tables(pdf_path, method = "stream", output = "tibble")

#### PIT-tagged LRS, SNS, KLS, SNS-KLS and SARP ----
tag_data_raw <- tables[[2]] |> glimpse()

tag_data_raw <- as.data.frame(tag_data_raw, stringsAsFactors = FALSE) %>%
  clean_names()

# The first number = the number of PIT-tagged fish available to avian predators that year in that location.
# This is essentially how many tagged suckers (or salmon) were released or present in the waterbody.
# The number in parentheses = the number of those tags recovered from avian predator nesting colonies in the same year.
# Recovery = physical detection of PIT tags on bird colonies (e.g., cormorants, pelicans) after birds consumed tagged fish.
# Importantly, these are minimum counts of predation events — not corrected for imperfect detection or deposition, so the true number of fish eaten could be higher.
parse_avail <- function(x) {
  # available number before parentheses, allowing commas
  v <- str_extract(x, "^[0-9,]+")
  v[grepl("^\\s*NR\\b", x, ignore.case = TRUE)] <- NA_character_
  as.numeric(str_replace_all(v, ",", ""))
}

parse_recovered <- function(x) {
  # number inside parentheses
  v <- str_extract(x, "(?<=\\()[0-9]+(?=\\))")
  v[grepl("^\\s*NR\\b", x, ignore.case = TRUE)] <- NA_character_
  as.numeric(v)
}

# split cols
tag_data_split <- tag_data_raw |>
  mutate(across(
    starts_with("x20"),
    list(available = ~ parse_avail(.x),
         recovered = ~ parse_recovered(.x)),
    .names = "{.col}_{.fn}"))

# remove the original columns
tag_data_split <- tag_data_split |>
  select(-matches("^x20\\d{2}$"))


tag_data_clean <- tag_data_split |>
  mutate(location_1 = case_when(
    row_number() %in% 1:9  ~ "Upper Klamath Lake",
    row_number() %in% 10    ~ "Klamath River",
    row_number() %in% 11:13 ~ "Clear Lake Reservoir",
    row_number() %in% 14 ~ "Sheepy Lake",
    TRUE ~ location_1)) |>
  slice(-c(5))

# year columns
year_cols <- grep("^x20\\d{2}_(available|recovered)$", names(tag_data_split), value = TRUE)

avian_predation_pit_tag <- tag_data_clean |>
  mutate(fish_group_2 = na_if(str_trim(fish_group_2), "")) |>
  filter(!(is.na(fish_group_2) & rowSums(!is.na(across(all_of(year_cols)))) == 0)) |>
  pivot_longer(cols = all_of(year_cols),
               names_to   = c("year", "metric"),
               names_pattern = "^x(20\\d{2})_(available|recovered)$",
               values_to  = "value") |>
  mutate(year  = as.integer(year),
         value = suppressWarnings(as.numeric(value))) |>
  pivot_wider(names_from  = metric,
              values_from = value) |>
  rename(location = location_1,
         fish_group = fish_group_2) |>
  arrange(location, fish_group, year) |>
  as.tibble() |>
  # mutate(recovered_rate = if_else(!is.na(available) & available > 0, recovered / available, NA_real_))
  glimpse()

# save clean data - how many tagged suckers were available and how many were recovered on bird colonies
# usethis::use_data(avian_predation_pit_tag, overwrite = TRUE)


#### estimate predation rates - LRS, SNS, KLS, SNS-KLS and SARP (UKL and Clear Lake) ----
estimate_predation_raw <- tables[[3]] |> glimpse()

pred_raw_clean <- estimate_predation_raw |>
  separate(col = 'Upper Klamath Lake Suckers', # split the mixed SNS/KLS column into two
           into = c("UKL_Adult_SNS", "UKL_Adult_KLS"),
           sep = "(?<=\\)|%)\\s+",
           extra = "merge",
           fill = "right") |>
  separate(col = 'Clear Lake Reservoir Suckers', # same for the Clear Lake combined column
           into = c("CLR_Adult_SNS", "CLR_Adult_KLS"),
           sep = "(?<=\\)|%)\\s+",
           extra = "merge",
           fill  = "right") |>
  separate(col = CLR_Adult_KLS,
           into = c("CLR_Adult_KLS", "CLR_Wild_Juv"),
           sep = "(?<=\\)|%)\\s+",
           extra = "merge",
           fill  = "right") |>
  mutate(across(starts_with("UKL_Adult_") | starts_with("CLR_Adult_"), str_trim)) # trim any whitespace off each new column

pred_raw_clean[8, c("CLR_Adult_SNS", "CLR_Adult_KLS", "CLR_Wild_Juv")] <- # migrating values from row 8 to row 7
  dplyr::coalesce(pred_raw_clean[8, c("CLR_Adult_SNS", "CLR_Adult_KLS", "CLR_Wild_Juv")],
                  pred_raw_clean[7, c("CLR_Adult_SNS", "CLR_Adult_KLS", "CLR_Wild_Juv")])

# Columns 2-5 = Upper Klamath Lake groups, columns 6-8 = Clear Lake Reservoir
names(pred_raw_clean)[2:5] <- c("UKL_Adult_LRS", "UKL_Adult_SNS", "UKL_Adult_KLS", "UKL_Juveniles")
names(pred_raw_clean)[6:8] <- c("CLR_Adult_LRS", "CLR_Adult_SNS_KLS", "CLR_Juveniles")

# Row indices where the years are listed
year_map <- tibble(row_index = c(4, 6, 8, 9, 10, 12),
                   year = c(2021, 2021, 2022, 2022, 2023, 2023))

# parse "estimate (lcl–ucl)"
parse_pred <- function(x) {
  if (is.na(x) || x %in% c("NA","–")) {
    return(tibble(est_pct = NA_character_, lcl_pct = NA_real_, ucl_pct = NA_real_))
    }

  # keep "<"
  x_clean <- str_remove_all(x, "[()%]")

  # range (CI)
  if (str_detect(x_clean, "–")) {
    rng <- str_split(x_clean, "–")[[1]] |>  str_trim()
    tibble(est_pct = NA_character_,
           lcl_pct = suppressWarnings(as.numeric(str_remove(rng[1], "%"))),
           ucl_pct = suppressWarnings(as.numeric(str_remove(rng[2], "%"))))

    # explicit "<" value (e.g. "< 0.1%")
    } else if (str_detect(x_clean, "<")) {
      tibble(est_pct = str_trim(x_clean),  # keep as character, e.g. "<0.1%"
             lcl_pct = NA_real_,
             ucl_pct = NA_real_)

      # normal percentage
      } else {
        tibble(est_pct = str_trim(str_remove(x_clean, "%")),
               lcl_pct = NA_real_,
               ucl_pct = NA_real_)
      }
}

# build table
estimates_suckers_clean <- map2_dfr(year_map$row_index, year_map$year, function(i, yr){
  row_vals <- pred_raw_clean[i, ]
  # Each column corresponds to one group
  bind_rows(
    parse_pred(row_vals$UKL_Adult_LRS) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Adult LRS", year=yr),
    parse_pred(row_vals$UKL_Adult_SNS) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Adult SNS", year=yr),
    parse_pred(row_vals$UKL_Adult_KLS) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Adult KLS", year=yr),
    parse_pred(row_vals$UKL_Juveniles) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Juveniles", year=yr),
    parse_pred(row_vals$CLR_Adult_LRS) %>% mutate(location_1="Clear Lake Reservoir", fish_group_2="Adult LRS", year=yr),
    parse_pred(row_vals$CLR_Adult_SNS_KLS) %>% mutate(location_1="Clear Lake Reservoir", fish_group_2="Adult SNS-KLS", year=yr),
    parse_pred(row_vals$CLR_Juveniles) %>% mutate(location_1="Clear Lake Reservoir", fish_group_2="Juveniles", year=yr)
  )
})


estimates_suckers_clean <- estimates_suckers_clean |>
group_by(location_1, fish_group_2, year) |>
  summarise(est_pct = suppressWarnings(max(est_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            lcl_pct   = suppressWarnings(max(lcl_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            ucl_pct   = suppressWarnings(max(ucl_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            .groups = "drop")


# save clean data -  Estimates of predation rates (95% credible intervals) on PIT-tagged Lost River suckers (LRS),
# Shortnose suckers (SNS), Klamath Largescale suckers (KLS), Shortnose/Klamath Largescale suckers (SNS-KLS), and wild juvenile suckers by piscivorous colonial waterbirds nesting at colonies in Upper Klamath
# Lake and Clear Lake Reservoir (i.e., cumulative predation effects)
#TODO
# usethis::use_data(pikc_a_name, overwrite = TRUE)


 #### Estimates of predation rates PIT-tagged Sucker Assisted Rearing Program (SARP) juvenile suckers ----
 estimate_predation_sarp_raw <- tables[[4]] |> glimpse()

 estimate_predation_sarp_raw <- as.data.frame(estimate_predation_sarp_raw, stringsAsFactors = FALSE) |>
   clean_names()
