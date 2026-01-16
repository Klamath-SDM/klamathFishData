library(pdftools)
library(dplyr)
library(tidyr)
library(tidyverse)
library(janitor)
# options(java.parameters = "-Xmx600m")
library(tabulapdf)
library(tidyverse)
library(purrr)

## 2021-2023 DATA
pdf_path <- "data-raw/Avian_Predation_on_UKB_Suckers_Summary_Report_2021_2023.pdf"
tables <- extract_tables(pdf_path, method = "stream", output = "tibble")


## 2024 DATA
pdf_path_2024 <- "data-raw/Avian-Predation-Final-Summary-Report_2024.pdf"
tables_2024 <- extract_tables(pdf_path_2024, method = "stream", output = "tibble")


#### ======= PIT-tagged LRS, SNS, KLS, SNS-KLS and SARP ===== ####
# The first number = the number of PIT-tagged fish available to avian predators that year in that location.
# This is essentially how many tagged suckers (or salmon) were released or present in the waterbody.
# The number in parentheses = the number of those tags recovered from avian predator nesting colonies in the same year.
# Recovery = physical detection of PIT tags on bird colonies (e.g., cormorants, pelicans) after birds consumed tagged fish.
# Importantly, these are minimum counts of predation events — not corrected for imperfect detection or deposition, so the true number of fish eaten could be higher.

#  code below reads table 2 of 2021-2023 report, and table 2 of 2024 report

# (1) read in 2021-2023 report table 2
tag_data_raw <- tables[[2]] |> glimpse()

tag_data_raw <- as.data.frame(tag_data_raw, stringsAsFactors = FALSE) %>%
  clean_names()

# (2) read in 2024 report table 2
tag_data_raw_2 <- tables_2024[[2]] |> glimpse()

tag_data_raw_2_clean <- tag_data_raw_2 |>
  separate(
    col = no_recovered_on_bird_colonies,
    into = c("recovered_CL", "recovered_SL", "recovered_KR"),
    sep = "\\s{1,}",
    fill = "right",
    extra = "merge") |>
  glimpse()

# TODO (3)
# 2024 looks slightly different than 2021-2023 so combining and re-running code below won't work
# 2024 table needs to be cleaned and combined with 2021-2023

# (4) functions to clean 2021-2023 table
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

# TODO combine with 2024 once cleaned
# (4) cleaned and NOT combined predation_estimates_avian_pit_tag
predation_estimates_avian_pit_tag <- avian_predation_pit_tag |>
  mutate(species = case_when(str_detect(fish_group, "LRS") ~ "lost river sucker",
                             str_detect(fish_group, "SNS-KLS") ~ "shortnose and klamath largescale suckers",
                             str_detect(fish_group, "SNS") ~ "shortnose sucker",
                             str_detect(fish_group, "KLS") ~ "klamath largescale sucker",
                             str_detect(fish_group, "Chinook") ~ "chinook salmon",
                             str_detect(fish_group, "Juvenile suckers") ~ "sucker",
                             str_detect(fish_group, "Juvenile sucker") ~ "sucker",
                             str_detect(fish_group, "SARP–Spr/Sum") ~ "sucker",
                             TRUE ~ NA_character_),
         life_stage = if_else(str_detect(fish_group, "Adult"), "adult", "juvenile"),
         origin = case_when(str_detect(fish_group, "Wild") ~ "wild",
                            str_detect(fish_group, "SARP") | str_detect(fish_group, "Chinook") ~ "hatchery",
                            TRUE ~ "wild"),
         release_season = case_when(str_detect(fish_group, "Spr/Sum") ~ "spring_summer",
                                    str_detect(fish_group, "Fall/Win") ~ "fall_winter",
                                    TRUE ~ NA_character_),
         sarp_program = case_when(str_detect(fish_group, "SARP") ~ TRUE,
                                  TRUE ~ FALSE),
         location = tolower(location)) |>
  select(-fish_group) |>
  relocate(species, life_stage, origin, release_season, sarp_program, .after = location) |>
  mutate(sarp_program = as.character(sarp_program),
         number_adults_tagged = available,
         number_recovered_tags = recovered) |>
  select(-available, -recovered) |>
  glimpse()

#### ======= estimate predation rates - LRS, SNS, KLS, SNS-KLS and SARP (UKL and Clear Lake) ===== ####

# Estimates of predation rates (95% credible intervals) on PIT-tagged Lost River suckers (LRS),
# Shortnose suckers (SNS), Klamath Largescale suckers (KLS), Shortnose/Klamath Largescale suckers (SNS-KLS), and wild juvenile suckers by piscivorous colonial waterbirds nesting at colonies in Upper Klamath
# Lake and Clear Lake Reservoir (i.e., cumulative predation effects)

#  code below reads table 3 of 2021-2023 report, and table 3 of 2024 report

# (1) read in 2021-2023 report table 3
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

# (2) read in 2024 report table 3
# using tabulizer::extract_areas() since original method is skipping this page - note that table needs to be selected
area_table <- extract_areas(pdf_path_2024, pages = 12)

estimate_predation_raw_2024 <- as.data.frame(area_table, stringsAsFactors = FALSE) |>
  clean_names()

est_predation_clean <- estimate_predation_raw_2024 |>
  mutate(
    upper_klamath_lake_1 = str_squish(upper_klamath_lake_1),
    upper_klamath_lake_2 = str_squish(clear_lake_reservoir)) |>
  separate(
    col = upper_klamath_lake_1,
    into = c("ukl_col2_part1", "ukl_col2_part2", "ukl_col2_part3"),
    sep = "\\s{1,}",
    fill = "right",
    extra = "merge") |>
  separate(
    col = upper_klamath_lake_2,
    into = c("cl_part1", "cl_part2", "cl_part3"),
    sep = "\\s{1,}",
    fill = "right",
    extra = "merge")

pred_raw_clean_2 <- est_predation_clean |>
  select(1:4, 6:7) |>
  slice(2:4) |>
  rename(...1 = x1)

names(pred_raw_clean_2)[2:4] <- c("UKL_Adult_LRS", "UKL_Adult_KLS", "UKL_Adult_SNS")
names(est_predation_clean)[5:6] <- c("CLR_Adult_LRS", "CLR_Adult_SNS_KLS")


pred_raw_clean_2 <- pred_raw_clean_2 |>
  mutate(across(starts_with("UKL_Adult_") | starts_with("CLR_Adult_"), str_trim))

# (3) combine 2024 with 2021-2023 to continue cleaning
est_predation_clean <- bind_rows(pred_raw_clean, pred_raw_clean_2)


# Row indices where the years are listed
year_map <- tibble(row_index = c(4, 6, 8, 9, 10, 12, 13, 15),
                   year = c(2021, 2021, 2022, 2022, 2023, 2023, 2024, 2024))

# parse "estimate (lcl–ucl)"
parse_pred <- function(x) {
  if (is.na(x) || x %in% c("NA","–")) {
    return(tibble(estimate_pct = NA_character_, lower_ci_pct = NA_real_, upper_ci_pct = NA_real_))
    }

  # keep "<"
  x_clean <- str_remove_all(x, "[()%]")

  # range (CI)
  if (str_detect(x_clean, "–")) {
    rng <- str_split(x_clean, "–")[[1]] |>  str_trim()
    tibble(estimate_pct = NA_character_,
           lower_ci_pct = suppressWarnings(as.numeric(str_remove(rng[1], "%"))),
           upper_ci_pct = suppressWarnings(as.numeric(str_remove(rng[2], "%"))))

    # explicit "<" value (e.g. "< 0.1%")
    } else if (str_detect(x_clean, "<")) {
      tibble(estimate_pct = str_trim(x_clean),  # keep as character, e.g. "<0.1%"
             lower_ci_pct = NA_real_,
             upper_ci_pct = NA_real_)

      # normal percentage
      } else {
        tibble(estimate_pct = str_trim(str_remove(x_clean, "%")),
               lower_ci_pct = NA_real_,
               upper_ci_pct = NA_real_)
      }
}

# build table
estimates_suckers_clean <- map2_dfr(year_map$row_index, year_map$year, function(i, yr){
  row_vals <- est_predation_clean[i, ]
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


predation_estimates_wild_clean <- estimates_suckers_clean |>
group_by(location_1, fish_group_2, year) |>
  summarise(estimate_pct = suppressWarnings(max(estimate_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            lower_ci_pct   = suppressWarnings(max(lower_ci_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            upper_ci_pct   = suppressWarnings(max(upper_ci_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
            .groups = "drop") |>
  rename(location = location_1,
         fish_group = fish_group_2) |>
  glimpse()

# (4) clean and combined predation_estimates_wild
predation_estimates_wild <- predation_estimates_wild_clean |>
  mutate(
  species = case_when(
    str_detect(fish_group, "LRS") ~ "lost river sucker",
    str_detect(fish_group, "SNS-KLS") ~ "shortnose and klamath largescale suckers",
    str_detect(fish_group, "SNS") ~ "shortnose sucker",
    str_detect(fish_group, "KLS") ~ "klamath largescale sucker",
    str_detect(fish_group, "Juvenile") ~ "sucker",
    TRUE ~ NA_character_),
  origin = "wild",
  life_stage = if_else(str_detect(fish_group, "Adult"), "adult", "juvenile"),
  location = tolower(location)) |>
  select(-fish_group) |>
  relocate(species, life_stage, origin, .after = location) |> glimpse()


#### ======= estimates of predation rates PIT-tagged Sucker Assisted Rearing Program (SARP) juvenile suckers  ===== ####

#  code below reads table 4 of 2021-2023 report, and table 4 of 2024 report

# (1) read in 2021-2023 report table 3
 estimate_predation_sarp_raw <- tables[[4]] |> glimpse()

 estimate_predation_sarp_raw <- as.data.frame(estimate_predation_sarp_raw, stringsAsFactors = FALSE) |>
   clean_names()

 estimate_predation_sarp <- estimate_predation_sarp_raw |>
   separate(col = 'upper_klamath_lake', # split the mixed column into two
            into = c("ukl_sarp_fall_win", "ukl_chin_spr_sum"),
            sep = "(?<=\\)|%)\\s+",
            extra = "merge",
            fill = "right") |>
   rename(ukl_chin_fall_win = x4,
          kr_chin_spr_sum = klamath_river,
          shpy_sarp_spr_sum = sheepy_lake,
          ukl_sarp_spr_sum = x2) |>
   glimpse()

 #TODO
# (2) read in 2024 report table 4
# area_table_estimate_sarp <- extract_areas(pdf_path_2024, pages = 16)
#
# estimate_predation_sarp_raw_2024 <- as.data.frame(area_table_estimate_sarp, stringsAsFactors = FALSE) |>
#    clean_names()

 # TODO (3)
 # 2024 looks slightly different than 2021-2023
 # 2024 table needs to be cleaned and combined with 2021-2023


# (4) functions to clean 2021-2023 table
 year_map_2 <- tibble(row_index = c(4, 6, 7, 9, 10, 12),
                    year = c(2021, 2021, 2022, 2022, 2023, 2023))


 # parse "estimate (lcl–ucl)"
 parse_sarp <- function(x) {
   if (is.na(x) || x %in% c("NA","–")) {
     return(tibble(estimate_pct = NA_character_, lower_ci_pct = NA_real_, upper_ci_pct = NA_real_))
   }

   # keep "<"
   x_clean <- str_remove_all(x, "[()%]")

   # range (CI)
   if (str_detect(x_clean, "–")) {
     rng <- str_split(x_clean, "–")[[1]] |>  str_trim()
     tibble(estimate_pct = NA_character_,
            lower_ci_pct = suppressWarnings(as.numeric(str_remove(rng[1], "%"))),
            upper_ci_pct = suppressWarnings(as.numeric(str_remove(rng[2], "%"))))

     # explicit "<" value (e.g. "< 0.1%")
   } else if (str_detect(x_clean, "<")) {
     tibble(estimate_pct = str_trim(x_clean),  # keep as character, e.g. "<0.1%"
            lower_ci_pct = NA_real_,
            upper_ci_pct = NA_real_)

     # normal percentage
   } else {
     tibble(estimate_pct = str_trim(str_remove(x_clean, "%")),
            lower_ci_pct = NA_real_,
            upper_ci_pct = NA_real_)
   }
 }

 # build table
 estimate_predation_sarp_clean <- map2_dfr(year_map_2$row_index, year_map_2$year, function(i, yr){
   row_vals <- estimate_predation_sarp[i, ]
   # Each column corresponds to one group
   bind_rows(
     parse_sarp(row_vals$ukl_sarp_spr_sum) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="SARP Spring/Summer", year=yr),
     parse_sarp(row_vals$ukl_sarp_fall_win) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="SARP Fall/Winter", year=yr),
     parse_sarp(row_vals$ukl_chin_spr_sum) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Chinook Spring/Summer", year=yr),
     parse_sarp(row_vals$ukl_chin_fall_win) %>% mutate(location_1="Upper Klamath Lake", fish_group_2="Chinook Fall/Winter", year=yr),
     parse_sarp(row_vals$kr_chin_spr_sum) %>% mutate(location_1="Klamath River", fish_group_2="Chinook Spring/Summer", year=yr),
     parse_sarp(row_vals$shpy_sarp_spr_sum) %>% mutate(location_1="Sheepy Lake", fish_group_2="SARP Spring/Summer", year=yr)
   )
 })


 estimate_predation_sarp <- estimate_predation_sarp_clean |>
   group_by(location_1, fish_group_2, year) |>
   summarise(estimate_pct = suppressWarnings(max(estimate_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
             lower_ci_pct   = suppressWarnings(max(lower_ci_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
             upper_ci_pct   = suppressWarnings(max(upper_ci_pct, na.rm = TRUE)) %>% { ifelse(is.infinite(.), NA_real_, .) },
             .groups = "drop") |>
   rename(location = location_1,
          fish_group = fish_group_2)

 # TODO
 # (5) Combine 2024 with 2021-2023 table and produce clean and combined predation_estimates_hatchery
predation_estimates_hatchery <- estimate_predation_sarp |>
  mutate(
   species = case_when(
     str_detect(fish_group, "SARP") ~ "sucker",
     str_detect(fish_group, "Chinook") ~ "chinook salmon",
     TRUE ~ NA_character_),
   life_stage = "juvenile",
   origin = "hatchery",
   release_season = case_when(
     str_detect(fish_group, "Spring/Summer") ~ "spring_summer",
     str_detect(fish_group, "Fall/Win") ~ "fall_winter",
     TRUE ~ NA_character_
   ),
   sarp_program = case_when(
     str_detect(fish_group, "SARP") ~ TRUE,
     TRUE ~ FALSE),
   location = tolower(location)) |>
   select(-fish_group) |>
   relocate(species, life_stage, origin, release_season, sarp_program, .after = location) |> glimpse()


#### ======= harmonize and combine all tables  ===== ####
#  predation_estimates_hatchery, predation_estimates_wild and predation_estimates_avian_pit_tag
## combine ----

# harmonize hatchery
hatchery_long <- predation_estimates_hatchery |>
  mutate(metric_type = "percent",
         available = NA_integer_,
         recovered = NA_integer_,
         percent_estimate = estimate_pct,
         sarp_program = as.character(sarp_program)) |>
  select(location, species, life_stage, origin, release_season, sarp_program, year, metric_type,
         percent_estimate, lower_ci_pct, upper_ci_pct) |> glimpse()

# harmonize wild
wild_long<- predation_estimates_wild |> # this table is ready to be combined with others
  mutate(metric_type = "percent",
         release_season = NA_character_,
         # # origin = "Natural",
         # available = NA_integer_,
         # recovered = NA_integer_,
         percent_estimate = estimate_pct,
         sarp_program = as.character(FALSE)) |>
  select(location, species, life_stage, origin, release_season, sarp_program, year, metric_type,
         percent_estimate, lower_ci_pct, upper_ci_pct) |> glimpse()

#combine hatchery and wild
percent_estimates <- bind_rows(wild_long, hatchery_long) |>
  # mutate(percent_estimate = value) |>
  select(-metric_type) |>
         glimpse()

# combine percent estimates and count
avian_predation_estimates <- percent_estimates |>
  left_join(predation_estimates_avian_pit_tag,
            by = c("location",
                   "species",
                   "life_stage",
                   "origin",
                   "release_season",
                   "sarp_program",
                   "year")) |> glimpse()
# save clean data
# usethis::use_data(avian_predation_estimates, overwrite = TRUE)

