#' @title Modeled Fisheries Data
#' @name fisheries_model_estimates
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific.
#' Data exploration was done in R Markdowns which also contain information on data source.
#' {Modeled Fisheries Data R Markdown}{https://github.com/Klamath-SDM/KlamathEDA/blob/main/data-raw/fisheries/modeled/modeled-fisheries-data.md}
#' @format A tibble with 127 rows and 12 columns
#' \itemize{
#'   \item \code{julian_year}: julian year
#'   \item \code{stream}: stream (INSERT LINK TO DOCUMENTATION for location here)
#'   \item \code{species}: species (coho, lost river lakeshore spawning, lost river river spawning, shortnose)
#'   \item \code{lifestage}: lifestage category (parr, smolts, adult)
#'   \item \code{sex}: sex, if applicable to model (female, male, NA)
#'   \item \code{estimate_type}: type of model estimate (abundance, apparent survival, seniority probability, annual population rate of change)
#'   \item \code{estimate}: value of estimate
#'   \item \code{confidence_interval}: type of confidence or credible interval (e.g., 80, 95)
#'   \item \code{lower_bounds_estimate}: value of lower bounds estimate, if applicable
#'   \item \code{upper_bounds_estimate}: value of upper bounds estimate, if applicable
#'   \item \code{estimation_method}: type of model used
#'   \item \code{is_complete_estimate}: data included in model are complete. In some cases, it is known that an
#'   estimate is not complete, otherwise it is assumed that estimate is complete.
#'   }
'fisheries_model_estimates'

#' @title Fisheries Location Lookup
#' @name fisheries_location_lookup
#' @description This dataset compiles location data relevant to fisheries data collection efforts from across the Klamath Basin.
#' @format A tibble with 35 rows and 10 columns
#' \itemize{
#'   \item \code{stream}: stream
#'   \item \code{sub_basin}: sub-basin name (upper klamath, lower klamath, trinity, shasta)
#'   \item \code{data_type}: type of data (rst, hatchery, redd/carcass survey)
#'   \item \code{site_name}: name of site such as a RST site or hatchery location (big bar, shasta river, bogus, willow creek, pear creek, weitchpec, iron gate fish hatchery, trinity river hatchery, klamath hatchery)
#'   \item \code{agency}: agency that manages/monitors the site (arcata fwo, arcata fwo, cdfw, hoopa tribal fisheries department, karuk, yurok tribal fisheries program, usfws)
#'   \item \code{latitude}: longitude
#'   \item \code{longitude}: longitude
#'   \item \code{downstream_latitude}: Latitude of the downstream end of the feature's extent, applicable for survey extents
#'   \item \code{downstream_longitude}: Longitude of the downstream end of the feature's extent, applicable for survey extents
#'   \item \code{link}: web link containing more information about fisheries locations
#'   }
'fisheries_location_lookup'

#' Avian Predation PIT-tag Recoveries
#'
#' Data on the number of PIT-tagged fish that were available and subsequently
#' recovered on piscivorous waterbird colonies in the Upper Klamath Basin
#' during 2021–2023.
#'
#' This dataset corresponds to **Table 2** in the report:
#' *Avian Predation on Upper Klamath Basin Suckers, 2021–2023 Summary Report*.
#' It provides raw counts of tagged fish released and tags recovered, by species
#' group, year, and location.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{location}{Waterbody where releases occurred (Upper Klamath Lake, Clear Lake Reservoir, etc.)}
#'   \item{fish_group}{Fish group (e.g., Adult LRS, Adult SNS, Juvenile suckers (SARP–Spr/Sum), Chinook, etc.)}
#'   \item{year}{Year of release / monitoring (2021–2023)}
#'   \item{available}{Number of PIT-tagged fish available to predators}
#'   \item{recovered}{Number of PIT-tags recovered on piscivorous waterbird colonies during the 2021–2023 breeding seasons}
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"avian_predation_pit_tag"


#' Predation Estimates on Wild Suckers
#'
#' Estimates of predation rates (with 95% credible intervals) on PIT-tagged
#' wild suckers (Lost River, Shortnose, Klamath Largescale, SNS–KLS hybrids,
#' and wild juveniles) by piscivorous colonial waterbirds in the Upper Klamath
#' Basin.
#'
#' This dataset corresponds to **Table 3** of the 2021–2023 summary report.
#' Values are adjusted for PIT-tag detection and deposition probabilities.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{location}{Waterbody (Upper Klamath Lake, Clear Lake Reservoir)}
#'   \item{fish_group}{Fish group (Adult LRS, Adult SNS, Adult KLS, SNS–KLS, Wild Juveniles)}
#'   \item{year}{Year (2021–2023)}
#'   \item{estimate_pct}{Estimated predation rate (% of available fish consumed)}
#'   \item{lower_ci_pct}{Lower 95% credible interval}
#'   \item{upper_ci_pct}{Upper 95% credible interval}
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"predation_estimates_wild"


#' Predation Estimates on SARP and Chinook
#'
#' Estimates of predation rates (with 95% credible intervals) on PIT-tagged
#' Sucker Assisted Rearing Program (SARP) juvenile suckers and juvenile Chinook
#' Salmon released into the Upper Klamath Basin, Clear Lake Reservoir, and
#' Sheepy Lake.
#'
#' This dataset corresponds to **Table 4** of the 2021–2023 summary report.
#' Estimates are provided separately for fish released in spring/summer (Spr/Sum)
#' and fall/winter (Fall/Win).
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{location}{Waterbody (Upper Klamath Lake, Clear Lake Reservoir, Sheepy Lake)}
#'   \item{fish_group}{Fish group (SARP–Spr/Sum, SARP–Fall/Win, Chinook–Spr/Sum, Chinook–Fall/Win)}
#'   \item{year}{Year (2021–2023)}
#'   \item{estimate_pct}{Estimated predation rate (% of available fish consumed)}
#'   \item{lower_ci_pct}{Lower 95% credible interval}
#'   \item{upper_ci_pct}{Upper 95% credible interval}
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"estimate_predation_sarp"


