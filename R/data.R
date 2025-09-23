#' @title Modeled Fisheries Data
#' @name fisheries_model_estimates
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific.
#' Data exploration was done in R Markdowns which also contain information on data source.
#' {Modeled Fisheries Data R Markdown}{https://github.com/Klamath-SDM/KlamathEDA/blob/main/data-raw/fisheries/modeled/modeled-fisheries-data.md}
#' @format A tibble with 1034 rows and 14 columns
#' \itemize{
#'   \item \code{julian_year}: julian year. Min/max years vary by location, species, type of estimate. Adult escapement is reported for a brood year which is the same as the julian year. Juvenile abundance typically spans multiple years (e.g. Oct 2020 - May 2021) and would be assigned a julian year for the latter part of this period.
#'   \item \code{stream}: stream ("bogus creek", "iron gate hatchery  (igh)", "klamath river", "lower klamath river", "other tributaries", "salmon river", "scott river", "shasta river", "trinity river", "trinity river hatchery (trh)", "upper klamath lake", "yurok and hoopa reservation tribs")
#'   \item \code{species}: species (coho salmon, winter steelhead, steelhead, fall chinook salmon, spring chinook salmon, lost river lakeshore spawning, lost river river spawning, shortnose)
#'   \item \code{origin}: origin (natural, hatchery, mixed, unknown)
#'   \item \code{lifestage}: lifestage category (parr, smolts, adult, yoy, age 1+, age 2+, adult and subadult, spawner)
#'   \item \code{sex}: sex, if applicable to model (female, male, NA)
#'   \item \code{estimate_type}: type of model estimate (abundance, redd abundance, count, apparent survival, seniority probability, annual population rate of change)
#'   \item \code{estimate}: value of estimate
#'   \item \code{confidence_interval}: type of confidence or credible interval (e.g., 80, 95)
#'   \item \code{lower_bounds_estimate}: value of lower bounds estimate, if applicable
#'   \item \code{upper_bounds_estimate}: value of upper bounds estimate, if applicable
#'   \item \code{estimation_method}: type of model used
#'   \item \code{is_complete_estimate}: data included in model are complete. In some cases, it is known that an
#'   estimate is not complete, otherwise it is assumed that estimate is complete.
#'   \item \code{source}: describes where data were sourced from.
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

