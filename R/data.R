#' @title Modeled Fisheries Data
#' @name fisheries_model_estimates
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific. This is a key dataset!
#' Data exploration was done in R Markdowns which also contain information on data source.
#' {Modeled Fisheries Data R Markdown}{https://github.com/Klamath-SDM/KlamathEDA/blob/main/data-raw/fisheries/modeled/modeled-fisheries-data.md}
#' @format A tibble with 1051 rows and 14 columns
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
#'   \item \code{source}: describes where data were sourced from. Currently there are 4 sources: (1) California Department of Fish and Wildlife (CDFW) document library (https://www.nrm.dfg.ca.gov/documents/ContextDocs.aspx?cat=Fisheries--AnadromousSalmonidPopulationMonitoring), (2) CDFW Megatable (https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment), (3) Gough, S. A., C. Z. Romberger, and N. A. Som. 2018. Fall Chinook Salmon Run Characteristics and Escapement in the Mainstem Klamath River below Iron Gate Dam, 2017. U.S. Fish and Wildlife Service. Arcata Fish and Wildlife Office, Arcata Fisheries Data Series Report Number DS 2018–58, Arcata, California (https://www.fws.gov/sites/default/files/documents/2017%20klamath%20spawn%20survey%20report%202017%20FINAL1.pdf), (4) Hewitt, D.A., Janney, E.C., Hayes, B.S., and Harris, A.C., 2018, Status and trends of adult Lost River (Deltistes luxatus) and shortnose (Chasmistes brevirostris) sucker populations in Upper Klamath Lake, Oregon, 2017: U.S. Geological Survey Open-File Report 2018-1064, 31 p., https://doi.org/10.3133/ofr20181064. (https://pubs.usgs.gov/of/2018/1064/ofr20181064.pdf)
#'   }
'fisheries_model_estimates'

#' @title Data Location Lookup
#' @name data_location_lookup
#' @description This dataset compiles location data relevant to fisheries data
#' collection efforts from across the Klamath Basin. This dataset is used to map
#' data collection locations.
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
'data_location_lookup'

#' @title Spawner Escapement
#' @name spawner_escapement
#' @description Digital version of [CDFW's Spring Chinook Salmon Megatable](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment) and [CDFW's Spring Chinook Salmon Megatable](https://casalmon.org/wp-content/uploads/2024/08/FINAL-2023-Spring-Chinook-Megatable-v.28-Mar-2024.pdf) Spawner Escapement sections
#' @format A tibble with 1,722 rows and 6 columns
#' \itemize{
#'   \item \code{location}: River, hatchery, or basin where escapement is reported
#'   \item \code{year}: Return year of the escapement estimate/count
#'   \item \code{species}: Species name (e.g., spring chinook salmon, fall chinook salmon)
#'   \item \code{origin}: Fish origin category (e.g., "hatchery", "wild")
#'   \item \code{lifestage}: Life stage reported (e.g., "adults", "grilse")
#'   \item \code{value}: Escapement count
#'   }
'spawner_escapement'

#' @title Predation Estimates on SARP and Chinook
#' @name avian_predation_estimates
#' @description This dataset combines avian predation rate estimates (with 95% credible intervals) and PIT-tag availability and recovery data for juvenile fishes in the Upper Klamath Basin. It includes PIT-tagged SARP suckers, hatchery-reared juvenile Chinook Salmon, and wild juvenile suckers released between 2021 and 2023.
#' This dataset corresponds to **Table 2**, **Table 3**, **Table 4**  of the 2021–2023 summary report.
#' @format A tibble with 39 rows and 16 columns
#' \itemize{
#'   \item{location}: Waterbody (upper klamath lake, clear lake reservoir, sheepy lake)
#'   \item{species}: Fish species (e.g. sucker, chinook salmon)
#'   \item{life_stage}: Fish life stage (e.g. juvenile)
#'   \item{origin}: Origin of realeased fish (e.g. hatchery, wild)
#'   \item{release_season}: Season when fish were realeased (e.g. spring_summer, fall_winter)
#'   \item{sarp_program}: Whether fish are part of the Sucker Assisted Rearing Program (SARP) (e.g. TRUE, FALSE)
#'   \item{year}: Year (2021–2023)
#'   \item{percent_estimate}: Estimated predation rate (% of available fish consumed)
#'   \item{lower_ci_pct}: Lower 95% credible interval
#'   \item{upper_ci_pct}: Upper 95% credible interval
#'   \item{number_adults_tagged}: Number of PIT-tagged fish available to predators
#'   \item{number_recovered_tags}: Number of PIT-tags recovered on piscivorous waterbird colonies during the 2021–2023 breeding seasons
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"avian_predation_estimates"
