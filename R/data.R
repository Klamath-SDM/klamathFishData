#' @title Salmon Juvenile Abundance
#' @name salmon_juvenile_abundance
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific. This is a key dataset!
#' In the future, this dataset may be broken down into separate tables like what
#' was done for spawner escapement.
#' @format A tibble with 122 rows and 14 columns
#' \itemize{
#'   \item \code{julian_year}: julian year (2001-2022). Min/max years vary by location, species, type of estimate. Juvenile abundance typically spans multiple years (e.g. Oct 2020 - May 2021) and would be assigned a julian year for the latter part of this period.
#'   \item \code{stream}: stream ("scott river", "shasta river")
#'   \item \code{species}: species (coho salmon, steelhead, fall chinook salmon)
#'   \item \code{origin}: origin (wild)
#'   \item \code{lifestage}: lifestage category (yoy, age 1+, age 2+, smolt)
#'   \item \code{sex}: sex, if applicable to model (NA)
#'   \item \code{estimate_type}: type of model estimate (abundance, count)
#'   \item \code{estimate}: value of estimate
#'   \item \code{confidence_interval}: type of confidence or credible interval (e.g., 95)
#'   \item \code{lower_bounds_estimate}: value of lower bounds estimate, if applicable
#'   \item \code{upper_bounds_estimate}: value of upper bounds estimate, if applicable
#'   \item \code{estimation_method}: type of model used (mark-recapture)
#'   \item \code{is_complete_estimate}: data included in model are complete. In some cases, it is known that an
#'   estimate is not complete, otherwise it is assumed that estimate is complete.
#'   \item \code{source}: describes where data were sourced from. Currently there is one data source: (1) California Department of Fish and Wildlife (CDFW) document library (https://www.nrm.dfg.ca.gov/documents/ContextDocs.aspx?cat=Fisheries--AnadromousSalmonidPopulationMonitoring)
#'   }
'salmon_juvenile_abundance'

#' @title Sucker Adult Estimates
#' @name sucker_adult_estimates
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific. This is a key dataset!
#' In the future, this dataset may be broken down into separate tables like what
#' was done for spawner escapement.
#' @format A tibble with 265 rows and 13 columns
#' \itemize{
#'   \item \code{julian_year}: julian year (1999-2022). Min/max years vary by location, species, type of estimate.
#'   \item \code{stream}: stream ("upper klamath lake")
#'   \item \code{species}: species (lost river sucker lakeshore spawning, lost river sucker river spawning, shortnose sucker)
#'   \item \code{lifestage}: lifestage category (adult)
#'   \item \code{sex}: sex, if applicable to model (female, male, NA)
#'   \item \code{estimate_type}: type of model estimate (apparent survival, seniority probability, annual population rate of change)
#'   \item \code{estimate}: value of estimate
#'   \item \code{confidence_interval}: type of confidence or credible interval (e.g., 80, 95)
#'   \item \code{lower_bounds_estimate}: value of lower bounds estimate, if applicable
#'   \item \code{upper_bounds_estimate}: value of upper bounds estimate, if applicable
#'   \item \code{estimation_method}: type of model used (Cormack-Jolly-Seber model)
#'   \item \code{is_complete_estimate}: data included in model are complete. In some cases, it is known that an
#'   estimate is not complete, otherwise it is assumed that estimate is complete.
#'   \item \code{source}: describes where data were sourced from. Currently there is one source: Hewitt, D.A., Janney, E.C., Hayes, B.S., and Harris, A.C., 2018, Status and trends of adult Lost River (Deltistes luxatus) and shortnose (Chasmistes brevirostris) sucker populations in Upper Klamath Lake, Oregon, 2017: U.S. Geological Survey Open-File Report 2018-1064, 31 p., https://doi.org/10.3133/ofr20181064. (https://pubs.usgs.gov/of/2018/1064/ofr20181064.pdf)
#'   }
'sucker_adult_estimates'

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

#' @title Salmon Spawner Escapement
#' @name spawner_escapement
#' @description Digital version of [CDFW's Spring Chinook Salmon Megatable](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment) and [CDFW's Spring Chinook Salmon Megatable](https://casalmon.org/wp-content/uploads/2024/08/FINAL-2023-Spring-Chinook-Megatable-v.28-Mar-2024.pdf) Spawner Escapement sections as well as other data sources for Coho and Steelhead not included in the Megatable.
#' @format A tibble with 1,722 rows and 6 columns
#' \itemize{
#'   \item \code{location}: River, hatchery, or basin where escapement is reported. ("bogus creek", "hoopa and yurok tributaries", "hoopa trinity tributaries", "iron gate hatchery", "klamath basin", "klamath river", "lower klamath river", "other klamath tributaries", "other klamath trinity tributaries", "other trinity tributaries", "salmon river", "scott river", "shasta river", "south fork trinity river", "trinity basin", "trinity river", "trinity river hatchery", "yurok klamath tributaries")
#'   \item \code{year}: Return year of the escapement estimate/count (1978-2024)
#'   \item \code{species}: Species name (spring chinook salmon, fall chinook salmon, coho salmon, winter steelhead, steelhead)
#'   \item \code{origin}: Fish origin category (hatchery, wild, mixed, unknown)
#'   \item \code{lifestage}: Life stage reported (adult, adult and subadult, grilse). In a few cases adult and subadult were not distinguished and are grouped together.
#'   \item \code{estimate_type}: Type of estimate reported (abundance, count, redd abundance). Work in progress as these have not been filled in yet for the data from the Megatables.
#'   \item \code{estimate}: value of estimate
#'   \item \code{confidence_interval}: type of confidence or credible interval (e.g., 95)
#'   \item \code{lower_bounds_estimate}: value of lower bounds estimate, if applicable
#'   \item \code{upper_bounds_estimate}: value of upper bounds estimate, if applicable
#'   \item \code{estimation_method}: type of model used (count, net upstream count, upstream count, expanded redd estimate, mark recapture. Work in progress as these have not been filled in yet for the data from the Megatables.)
#'   \item \code{is_complete_estimate}: data included in model are complete. In some cases, it is known that an
#'   estimate is not complete, otherwise it is assumed that estimate is complete.
#'   \item \code{source}: describes where data were sourced from. Currently there are 2 sources: (1) California Department of Fish and Wildlife (CDFW) document library (https://www.nrm.dfg.ca.gov/documents/ContextDocs.aspx?cat=Fisheries--AnadromousSalmonidPopulationMonitoring), (2) CDFW Spring and Fall Megatables
#'   }
'salmon_spawner_escapement'

#' @title Predation Estimates on SARP and Chinook
#' @name avian_predation_estimates
#' @description This dataset combines avian predation rate estimates (with 95% credible intervals) and PIT-tag availability and recovery data for juvenile fishes in the Upper Klamath Basin. It includes PIT-tagged SARP suckers, hatchery-reared juvenile Chinook Salmon, and wild juvenile suckers released between 2021 and 2023.
#' This dataset corresponds to **Table 2**, **Table 3**, **Table 4**  of the *Avian Predation on Upper Klamath Basin Suckers: Summary Report 2021–2023*.
#' This dataset compiles avian predation estimates alongside PIT‑tag availability and recovery information for juvenile
#' fishes released in the Upper Klamath Basin between 2021 and 2023. It includes PIT‑tagged Sucker Assisted Rearing Program (SARP)
#' juvenile suckers, wild juvenile suckers, and hatchery‑reared juvenile Chinook Salmon.
#'
#' @format A tibble with 39 rows and 16 columns
#' \itemize{
#'   \item{location}: Waterbody or colony area where piscivorous waterbirds nested and where PIT tags were recovered (upper klamath lake, clear lake reservoir, sheepy lake)
#'   \item{species}: Fish species associated with each release group (e.g. sucker, chinook salmon)
#'   \item{life_stage}: Fish life stage (e.g. juvenile, adult)
#'   \item{origin}: Origin of realeased fish (e.g. hatchery, wild)
#'   \item{release_season}: Season when fish were realeased (e.g. spring_summer, fall_winter)
#'   \item{sarp_program}: Whether fish are part of the Sucker Assisted Rearing Program (SARP) (e.g. TRUE, FALSE)
#'   \item{year}: Year (2021–2023)
#'   \item{percent_estimate}: Estimated predation rate (% of available fish consumed). This is a statistically adjusted percentage that accounts for the probability of a bird depositing a tag on a colony and the probability of a
#'   researcher successfully detecting that tag. Predation estimates are adjusted to account for PIT tag detection and deposition probabilities that were unique to each predator species, colony, and year
#'   \item{lower_ci_pct}: Lower 95% credible interval. This interval provides the range in which the true predation rate is likely to fall. Predation rates are estimates derived from a hierarchical Bayesian model.
#'   \item{upper_ci_pct}: Upper 95% credible interval. This interval provides the range in which the true predation rate is likely to fall. Predation rates are estimates derived from a hierarchical Bayesian model.
#'   \item{number_adults_tagged}: Total number of PIT-tagged fish (both adults and juveniles) present in the study area and considered "available" to avian predators during the specified year
#'   \item{number_recovered_tags}: Raw count of physical PIT-tags recovered on piscivorous waterbird colonies during the 2021–2023 breeding seasons.
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://birdresearchnw.org/brn-publications/avian-predation-on-ukb-suckers-summary-report-2021-2023/)
"avian_predation_estimates"

#' @title Annual Summary of Hatchery
#' @name KFNFH_annual_summary
#' @description Summary of collections and releases from the Klamath Falls National Fish Hatchery since its inception in 2016. Data summarizes the annual inputs and outputs of the hatchery program by federal fiscal year
#' Corresponds to **Table 1** and **Table 4** in the FY2024 KFNFH Annual Report.
#' @format A tibble with 9 rows and 12 columns
#' \itemize{
#'   \item \code{hatchery_name}: name of hatchery (KFNFH)
#'   \item \code{fiscal_year}: fiscal year
#'   \item \code{larvae_collected}: number of wild larval suckers collected
#'   \item \code{sarp_release}: number of fish released or transferred that were raised in the Sucker Assisted Rearing Program (SARP)
#'   \item \code{sarp_tl_mm}: average total length (TL) of the SARP release fish, measured in millimeters (mm)
#'   \item \code{fingerling_release}: number of fingerlings released or transferred
#'   \item \code{fingerling_tl_mm}: average total length (TL) of the fingerling release fish, measured in millimeters (mm)
#'   \item \code{fry_release}: number of fry released or transferred. This includes fry produced from experimental, wild-spawning activities that were transferred to other facilities (e.g., the Klamath Tribes Hatchery in FY24)
#'   \item \code{fry_tl_mm }: average total length (TL) of the  fry release fish, measured in millimeters (mm)
#'   \item \code{salvage_release}:  number of suckers salvaged throughout the year
#'   \item \code{salvage_sl_mm}: average length of release fish, measured in millimeters (mm)
#'   \item \code{salvage_tl_mm}: standard lengths (SL) of salvage fish are averages from measurements for each year
#'   \item \code{primary_collection_method}: drift net, dip net, or both
#'   }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_annual_summary"

#' @title Sucker Juvenile Survival
#' @name sucker_juvenile_survival
#' @description This dataset compiles modeled estimates of juvenile sucker survival
#' for both age-0 and age-0 to age-1 for Upper Klamath Lake from 2015-2022. These data
#' are from Martin et al. 2024 (https://doi.org/10.3133/ofr20241013)
#' @format A tibble with 30 rows and 6 columns
#' \itemize{
#'   \item \code{taxa}: taxa (lost river suckers, shortnose suckers/klamath largescale suckers/SNS/KLS)
#'   \item \code{location}: location of suckers (upper klamath lake)
#'   \item \code{year}: julian year (2015-2022)
#'   \item \code{age}: age of suckers (age-0 or age-0-age-1)
#'   \item \code{survival_indice_type}: type of survival indice estimate (august to september and september to june)
#'   \item \code{value}: value of survival estimate
#'   }
#' @source [Growth, Survival, and Cohort Formation of Juvenile Lost River (Deltistes luxatus) and Shortnose Suckers (Chasmistes brevirostris) in Upper Klamath Lake, Oregon, and Clear Lake Reservoir, California—2021–22 Monitoring Report](https://doi.org/10.3133/ofr20241013)
'sucker_juvenile_survival'
