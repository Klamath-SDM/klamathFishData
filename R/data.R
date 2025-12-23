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

#' @title Megatable - IN DEVELOPMNENT
#' @name megatable
#' @description Digital version of [CDFW's Megatable](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment)
#' @format A tibble with 4077 rows and 7 columns
#' \itemize{
#'   \item \code{location}:
#'   \item \code{subsection}:
#'   \item \code{section}:
#'   \item \code{species}:
#'   \item \code{lifestage}:
#'   \item \code{year}:
#'   \item \code{value}:
#'   }
'megatable'



#' @title Avian Predation PIT-tag Recoveries
#' @name predation_estimates_avian_pit_tag
#' @description Data on the number of PIT-tagged fish that were available and subsequently
#' recovered on piscivorous waterbird colonies in the Upper Klamath Basin
#' during 2021–2023.
#' This dataset corresponds to **Table 2** in the report:
#' *Avian Predation on Upper Klamath Basin Suckers, 2021–2023 Summary Report*.
#' It provides raw counts of tagged fish released and tags recovered, by species
#' group, year, and location.
#' @format A tibble with 39 rows and 9 columns
#' \itemize{
#'   \item{location}{Waterbody where releases occurred (upper klamath lake, clear lake reservoir, etc.)}
#'   \item{species}{Fish species (e.g., lost river sucker, shortnose and klamath largescale suckers, sucker juveniles, chinook salmon,
#'   sucker, klamath largescale sucker, shortnose sucker)}
#'   \item{life_stage}{Fish life stage (e.g. adult, juvenile)}
#'   \item{origin}{Origin of realeased fish (e.g. wild, hatchery)}
#'   \item{release_season}{Season when fish were realeased (e.g. spring_summer, fall_winter)}
#'   \item{sarp_program}{Wheather fish are part of the Sucker Assisted Rearing Program (SARP) (e.g. TRUE, FALSE)}
#'   \item{year}{Year of release / monitoring (2021–2023)}
#'   \item{available}{Number of PIT-tagged fish available to predators}
#'   \item{recovered}{Number of PIT-tags recovered on piscivorous waterbird colonies during the 2021–2023 breeding seasons}
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"predation_estimates_avian_pit_tag"


#' @title Avian Predation Estimates on Wild Suckers
#' @name predation_estimates_wild
#' @description Estimates of predation rates (with 95% credible intervals) on PIT-tagged
#' wild suckers (Lost River, Shortnose, Klamath Largescale, SNS–KLS hybrids,
#' and wild juveniles) by piscivorous colonial waterbirds in the Upper Klamath
#' Basin.
#' This dataset corresponds to **Table 3** of the 2021–2023 summary report.
#' Values are adjusted for PIT-tag detection and deposition probabilities.
#' @format A tibble with 21 rows and 8 columns
#' \itemize{
#'   \item{location}: Waterbody (upper klamath lake, clear lake reservoir)
#'   \item{species}: Fish species (e.g., lost river sucker, shortnose and klamath largescale suckers, sucker juveniles,
#'   sucker, klamath largescale sucker, shortnose sucker)
#'   \item{life_stage}: Fish life stage (e.g. adult, juvenile)
#'   \item{origin}: Origin of released fish (e.g. wild)
#'   \item{year}: Year (2021–2023)
#'   \item{estimate_pct}: Estimated predation rate (% of available fish consumed)
#'   \item{lower_ci_pct}: Lower 95% credible interval
#'   \item{upper_ci_pct}: Upper 95% credible interval
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"predation_estimates_wild"


#' @title Predation Estimates on SARP and Chinook
#' @name predation_estimates_hatchery
#' @description Estimates of predation rates (with 95% credible intervals) on PIT-tagged
#' Sucker Assisted Rearing Program (SARP) juvenile suckers and juvenile Chinook
#' Salmon released into the Upper Klamath Basin, Clear Lake Reservoir, and
#' Sheepy Lake.
#' This dataset corresponds to **Table 4** of the 2021–2023 summary report.
#' Estimates are provided separately for fish released in spring/summer (Spr/Sum)
#' and fall/winter (Fall/Win).
#' @format A tibble with 18 rows and 10 columns
#' \itemize{
#'   \item{location}: Waterbody (upper klamath lake, clear lake reservoir, sheepy lake)
#'   \item{species}: Fish species (e.g. sucker, chinook salmon)
#'   \item{life_stage}: Fish life stage (e.g. juvenile)
#'   \item{origin}: Origin of realeased fish (e.g. hatchery)
#'   \item{release_season}: Season when fish were realeased (e.g. spring_summer, fall_winter)
#'   \item{sarp_program}: Wheather fish are part of the Sucker Assisted Rearing Program (SARP) (e.g. TRUE, FALSE)
#'   \item{year}: Year (2021–2023)
#'   \item{estimate_pct}: Estimated predation rate (% of available fish consumed)
#'   \item{lower_ci_pct}: Lower 95% credible interval
#'   \item{upper_ci_pct}: Upper 95% credible interval
#' }
#'
#' @source Bird Research Northwest (2023)
#' [Avian Predation on UKB Suckers 2021–2023 Summary Report](https://www.birdresearchnw.org/Avian%20Predation%20on%20UKB%20Suckers_Summary%20Report%202021-2023.pdf)
"predation_estimates_hatchery"

#' @title Historical Hatchery Collection Releases
#' @name KFNFH_historical_collection_release
#' @description Collections and releases from the Klamath Falls National Fish Hatchery since its inception in 2016. Data summarizes the annual inputs and outputs of the hatchery program by federal fiscal year
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
"KFNFH_historical_collection_release"

#' @title Hatchery Distributions and Transfers, KFNFH FY2024
#' @name KFNFH_hatchery_release_2024
#' @description Summary of all fish distributions for repatriation or transfer from the Klamath Falls National Fish Hatchery during the fiscal year of 2024. A "distribution" is considered a stocking or repatriation event, while a "transfer" involves moving fish for temporary holding
#' Corresponds to **Table 11** in the FY2024 KFNFH Annual Report.
#' @format A tibble with 34 rows and 12 columns
#' \itemize{
#'   \item \code{hatchery_name}: name of hatchery (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2024)
#'   \item \code{stock_date}: date the fish distribution or transfer event occurred
#'   \item \code{species}: species of sucker being distributed (e.g. LRS, SNS, ESS LSR)
#'   \item \code{lot}: grouping or cohort of fish categorized primarily by their collection year (CY) or their operational origin
#'   \item \code{number_fish}: number of fish in that specific grouping (lot) that were distributed or transferred
#'   \item \code{weight_lb}: total weight of the fish lot being distributed, measured in pounds (lb)
#'   \item \code{actual_tl_mm}: actual total length (TL) of the fish, measured in millimeters (mm)
#'   \item \code{number_lb}: number of fish per pound for the specific lot being distributed
#'   \item \code{projected_tl_inches}: projected total length (TL) of the fish, measured in inches
#'   \item \code{projected_tl_mm}: projected total length (TL) of the fish, measured in millimeters (mm)
#'   \item \code{from_ponds}: source location within the hatchery facility from which the fish were harvested just prior to distribution or transfer
#'   \item \code{stocking_location}: ultimate destination where the fish were distributed or transferred
#'   }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_hatchery_release_2024"

#' @title Wild LRS Adult Collections – East Side Springs (ESS), FY2024
#' @name KFNFH_LRS_ESS_adult_collection_2024
#' @description
#' Records of wild adult Lost River suckers collected at East Side Springs
#' for assisted spawning at the Klamath Falls National Fish Hatchery (KFNFH)
#' during fiscal year 2024. Adults were captured, spawned, and returned
#' to the lake to support genetic representation of the ESS population.
#'
#' Corresponds to **Table 2** in the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with columns describing collection timing and fish attributes.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2024)
#'   \item \code{date}: date adults were collected
#'   \item \code{fin_clip_id_number}
#'   \item \code{pit_tag_suffix}
#'   \item \code{sex}: sex of the adult fish
#'   \item \code{gamete_use}
#'   \item \code{tl_mm}: total length (mm)
#'   \item \code{weight_g}: weight (grams)
#'   \item \code{condition}: observed condition notes
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_LRS_ESS_adult_collection_2024"


#' @title LRS ESS Incubation and Hatch Results, FY2024
#' @name KFNFH_LRS_ESS_incubation_hatch_2024
#' @description
#' Egg incubation and hatch outcomes for Lost River suckers spawned from
#' East Side Springs (ESS) adults at the Klamath Falls National Fish Hatchery
#' during fiscal year 2024.
#'
#' Corresponds to **Table 3** in the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with one row per spawning female
#' \itemize{
#'   \item \code{spawning_date}: date eggs were spawned
#'   \item \code{female}: female parent PIT tag suffix
#'   \item \code{male_1}: first male parent PIT tag suffix
#'   \item \code{male_2}: second male parent PIT tag suffix
#'   \item \code{male_3}: third male parent PIT tag suffix
#'   \item \code{male_4}: fourth male parent PIT tag suffix
#'   \item \code{incubator}: incubation system (e.g., jars, aquaria)
#'   \item \code{egg_volume_mL}: volume of eggs incubated (mL)
#'   \item \code{eggs_per_mL}: egg density (eggs per mL)
#'   \item \code{total_eggs}: total number of eggs incubated
#'   \item \code{total_fry}: total fry produced
#'   \item \code{hatch_percent}: percent hatch success
#'   \item \code{hatch_date}: date fry hatched
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_LRS_ESS_incubation_hatch_2024"


#' @title Adfluvial Early Rearing Performance, FY2024
#' @name KFNFH_adfluvial_early_rearing_2024
#' @description
#' Early rearing performance of adfluvial (river-origin) sucker larvae
#' at the Klamath Falls National Fish Hatchery (KFNFH) during fiscal year 2024.
#' Data summarize larval collection, stocking, mortality, and survival
#' prior to transfer to outdoor pond grow-out.
#'
#' Corresponds to **Table 5** in the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with one row per stocking of an individual culture unit
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: federal fiscal year (2024)
#'   \item \code{date}: collection, transfer, or observation date
#'   \item \code{culture_unit}: indoor rearing unit or tank identifier
#'   \item \code{collected}: number of larvae collected or introduced
#'   \item \code{mortality}: observed larval mortalities
#'   \item \code{stocked}: number of larvae successfully stocked or transferred
#'   \item \code{survival_percent}: percent survival relative to collected larvae
#'   \item \code{observed_mortality_percent}: percent observed mortality
#'   \item \code{unobserved_mortality_percent}: percent unobserved or inferred loss
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_adfluvial_early_rearing_2024"


#' @title Adfluvial Pond Grow-out Performance, FY2024
#' @name KFNFH_adfluvial_pond_growout
#' @description
#' Grow-out performance of adfluvial suckers reared at the Klamath Falls
#' National Fish Hatchery (KFNFH) during fiscal year 2024. The dataset
#' includes fertilized pond rearing, extended holding, and inventory
#' transitions, with each row representing a discrete grow-out unit
#' and time interval.
#'
#' Data are distinguished by originating report table using
#' \code{inventory_source}.
#'
#' Combines **Tables 6–8** from the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with rows for each pond for each inventory season.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2024)
#'   \item \code{inventory_source}: source table in the report (e.g., "FY2024 Table 6")
#'   \item \code{pond}: pond or rearing unit identifier
#'   \item \code{lot}: cohort or production lot identifier
#'   \item \code{stocked_date}: date fish were stocked into the unit
#'   \item \code{start_number}: number of fish at the start of the interval
#'   \item \code{start_g_fish}: mean fish weight at stocking (g)
#'   \item \code{start_wt_g}: total biomass at stocking (g)
#'   \item \code{start_wt_lb}: total biomass at stocking (lb)
#'   \item \code{start_tl_mm}: mean total length at stocking (mm)
#'   \item \code{harvest_date}: date fish were harvested or transferred
#'   \item \code{end_number}: number of fish at harvest or transfer
#'   \item \code{end_g_fish}: mean fish weight at harvest (g)
#'   \item \code{end_wt_g}: total biomass at harvest (g)
#'   \item \code{end_gt_lb}: total biomass at harvest (lb)
#'   \item \code{end_tl_mm}: mean total length at harvest (mm)
#'   \item \code{days}: duration of the grow-out interval (days)
#'   \item \code{months}: duration of the grow-out interval (months)
#'   \item \code{growth_mm_day}: mean daily growth rate (mm/day)
#'   \item \code{weight_gain_lb}: total biomass gain during interval (lb)
#'   \item \code{harvest_mortality_number}: mortalities recorded at harvest
#'   \item \code{survival_percent}: percent survival during the interval
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_adfluvial_pond_growout"


#' @title Net Pen Survival of Adfluvial Suckers
#' @name KFNFH_pen_survival
#' @description
#' Survival and growth outcomes for adfluvial suckers reared in net pens
#' (including Upper Klamath Lake and Gerber Reservoir) as part of the
#' Sucker Assisted Rearing Program.
#'
#' Derived from **Tables 9 and 10** in the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with rows corresponding to pen by year.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{pen_type}: type of pen (e.g., Net Pen)
#'   \item \code{operation_year}: year of net pen operation
#'   \item \code{captured_year}: year fish were originally collected
#'   \item \code{pen_id}: pen identifier
#'   \item \code{stock_tl_mm}: mean total length at stocking (mm)
#'   \item \code{number_stocked}: number of fish stocked
#'   \item \code{harvest_tl_mm}: mean total length at harvest (mm)
#'   \item \code{number_harvested}: number of fish harvested
#'   \item \code{survival_percentage}: percent survival
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"KFNFH_pen_survival"


#' @title Sucker PIT-tag Detections, FY2024
#' @name sucker_pit_tag_detection_2024
#' @description
#' Summary of PIT-tag detections from stationary monitoring locations
#' used to assess post-release survival and persistence of hatchery
#' and salvaged suckers in Upper Klamath Lake during fiscal year 2024.
#'
#' Corresponds to **Table 12** in the FY2024 KFNFH Annual Report.
#'
#' @format A tibble with monitoring deployment summaries.
#' \itemize{
#'   \item \code{monitor_location}: PIT tag monitoring site
#'   \item \code{date_deployed}: deployment date
#'   \item \code{date_retrieved}: retrieval date
#'   \item \code{tags_total_unique}: total unique PIT tags detected
#'   \item \code{tags_sarp}: tags from SARP-reared fish
#'   \item \code{tags_salvage}: tags from salvaged fish
#'   \item \code{tags_unknown}: tags of unknown origin
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2024
"sucker_pit_tag_detection_2024"

