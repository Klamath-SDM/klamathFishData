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

#' @title Historical Hatchery Collection and Releases, KFNFH
#' @name KFNFH_historical_collection_release
#' @description Collections and releases from the Klamath Falls National Fish Hatchery (KFNFH) since its inception in 2016. Data summarizes the annual inputs and outputs of the hatchery program by federal fiscal year (FY2016-2025), plus an approximate count of broodstock on station.
#' Corresponds to **Table 1** and **Table 7** in the FY2025 KFNFH Annual Report. Each annual report reproduces the full history with updated values, so the most recent report is used for all years. Broodstock columns were compiled from the annual reports (see \code{data-raw/hatchery_data_exploration.Rmd}).
#' Note: FY2025 Table 1 gives 30,142 larvae collected in FY2024, while the FY2024 report (Tables 1 and 4), FY2025 Table 7 and the hatchery's larval collection workbook all give 30,150; 30,150 is used here. Annual collection totals (6,036 in FY2023, 30,150 in FY2024, 70,049 in FY2025) were also confirmed against the hatchery workbook \code{2025 Larval Collection and Early Rearing Summary Data (version 1).xlsx}.
#' @format A tibble with 10 rows and 18 columns
#' \itemize{
#'   \item \code{hatchery_name}: name of hatchery (KFNFH)
#'   \item \code{fiscal_year}: fiscal year
#'   \item \code{larvae_collected}: number of wild larval suckers collected
#'   \item \code{sarp_release}: number of fish released or transferred that were raised in the Sucker Assisted Rearing Program (SARP)
#'   \item \code{sarp_tl_mm}: average total length (TL) of the SARP release fish, measured in millimeters (mm)
#'   \item \code{fingerling_release}: number of fingerlings released or transferred
#'   \item \code{fingerling_tl_mm}: average total length (TL) of the fingerling release fish, measured in millimeters (mm)
#'   \item \code{fry_release}: number of fry released or transferred. This includes fry produced from experimental, wild-spawning activities that were transferred to other facilities (e.g., the Klamath Tribes Hatchery in FY24 and FY25)
#'   \item \code{fry_tl_mm}: average total length (TL) of the fry release fish, measured in millimeters (mm)
#'   \item \code{salvage_release}: number of suckers salvaged throughout the year
#'   \item \code{salvage_sl_mm}: average standard length (SL) of salvage fish in millimeters (mm), reported in years where the report gives standard length
#'   \item \code{salvage_tl_mm}: average total length (TL) of salvage fish in millimeters (mm), reported in years where the report gives total length
#'   \item \code{primary_collection_method}: drift net, dip net, or both
#'   \item \code{total_released}: total number of fish released across release types
#'   \item \code{broodstock_on_station_raw}: broodstock currently on station, as written in the report
#'   \item \code{broodstock_cohort_years}: capture years of the broodstock on station, as written in the report
#'   \item \code{new_broodstock_reported}: number of new broodstock reported
#'   \item \code{broodstock_on_station}: leading number parsed from \code{broodstock_on_station_raw}; a rounded or minimum estimate, not an exact count
#'   }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2025 (Tables 1 and 7); broodstock columns compiled from the annual reports
"KFNFH_historical_collection_release"

#' @title Hatchery Distributions and Transfers, KFNFH
#' @name KFNFH_hatchery_release
#' @description Summary of all fish distributions for repatriation or transfer from the Klamath Falls National Fish Hatchery during fiscal years 2021-2025. A "distribution" is considered a stocking or repatriation event, while a "transfer" involves moving fish for temporary holding.
#' FY2025 matches the hatchery workbook \code{FY25 Fish Distribution.xlsx}; the FY2021 and FY2022 totals match \code{FY20-FY25 Pond Summaries and Fish Distributions.xlsx} (FY2023 is 48,377 here and 48,374 in that workbook).
#' Corresponds to **Table 15** in the FY2025 KFNFH Annual Report, **Table 11** in the FY2024 report, **Table 10** in the FY2023 report, **Table 5** in the FY2022 report, and **Table 2** in the FY2021 report.
#' @format A tibble with 181 rows and 16 columns
#' \itemize{
#'   \item \code{hatchery_name}: name of hatchery (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2021-2025)
#'   \item \code{dispo_date}: date the fish distribution or transfer event occurred
#'   \item \code{species}: species of sucker being distributed (e.g. LRS, SNS, ESS LRS)
#'   \item \code{lot}: grouping or cohort of fish categorized primarily by their collection year (CY) or their operational origin
#'   \item \code{number_fish}: number of fish in that specific grouping (lot) that were distributed or transferred
#'   \item \code{weight_lb}: total weight of the fish lot being distributed, measured in pounds (lb)
#'   \item \code{actual_tl_mm}: actual total length (TL) of the fish, measured in millimeters (mm)
#'   \item \code{number_per_lb}: number of fish per pound for the specific lot being distributed
#'   \item \code{projected_tl_inches}: projected total length (TL) of the fish, measured in inches
#'   \item \code{projected_tl_mm}: projected total length (TL) of the fish, measured in millimeters (mm)
#'   \item \code{from_ponds}: source location within the hatchery facility from which the fish were harvested just prior to distribution or transfer
#'   \item \code{dispo_location}: ultimate destination where the fish were distributed or transferred
#'   \item \code{fish_type}: fish type as reported in the earlier (FY2021-2022) report tables
#'   \item \code{tl_inches}: total length in inches as reported in the earlier (FY2021-2022) report tables
#'   \item \code{tl_mm}: total length in millimeters as reported in the earlier (FY2021-2022) report tables
#'   }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2021-2025
"KFNFH_hatchery_release"

#' @title Wild LRS Adult Collections – East Side Springs (ESS)
#' @name KFNFH_LRS_ESS_adult_collection
#' @description
#' Records of wild adult Lost River suckers collected at East Side Springs
#' for assisted spawning at the Klamath Falls National Fish Hatchery (KFNFH)
#' during fiscal years 2023-2025. Adults were captured, spawned, and returned
#' to the lake to support genetic representation of the ESS population.
#'
#' Corresponds to **Table 2** in the FY2025, FY2024 and FY2023 KFNFH Annual Reports.
#'
#' @format A tibble with columns describing collection timing and fish attributes. Columns that a report did not include are \code{NA} for that year.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2023-2025)
#'   \item \code{date}: date adults were collected
#'   \item \code{fin_clip_id_number}: fin clip identification number
#'   \item \code{pit_tag_suffix}: PIT tag suffix (FY2024 and FY2025)
#'   \item \code{pit_tag_last_5}: last five characters of the PIT tag
#'   \item \code{sex}: sex of the adult fish
#'   \item \code{gamete_use}: whether gametes were used (e.g. Spawned, Not Spawned; FY2025 includes the reason, e.g. "Not Spawned/No Eggs")
#'   \item \code{fl_mm}: fork length (mm)
#'   \item \code{tl_mm}: total length (mm); reported in FY2024 only
#'   \item \code{spring_location}: spring or location where the fish was collected
#'   \item \code{family_groups}: number of family groups the female was crossed into (FY2024 and FY2025)
#'   \item \code{fl_in}: fork length (inches)
#'   \item \code{atfc_genetic_id}: Abernathy Fish Technology Center genetic ID (FY2023)
#'   \item \code{fry_collected_for_aftc}: whether fry were collected for genetic analysis by the Abernathy Fish Technology Center (FY2023)
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2023-2025. FY2024 (43 fish) matches \code{METADATA FOR ALL ESS LRS FISH SPAWNED (version 1).xlsx} and FY2023 (29 fish) matches the "Genetics for All Fish" sheet of \code{FY25 METADATA FOR ALL ESS LRS FISH SPAWNED.xlsx}; the FY2025 records (69 fish) come from the report table only
"KFNFH_LRS_ESS_adult_collection"


#' @title LRS ESS Incubation and Hatch Results
#' @name KFNFH_LRS_ESS_incubation_hatch
#' @description
#' Egg incubation and hatch outcomes for wild Lost River suckers spawned from
#' East Side Springs (ESS) adults at the Klamath Falls National Fish Hatchery
#' during fiscal years 2024 and 2025.
#'
#' Corresponds to **Table 3** in the FY2024 and FY2025 KFNFH Annual Reports. FY2025 rows were compiled from the report (see \code{data-raw/hatchery_data_exploration.Rmd}).
#'
#' @format A tibble with one row per spawning female and incubation vessel
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2024-2025)
#'   \item \code{program}: spawning program (Wild ESS Spawning)
#'   \item \code{species}: species (LRS)
#'   \item \code{spawning_date}: date eggs were spawned
#'   \item \code{female}: female parent PIT tag suffix
#'   \item \code{male_1}: first male parent PIT tag suffix
#'   \item \code{male_2}: second male parent PIT tag suffix
#'   \item \code{male_3}: third male parent PIT tag suffix
#'   \item \code{male_4}: fourth male parent PIT tag suffix
#'   \item \code{male_5}: fifth male parent PIT tag suffix (FY2025 only)
#'   \item \code{male_6}: sixth male parent PIT tag suffix (FY2025 only)
#'   \item \code{family_groups}: number of family groups (distinct males crossed with the female)
#'   \item \code{incubator}: incubation system (e.g., jars, aquaria); not reported in FY2025
#'   \item \code{egg_volume_mL}: volume of eggs incubated (mL)
#'   \item \code{eggs_per_mL}: egg density (eggs per mL)
#'   \item \code{total_eggs}: total number of eggs incubated
#'   \item \code{total_fry}: total fry produced at the first count
#'   \item \code{hatch_percent}: percent hatch success at the first count
#'   \item \code{hatch_date}: date fry hatched; not reported in FY2025
#'   \item \code{second_fry_count}: total fry at the second count (FY2025 only)
#'   \item \code{second_hatch_percent}: percent hatch success at the second count (FY2025 only)
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2024 and 2025. The FY2025 females (18 crosses) match the "FY25 Tables Reorganized" sheet of \code{FY25 METADATA FOR ALL ESS LRS FISH SPAWNED.xlsx}; the FY2024 crosses match the "FY24 Tables Reorganized - MY" sheet of \code{METADATA FOR ALL ESS LRS FISH SPAWNED (version 1).xlsx}
"KFNFH_LRS_ESS_incubation_hatch"


#' @title Adfluvial Early Rearing Performance
#' @name KFNFH_adfluvial_early_rearing
#' @description
#' Early rearing performance of adfluvial (river-origin) sucker larvae
#' at the Klamath Falls National Fish Hatchery (KFNFH) during fiscal years 2023, 2024 and 2025.
#' Data summarize larval collection, stocking, mortality, and survival
#' prior to transfer to outdoor pond grow-out. The FY2021 and FY2022 reports do not include an equivalent table.
#'
#' Corresponds to **Table 3** in the FY2023 report, **Table 5** in the FY2024 report, and **Table 8** in the FY2025 KFNFH Annual Report.
#'
#' @format A tibble with one row per stocking of an individual culture unit
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: federal fiscal year (2023-2025)
#'   \item \code{date_raw}: collection date as written in the report (may list two days, e.g. "5/8/2025 & 5/9/2025")
#'   \item \code{date}: collection, transfer, or observation date (first date when two are listed)
#'   \item \code{culture_unit}: indoor rearing unit or tank identifier
#'   \item \code{collected}: number of larvae collected or introduced
#'   \item \code{mortality}: observed larval mortalities
#'   \item \code{stocked}: number of larvae successfully stocked or transferred
#'   \item \code{survival_percent}: percent survival relative to collected larvae
#'   \item \code{observed_mortality_percent}: percent observed mortality
#'   \item \code{unobserved_mortality_percent}: percent unobserved or inferred loss
#'   \item \code{restock}: \code{TRUE} for "Restock" rows (larvae moved back into an already-stocked tank). In FY2024 these rows re-count 7,327 fish, so the \code{collected} column sums to 37,477 while 30,150 larvae were collected; sum \code{collected} over non-restock rows (or use \code{KFNFH_historical_collection_release}) for collection totals
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2023, 2024 and 2025. The FY2024 rows are identical to the hatchery workbook \code{2024 Larval Collection and Early Rearing Summary Data.xlsx} (sheet "Pond Performance Table 4 Report"); FY2023 and FY2025 totals agree with \code{2025 Larval Collection and Early Rearing Summary Data (version 1).xlsx}
"KFNFH_adfluvial_early_rearing"


#' @title Adfluvial Pond Grow-out Performance, FY2021-2025
#' @name KFNFH_pond_growout
#' @description
#' Grow-out performance of adfluvial and East Side Springs (ESS) suckers reared at the Klamath Falls
#' National Fish Hatchery (KFNFH) during fiscal years 2021-2025. The dataset
#' includes fertilized pond rearing, extended holding, and inventory
#' transitions, with each row representing a discrete grow-out unit
#' and time interval (one pond cycle).
#'
#' Data are distinguished by originating report table using
#' \code{inventory_source}.
#'
#' Combines **Tables 10-12** from the FY2025 KFNFH Annual Report, **Tables 6-8** from FY2024, **Tables 5-7** from FY2023, **Tables 2-4** from FY2022, and **Tables 3-4** from FY2021. A cycle reported as still in progress in one report (no harvest date) is replaced by its completed record from the later report, so a pond cycle appears once. The Production Harvest Data Sheet workbooks used in the exploration were compared with these tables and add no additional cycles. The fiscal year of a cycle is the report in which it was completed (or, if still in progress, the last report that lists it). FY2022 rows taken from report tables 3 and 4 (embedded as images) have no \code{inventory_source}.
#'
#' @format A tibble with rows for each pond for each inventory season.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2021-2025)
#'   \item \code{inventory_source}: source table in the report (e.g., "FY2024 Table 6")
#'   \item \code{life_history}: ESS (east spring spawner) or adfluvial
#'   \item \code{source}: where the row came from ("annual_report_table" or "production_harvest_workbook")
#'   \item \code{pond}: pond or rearing unit identifier
#'   \item \code{lot}: cohort or production lot identifier
#'   \item \code{pond_stock_date}: date fish were stocked into the unit
#'   \item \code{harvest_date}: date fish were harvested or transferred
#'   \item \code{start_number}: number of fish at the start of the interval
#'   \item \code{start_g_fish}: mean fish weight at stocking (g)
#'   \item \code{start_wt_g}: total biomass at stocking (g)
#'   \item \code{start_wt_lb}: total biomass at stocking (lb)
#'   \item \code{start_tl_mm}: mean total length at stocking (mm)
#'   \item \code{end_number}: number of fish at harvest or transfer
#'   \item \code{end_g_fish}: mean fish weight at harvest (g)
#'   \item \code{end_wt_g}: total biomass at harvest (g)
#'   \item \code{end_gt_lb}: total biomass at harvest (lb) as named in the FY2024 tables (see also \code{end_wt_lb})
#'   \item \code{end_tl_mm}: mean total length at harvest (mm)
#'   \item \code{days}: duration of the grow-out interval (days)
#'   \item \code{months}: duration of the grow-out interval (months)
#'   \item \code{growth_mm_day}: mean daily growth rate (mm/day)
#'   \item \code{weight_gain_lb}: total biomass gain during interval (lb); not reported for FY2025
#'   \item \code{harvest_mortality_number}: mortalities recorded at harvest
#'   \item \code{survival_percent}: percent survival during the interval
#'   \item \code{end_wt_lb}: total biomass at harvest (lb); FY2025 and earlier report tables that name the column this way
#'   \item \code{weight_gain_g}: total biomass gain during interval (g)
#'   \item \code{dispo_to}: destination of the harvested fish
#'   \item \code{dispo_tl_mm}: mean total length of the fish sent to the destination (mm)
#'   \item \code{dispo_number}: number of fish sent to the destination
#'   \item \code{dispo_percent}: percent of harvested fish sent to the destination
#'   \item \code{short_number}: number of fish short of the target length
#'   \item \code{short_percent}: percent of fish short of the target length
#'   \item \code{short_tl_mm}: mean total length of the fish short of the target (mm)
#'   \item \code{brood_number}: number of fish kept as broodstock
#'   \item \code{brood_percent}: percent of harvested fish kept as broodstock
#'   \item \code{age_class}: age class of the fish in the pond in that fiscal year, from \code{KFNFH_age_facility_lookup}, which is rebuilt from the reports and workbooks (e.g. "1", "2", "1/2" where several age groups were held, "wild", "other"); \code{NA} where the pond is not in the lookup
#'   \item \code{starting_count}: starting count of fish in the pond from \code{KFNFH_age_facility_lookup}
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2021-2025. Completed FY2025 cycles were validated against \code{FY2025 Production Harvest Data Sheet_20251107.xlsx}; the workbook's live end count equals \code{end_number} minus \code{harvest_mortality_number}
"KFNFH_pond_growout"


#' @title Net Pen Survival of Adfluvial Suckers
#' @name KFNFH_pen_survival
#' @description
#' Survival and growth outcomes for adfluvial suckers reared in net pens
#' (including Upper Klamath Lake and Gerber Reservoir) as part of the
#' Sucker Assisted Rearing Program, by operation year. Years in which a pen was
#' not operated are kept as rows with a \code{note} and missing values.
#'
#' Derived from **Tables 9 and 10** in the FY2024 KFNFH Annual Report and confirmed against **Tables 13 and 14** in the FY2025 report, which add the operation status for 2024 and 2025.
#'
#' @format A tibble with rows corresponding to pen by year.
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{pen_type}: type of pen (Net Pen for Upper Klamath Lake, Gerber Net Pen)
#'   \item \code{operation_year}: year of net pen operation
#'   \item \code{captured_year}: year fish were originally collected
#'   \item \code{pen_id}: pen identifier
#'   \item \code{stock_tl_mm}: mean total length at stocking (mm)
#'   \item \code{number_stocked}: number of fish stocked
#'   \item \code{harvest_tl_mm}: mean total length at harvest (mm)
#'   \item \code{number_harvested}: number of fish harvested
#'   \item \code{survival_percentage}: percent survival
#'   \item \code{note}: reason there are no data for that operation year (e.g. "Low water - not operational", "No fish available - not operational")
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2024 and 2025. All rows and notes are identical to the hatchery workbook \code{nphistory.xlsx} (sheets "UKL" and "Gerber")
"KFNFH_pen_survival"


#' @title Sucker PIT-tag Detections, FY2024
#' @name sucker_pit_tag_detection_2024
#' @description
#' Summary of PIT-tag detections from stationary monitoring locations
#' used to assess post-release survival and persistence of hatchery
#' and salvaged suckers in Upper Klamath Lake during fiscal year 2024.
#'
#' Corresponds to **Table 12** in the FY2024 KFNFH Annual Report. Calendar year 2025 detections (six sites) are in the FY2025 report (Tables 16-17) and in the hatchery workbook \code{monitor table.xlsx}; they are not yet in this object.
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


#' @title Fish Age by Facility and Fiscal Year, KFNFH
#' @name KFNFH_age_facility_lookup
#' @description
#' Age class(es) and starting count of the fish in each pond or net pen at the Klamath Falls National Fish Hatchery (KFNFH), by fiscal year.
#' USFWS does not maintain such a table, so it is rebuilt from the reports and workbooks only:
#' \itemize{
#'   \item Net pens (UKL and Gerber): from the hatchery workbook \code{nphistory.xlsx}. Age is the operation year minus the capture year; \code{starting_count} is the number of fish stocked.
#'   \item Ponds (A, B, C and P series): from every pond cycle in the Production Harvest Data Sheet workbooks and the annual report pond tables. A cycle counts in the fiscal year it was stocked and in the fiscal year it was harvested. The age class is that fiscal year minus the lot's collection year (4 means 4 or older); East Side Springs and mixed lots are "other", and wild and salvage lots are "wild". \code{starting_count} is the number of fish at the start of the first cycle harvested in the year.
#' }
#' This is the single source of fish age used by \code{KFNFH_pond_growout} and \code{KFNFH_mortality_pond_monthly}.
#'
#' **Differences from the hand-compiled lookup this replaces.** Of the 232 facility-years in the hand-compiled lookup, the rebuilt lookup gives the same age class for 161, a subset of the curated classes for 18 (the curated value lists an extra class such as "other", "wild" or a second age that is not in the reports or workbooks), and a different class for 43 (mostly the fiscal year assigned to a cycle that spans two years). Ten facility-years only in the hand-compiled lookup are not in the package (FY2020 ponds A1, A2, P5 and P11-P15, and the unoperated UKLE and UKLW pens in FY2025); five rebuilt facility-years are not in the hand-compiled lookup. Starting counts are equal for 106 of the 126 counts present in both.
#'
#' @format A tibble with 227 rows and 5 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year
#'   \item \code{facility}: facility code (pond or net pen, upper case)
#'   \item \code{age_class}: age class(es) of the fish, as text; several classes are separated by "/" (e.g. "1/2"); "wild" and "other" also occur
#'   \item \code{starting_count}: number of fish at the start of the year (see the description); \code{NA} when no record gives it
#' }
#'
#' @source Klamath Falls National Fish Hatchery workbooks \code{nphistory.xlsx} and the Production Harvest Data Sheet workbooks (FY2024 and FY2025), and the Annual Reports for Fiscal Years 2021-2025 (pond tables)
"KFNFH_age_facility_lookup"


#' @title Hatchery Mortality Events from Report Narratives, KFNFH
#' @name KFNFH_mortality_events
#' @description
#' Disease, water quality, weather, predation and other events described in the narratives of the FY2021-2024
#' Klamath Falls National Fish Hatchery (KFNFH) annual reports, with a category, a best-estimate percent lost and a
#' severity tier. Most events are not quantified in the reports. The FY2025 report has not been reviewed.
#' See \code{KFNFH_mortality_events_summary} for a simplified version.
#'
#' Identifying an event and describing it (period, location, category, cause, population affected, loss as reported, confidence) is a
#' reading of the report narratives and is the only part taken from the team's hand-compiled table; each event is tied to a phrase in
#' the report. \code{percent_lost} is recomputed from the net pen records (\code{nphistory.xlsx}), the larval rearing tables and workbook, and
#' numbers in the report text, and \code{severity_tier} follows from it by a fixed rule (over 70\% Catastrophic, 30-70\% Major, 10-30\% Moderate,
#' under 10\% Minor; no loss in a non-mortality event is a near-miss; no number is qualitative only).
#'
#' **Difference from the hand-compiled table:** event E22 (August 2024 dissolved-oxygen crashes in ponds P12 and P13) was given an estimated
#' 45\% loss and the tier Major. The FY2024 report only says "several thousand fish" were lost, so \code{percent_lost} is \code{NA} and the tier
#' is "Qualitative only (unquantified)". All other losses and tiers are identical.
#'
#' @format A tibble with 28 rows and 13 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{event_id}: event identifier (E01, E02, ...)
#'   \item \code{year}: year the event occurred
#'   \item \code{period}: date or period of the event as described
#'   \item \code{location}: pond, tank, pen or facility affected
#'   \item \code{category}: event category (disease, water_quality, weather, predation, equipment, other)
#'   \item \code{mortality_event}: whether the event resulted in fish mortality
#'   \item \code{cause_description}: cause of the event as described
#'   \item \code{population_affected}: group of fish affected (the denominator for the loss)
#'   \item \code{loss_as_reported}: number or percent lost as reported
#'   \item \code{percent_lost}: best-estimate percent lost (numeric); \code{NA} when not quantified
#'   \item \code{confidence}: how well the loss is quantified (e.g. Quantified, Estimated, Qualitative only)
#'   \item \code{severity_tier}: severity class (Catastrophic >70\% loss, Major 30-70\%, Moderate 10-30\%, Minor <10\%, Near-miss, Non-mortality, Qualitative only)
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2021-2024 (narrative sections, event descriptions compiled by the package authors); losses from \code{nphistory.xlsx}, \code{2024 Larval Collection and Early Rearing Summary Data.xlsx}, the FY2023 report tables and the report text
"KFNFH_mortality_events"


#' @title Hatchery Mortality Events Summary, KFNFH
#' @name KFNFH_mortality_events_summary
#' @description
#' Simplified version of \code{KFNFH_mortality_events}: one row per event with the category, whether it was a
#' mortality event, the percent mortality and the number of fish lost where available. Most events have no quantity. Computed the same way as \code{KFNFH_mortality_events}; event E22 has no percent mortality (see that object).
#'
#' @format A tibble with 28 rows and 7 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{event_id}: event identifier, matches \code{KFNFH_mortality_events}
#'   \item \code{year}: year the event occurred
#'   \item \code{category}: event category
#'   \item \code{mortality_event}: whether the event resulted in fish mortality
#'   \item \code{percent_mortality}: percent of the affected population lost (numeric percent, e.g. 99 = 99\%)
#'   \item \code{n_mortality}: number of fish lost, where reported
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2021-2024, synthesized by the package authors
"KFNFH_mortality_events_summary"


#' @title Monthly Pond Mortality, KFNFH
#' @name KFNFH_mortality_pond_monthly
#' @description
#' Monthly mortality counts by pond from the hatchery's water quality and mortality logs for fiscal years 2023-2025,
#' plus per-event mortality during harvest operations in FY2023. The records are at different levels of detail in each
#' year (FY2023 is a pond by month table; FY2024 and FY2025 are daily per-pond logs summed to months).
#' Counts come straight from the three workbooks; \code{age_class} and \code{starting_count} come from \code{KFNFH_age_facility_lookup}, so about 30\% of the records carry a different age class than the hand-compiled lookup would give (see that object).
#' FY2024 covers 1 January to 22 September 2024 only (the October-December 2023 quarter was not found), so FY2024 totals are a partial year. FY2023 includes all pond series (A, B and P) and is larger than the "Monthly with no harvest Morts" column of the source sheet, which covers the P-series only.
#'
#' @format A tibble with 1516 rows and 8 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (2023-2025)
#'   \item \code{month}: calendar month (abbreviated); missing for some harvest records
#'   \item \code{pond}: pond or tank identifier
#'   \item \code{mortality}: number of fish recorded dead
#'   \item \code{record_type}: "pond" for routine pond mortality or "harvest" for mortality recorded during harvest operations
#'   \item \code{age_class}: age class of the fish in the pond, from \code{KFNFH_age_facility_lookup}
#'   \item \code{starting_count}: starting count of fish in the pond, from \code{KFNFH_age_facility_lookup}
#' }
#'
#' @source Klamath Falls National Fish Hatchery mortality spreadsheets: Fiscal Year 23 WQ_Mortality reporting.xlsx, Copy of 2024_KFNFH_WQ.xlsx, KFNFH_WQ_FY2025_2 (version 1).xlsx
"KFNFH_mortality_pond_monthly"


#' @title Hatchery Pond and Tank Capacity, KFNFH
#' @name KFNFH_capacity
#' @description
#' Outdoor pond and indoor tank capacity at the Klamath Falls National Fish Hatchery (KFNFH) by fiscal year. Construction changed capacity
#' over time, so the current (constrained) capacity and the planned full build-out capacity are recorded as separate rows
#' (\code{capacity_type}) rather than choosing one.
#'
#' Pond counts and acreage come from the facility description of each report (each phrase is checked against the report text). Indoor gallons are
#' summed from the tank lists in the reports: 10,360 (FY2021), 12,035 (FY2022), 12,755 (FY2023 and FY2024; the second indoor building is first described in
#' the FY2025 report) and 16,395 (FY2025). **Difference from the hand-compiled table:** it repeated the FY2025 total (16,395) for FY2022-FY2024 and for the planned build-out; the
#' reports support those values only for FY2025, and no report gives indoor gallons for the planned build-out (\code{NA}).
#'
#' @format A tibble with 6 rows and 6 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{fiscal_year}: fiscal year (planned build-out is shown in a future fiscal year)
#'   \item \code{n_outdoor_ponds}: number of outdoor ponds
#'   \item \code{acreage_outdoor_ponds}: total acreage of outdoor ponds
#'   \item \code{gallons_indoor}: indoor tank capacity in gallons
#'   \item \code{capacity_type}: "current" or "planned"
#' }
#'
#' @source Facility descriptions in the Klamath Falls National Fish Hatchery Annual Reports for Fiscal Years 2021-2025 (pond counts and acreage; the planned 33 ponds on 8.5 acres is in the FY2025 report) and the tank lists in the same reports (indoor gallons)
"KFNFH_capacity"


#' @title Broodstock Spawned by Capture Year, KFNFH
#' @name KFNFH_broodstock_spawned
#' @description
#' Number of broodstock, by capture-year class, species and sex, spawned at the Klamath Falls National Fish Hatchery (KFNFH)
#' in spring 2025 (Shortnose sucker, SNS, and Lost River sucker, LRS). One LRS record has a capture year that was not recorded.
#'
#' Corresponds to **Table 4** in the FY2025 KFNFH Annual Report.
#'
#' @format A tibble with 9 rows and 8 columns
#' \itemize{
#'   \item \code{hatchery_name}: hatchery name (KFNFH)
#'   \item \code{spawn_fiscal_year}: fiscal year in which the fish were spawned (2025)
#'   \item \code{species}: SNS or LRS
#'   \item \code{capture_year}: year the broodstock was captured (\code{NA} when not recorded)
#'   \item \code{capture_year_raw}: capture year as written in the report
#'   \item \code{females_spawned}: number of females spawned
#'   \item \code{males_spawned}: number of males spawned
#'   \item \code{total_spawned}: total number of fish spawned
#' }
#'
#' @source Klamath Falls National Fish Hatchery Annual Report for Fiscal Year 2025
"KFNFH_broodstock_spawned"
