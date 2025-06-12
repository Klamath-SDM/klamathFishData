#' @title Modeled Fisheries Data
#' @name model_output
#' @description This dataset compiles modeled estimates and credible intervals
#' from fisheries models such as abundance and survival estimates. This dataset
#' is not species or location specific.
#' @format A tibble with 316 rows and 11 columns
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
'model_output'

#' @title Salmon Habitat Data
#' @name habitat_data
#' @description This dataset compiles modeled salmon habitat data derived from literature reviews.
#' It includes information on habitat model extent, data sources, and approximate geographic locations.
#' The Stream Salmonid Simulator (S3) model incorporates two-dimensional (2D) hydraulic models for specific Klamath River sections to calculate habitat suitability based on environmental variables, such as river flow and channel width. These models use Weighted Usable Habitat Area (WUA) curves created for specific life stages and habitat types of salmon. The WUA information derived from 2D models (covering 11.3 km or 3.6% of the Klamath’s river length) is extrapolated to unmodeled reaches, enabling habitat assessments across larger river sections.
#' Three S3 models have been developed. The original model was developed to support Fall Run Chinook and later updated to include the Trinity River and Coho populations.
#' @format A tibble with 15 rows and 13 columns
#' \itemize{
#'   \item \code{stream}: stream
#'   \item \code{sub_basin}: sub-basin name (Upper Klamath, Lower Klamath, Trinity)
#'   \item \code{location_name}: location name
#'   \item \code{model_type}: type of model used (2D hydrodynamic model, SRH-2D, micro-habitat models, HEC-EFM, SRH-1D)
#'   \item \code{length_miles}: geographical length of data coverage in miles
#'   \item \code{rm_start}: river mile of the beginning of extent, if applicable
#'   \item \code{rm_end}: river mile of the end of extent, if applicable
#'   \item \code{status}: status of model used (developed, in development)
#'   \item \code{location_souorce}: source used to determine the coordinates
#'   \item \code{source}: literature reference
#'   \item \code{link}: web link containing more information about data
#'   \item \code{latitude}: latitude of data location
#'   \item \code{longitude}: longitude of data location
#'   }
#' @details
#' For more infomation about these data compilation visit {the exploratory markdown}{https://github.com/Klamath-SDM/klamath-map/blob/add-habitat/data-raw/habitat_summary.html}
#'
'habitat_data'

#' @title Rotary Screw Traps Data
#' @name rst_sites
#' @description This dataset compiles rotary screw traps across the Klamath Basin.
#' @format A tibble with 6 rows and 8 columns
#' \itemize{
#'   \item \code{stream}: stream
#'   \item \code{sub_basin}: sub-basin name (Upper Klamath, Lower Klamath, Trinity, Shasta)
#'   \item \code{data_type}: type of data (rotary screw trap data)
#'   \item \code{rst_name}: name of rotary screw trap
#'   \item \code{agency}: agency that manages/monitors screw trap
#'   \item \code{latitude}: longitude
#'   \item \code{longitude}: longitude
#'   \item \code{link}: web link containing more information about rotary screw trap
#'   }
'rst_sites'


#' @title Hatchery Locations
#' @name hatcheries
#' @description This dataset contains the hatchery locations and resources to find more information about them.
#' @format A tibble with 3 rows and 8 columns
#' \itemize{
#'   \item \code{stream}: stream
#'   \item \code{sub_basin}: sub-basin name (Upper Klamath, Shasta)
#'   \item \code{data_type}: type of data (hatchery data)
#'   \item \code{site_name}: name of hatchery
#'   \item \code{agency}: agency that manages/monitors hatchery
#'   \item \code{latitude}: latitude
#'   \item \code{longitude}: longitude
#'   \item \code{link}: web link containing more information about hatchery
#'   }
'hatcheries'
