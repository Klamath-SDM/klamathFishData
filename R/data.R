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
