#' EMDS Extract Required Columns
#'
#' This is a list of the key columns that are needed through the munging process
#' for the clean export. The dataset originates from the
#' [UNAIDS EDMS Database](https://edms.unaids.org/).
#'
#' @format Character vectors.
#' A vector of length 11
"req_cols"

#' Mapping of EDMS Indicator Names to those used in OHA
#'
#' A mapping of the EDMS `acronym` to common names used by OHA analysts.
#'
#' @format A data frame with 21 rows and 3 columns:
#' \describe{
#'   \item{acronym}{EDMS Acronym}
#'   \item{indicator_edms}{Parsed EDMS indicator name}
#'   \item{indicator}{Common indicator name}
#' }
"indicator_map"

#' Full list of expected indicators
#'
#' A dataframe containing the different indicators and their disaggregate p
#' pieces to ensure all data expected are being exported from EDMS.
#'
#' @format A data frame with 62 rows and 4 columns:
#' \describe{
#'   \item{indicator_edms}{Parsed EDMS indicator name}
#'   \item{age, sex}{EDMS indicator disaggregates}
#'   \item{source}{Source folder on EDMS}
#' }
"indicator_map"
