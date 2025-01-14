#' EMDS Extract Required Columns
#'
#' This is a list of the key columns that are needed through the munging process
#' for the clean export. The dataset originates from the
#' [UNAIDS EDMS Database](https://edms.unaids.org/).
#'
#' @format Character vectors.
#' A vector of length 11
"req_cols"


#' PEPFAR countries + ISO codes
#'
#' This is a dataframe of countries that receive PEPFAR funding. This df comes
#' from `glamr::pepfar_country_list` and is originally derived from the
#' PEPFAR Financial Structured Dataset (FSD).
#'
#' @format A data frame with 55 rows and 2 columns:
#' \describe{
#'   \item{country_pepfar}{Country name}
#'   \item{iso3}{Country ISO3 code}
#' }
"pepfar"

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
#' A dataframe containing the different indicators and their disaggregate
#' components to ensure all data expected for are being exported from EDMS.
#'
#' @format A data frame with 62 rows and 5 columns:
#' \describe{
#'   \item{indicator_edms}{Parsed EDMS indicator name}
#'   \item{age, sex}{EDMS indicator disaggregates}
#'   \item{source}{Source folder on EDMS}
#'   \item{expected}{Expected as part of EDMS Epi + 95s pull (TRUE)}
#' }
"expected_ind"
