#' Country flags for Epi Control and 90's goal achievement
#'
#' This is a dataframe stored in a list with countries and their ISOs, and flags
#' indicating achievement of Epidemic Control and all three 90's. The metric for
#' 90-90-90 achievement has 2 flags, one using PLHIV as the base and another
#' using a relative base.
#'
#' @format A data frame with 51 rows and 4 variables:
#' \describe{
#'   \item{country}{Country name from UNAIDS Clean Estimates}
#'   \item{iso}{Country ISO code}
#'   \item{plhiv_base_90s}{Achievement of all three 90's (using PLHIV base)}
#'   \item{rel_base_90s}{Achievement of all three 90's (using relative base)}
#'   \item{epi_control}{Achieved epidemic control}
#' }

"flag_list"
