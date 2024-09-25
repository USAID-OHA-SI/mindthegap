#' Validate PEPFAR countries
#'
#' Ensure there are no missing PEPFAR countries from the EDMS pull
#'
#' @param df dataframe
#' @keywords internal
validate_countries <- function(df){

  #access list of PEPFAR countries from stored data (source: glamr)
  pepfar_countries <- pepfar$country_pepfar

  #pepfar countries that exist in the EDMS
  pepfar_countries_edms <- df %>%
    dplyr::distinct(country, pepfar) %>%
    dplyr::filter(pepfar == TRUE) %>%
    dplyr::pull(country)

  missing <- setdiff(pepfar_countries, pepfar_countries_edms)

  if(length(missing) > 0)
    cli::cli_warn(c("{length(missing)} expected countr{?y/ies} {?is/are} missing from the EDMS data pull.",
                    i = "Missing PEPFAR countr{?y/ies}: {.val {missing}}"))

}
