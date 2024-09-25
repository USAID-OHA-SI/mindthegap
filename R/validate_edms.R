#' Validate Path
#'
#' Validate file path to EDMS output is a csv and exits.
#'
#' @inheritParams munge_edms
#'
#' @keywords internal
#'
validate_path <- function(path){

  if(tools::file_ext(path) != "csv")
    cli::cli_abort(c("Expecting user to provide a csv as the {.var path}.",
                     i = "Please provide a csv file."))

  if(!file.exists(path))
    cli::cli_abort(c("File does not exist: {.file {path}}."))
}

#' Validate Columns
#'
#' Ensure that all the columns that are needed exists in the import
#'
#' @param df imported dataframe
#' @keywords internal
#'
validate_columns <- function(df){

  #validation to ensure all columns exist
  if(!all(req_cols %in% names(df))){
    col_missing <- setdiff(req_cols, names(df))
    cli::cli_abort("The dataframe is missing {length(col_missing)} key column{?s} needed: {.field {col_missing}}")
  }

}

#' Validate Indicator and Disaggregated included
#'
#' Check if all expected indicators and their disaggregates are included and/or
#' any additional indicators/disaggregates are included
#'
#' @param df dataframe
#' @keywords internal
#'
validate_ind_disaggs <- function(df){
  #get list of indicators in EDMS output
  df_included <- df %>%
    # parse_indicator() %>%
    dplyr::filter(e_cat != "Uncertainty Analysis") %>%
    dplyr::distinct(indicator_edms, age, sex, source = e_cat) %>%
    dplyr::mutate(in_data = TRUE)

  #list of indicators/disaggs expected (for normal, annual pull)
  df_expected <- indicator_validation %>%
    dplyr::mutate(expected = TRUE)

  #join list to see what is missing/additional
  df_join <- dplyr::full_join(df_included, df_expected,
                              dplyr::join_by(indicator_edms, age, sex, source))

  #create a status to sort on
  df_status <- df_join %>%
    dplyr::mutate(status = dplyr::case_when(expected == TRUE  & in_data == TRUE  ~ "Success",
                                            expected == TRUE  & in_data == FALSE ~ "Missing",
                                            expected == FALSE & in_data == TRUE  ~ "Additional"),
                  ind_combo = stringr::str_glue("{indicator_edms}: {sex}|{age} [{source}]"))

  #check if anything is missing
  missing <- df_status %>%
    dplyr::filter(status == "Missing") %>%
    dplyr::pull()

  if(length(missing) > 0)
    cli::cli_warn(c("The following {length(missing)} expected item{?s} {?is/are} missing from the EDMS output:",
                    stats::setNames(missing, rep("*", length(missing)))
    ))

  #check if anything is extra
  additional <- df_status %>%
    dplyr::filter(status == "Additional") %>%
    dplyr::pull()

  if(length(additional) > 0)
    cli::cli_warn(c("The following {length(additional)} additional item{?s} {?is/are} included in the EDMS output:",
                    stats::setNames(additional, rep("*", length(additional)))
    ))
}

#' Validate PEPFAR countries
#'
#' Ensure there are no missing PEPFAR countries from the EDMS pull
#'
#' @param df dataframe
#' @keywords internal
validate_countries <- function(df){

  pepfar_countries_expected <- glamr::pepfar_country_list$country

  pepfar_countries_edms <- df %>%
    dplyr::distinct(country, pepfar) %>%
    dplyr::filter(pepfar == TRUE) %>%
    dplyr::pull(country)

  missing <- setdiff(pepfar_countries_expected, pepfar_countries_edms)

  if(length(missing) > 0)
    cli::cli_warn(c("{length(missing)} expected countr{?y/ies} {?is/are} missing from the EDMS data pull.",
                    i = "Missing PEPFAR countr{?y/ies}: {.val {missing}}"))

}
