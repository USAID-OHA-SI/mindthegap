#' Validate Indicator and Disaggregated included
#'
#' Check if all expected indicators and their disaggregates are included and/or
#' any additional indicators/disaggregates are included
#'
#' @param df dataframe
#' @inheritParams munge_edms
#' @keywords internal
#'
validate_ind_disaggs <- function(df, epi_95s_flag = TRUE){
  #get list of indicators in EDMS output
  df_included <- df %>%
    # parse_indicator() %>%
    dplyr::filter(e_cat != "Uncertainty Analysis") %>%
    dplyr::distinct(indicator_edms, age, sex, source = e_cat) %>%
    dplyr::mutate(in_data = TRUE)

  if(epi_95s_flag == FALSE){

    included <- df_included %>%
      dplyr::mutate(ind_combo = stringr::str_glue("{indicator_edms}: {sex}|{age} [{source}]")) %>%
      dplyr::pull()

    cli::cli_inform(c("The following {length(included)} item{?s} {?is/are} included from the EDMS output:",
                      stats::setNames(included, rep("*", length(included)))
                      ))

    invisible()
  }

  #join list to see what is missing/additional
  df_join <- dplyr::full_join(df_included, expected_ind,
                              dplyr::join_by(indicator_edms, age, sex, source))

  #create a status to sort on
  df_status <- df_join %>%
    dplyr::mutate(across(c(expected, in_data), \(x) ifelse(is.na(x), FALSE, x)),
                  status = dplyr::case_when(expected == TRUE  & in_data == TRUE  ~ "Success",
                                            expected == TRUE  & in_data == FALSE ~ "Missing",
                                            expected == FALSE & in_data == TRUE  ~ "Additional"),
                  ind_combo = stringr::str_glue("{indicator_edms}: {sex}|{age} [{source}]"))

  #check if anything is missing
  missing <- df_status %>%
    dplyr::filter(status == "Missing") %>%
    dplyr::pull()

  #check if anything is extra
  additional <- df_status %>%
    dplyr::filter(status == "Additional") %>%
    dplyr::pull()

  # Return warning message
  if (length(missing) > 0 || length(additional) > 0) {
    cli::cli_warn(c(
      if (length(missing) > 0) "The following {length(missing)} expected item{?s} {?is/are} missing from the EDMS output:" else NULL,
      if (length(missing) > 0) stats::setNames(missing, rep("*", length(missing))) else NULL,
      if (length(additional) > 0) "The following {length(additional)} additional item{?s} {?is/are} included in the EDMS output:" else NULL,
      if (length(additional) > 0) stats::setNames(additional, rep("*", length(additional))) else NULL
    ))
  }

}


