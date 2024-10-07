#' Load tidy UNAIDS HIV Estimates and Test & Treat Data
#'
#' This function is used to load the latest UNAIDS data. These data were queried
#' and exported from [UNAIDS EMDS database](https://edms.unaids.org/), cleaned
#' using `munge_edms` and finally loaded up on GitHub as a release. This
#' function downloads the data and loads it into your working session.
#'
#' @param pepfar_only filters dataset to only PEPFAR countries if TRUE (default = TRUE)
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'    df_unaids <- load_unaids(pepfar_only = TRUE)
#' }
#'
load_unaids <- function(pepfar_only = TRUE){

  #id folder for downloading file into
  temp_folder <- tempdir()

  #file name to match release (PEPFAR only or all)
  filename <- glue::glue("UNAIDS_{unaids_year+1}_Clean_Estimates_PEPFAR-only.csv.gz")
  if (pepfar_only == FALSE)
    filename <- glue::glue("UNAIDS_{unaids_year+1}_Clean_Estimates.csv.gz")

  #download a specific file
  piggyback::pb_download(file = filename,
                         repo = "USAID-OHA-SI/mindthegap",
                         tag = "latest",
                         dest = temp_folder,
                         show_progress = FALSE)

  #read in data
  df <- readr::read_csv(file = file.path(temp_folder, filename),
                        col_types = list(
                          year = "d",
                          estimate = "d",
                          lower_bound = "d",
                          upper_bound = "d",
                          estimate_flag = "l",
                          pepfar = "l",
                          achv_95_plhiv = "l",
                          achv_95_relative = "l",
                          .default = "c"),
                        progress = FALSE
  )

  #account for structural change post v2.0
  df <- handle_historic(df)

  return(df)

}

#' Handle the import of historic data
#'
#' The structure adjusted in v2.0 (after 2024-10-01). This function provides the
#' handling of column names that have changed
#'
#' @param df dataframe
#' @keywords internal
#'
handle_historic <- function(df){

  #handing to account for data structure prior to v2.0 (2024-10-01)
  if("achv_epi_control" %in% names(df))
    df <- dplyr::mutate(df, achv_epi_control = as.logical(achv_epi_control))

  if("epi_control" %in% names(df))
    df <- dplyr::mutate(df, epi_control = as.logical(epi_control))
  if("epi_ratio_2023" %in% names(df))
    df <- dplyr::mutate(df, epi_ratio_2023 = as.double(epi_ratio_2023))

  if("indic_type" %in% names(df))
    df <- dplyr::rename(df, "indicator_type" = "indic_type")
  if("epi_control" %in% names(df))
    df <- dplyr::rename(df, "achv_epi_control" = "epi_control")

  if("sheet" %in% names(df) && unique(df[df$indicator=="Number PMTCT Needing ART",]$sheet) > 1)
    df <- dplyr::filter(df, !(indicator == "Number PMTCT Needing ART" & sheet == "HIV Test & Treat"))

  return(df)
}

#' Pull clean UNAIDS HIV Estimates and Test & Treat Data
#'
#' Deprecated. Pull clean UNAIDS estimates
#'
#' @param data_type returns one of 2  available data set types
#' eg "HIV Estimates", "HIV Test & Treat" (or both if missing)
#' @param pepfar_only filters dataset to only PEPFAR countries if TRUE (default = TRUE)
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
#' }
#'

pull_unaids <- function(data_type, pepfar_only = TRUE) {

  lifecycle::deprecate_warn("2.0.0", "pull_unaids()", "load_unaids()")

  cli::cli_alert_warning("Ignoring {.arg data_type} when using {.fn load_unaids}")

  load_unaids(pepfar_only)

}


#' Pull clean UNAIDS HIV Estimates
#'
#' Deprecated.
#' Pull clean UNAIDS estimates data. Wrapper around
#' `pull_unaids`.
#'
#' @inheritParams pull_unaids
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_estimates()
#' }
pull_estimates <- function(pepfar_only = TRUE){

  lifecycle::deprecate_warn("2.0.0", "pull_estimates()", "load_unaids()")

  cli::cli_alert_warning("Returning full UNAIDS dataset.")

  load_unaids(pepfar_only)

}

#' Pull clean UNAIDS HIV Test & Treat
#'
#' Deprecated.
#' Pull clean UNAIDS 2022 (1990-2022) HIV Test & Treat data. Wrapper around
#' `pull_unaids`.
#'
#' @inheritParams pull_unaids
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_testtreat()
#' }
pull_testtreat <- function(pepfar_only = TRUE){

  lifecycle::deprecate_warn("2.0.0", "pull_estimates()", "load_unaids()")

  cli::cli_alert_warning("Returning full UNAIDS dataset.")

  load_unaids(pepfar_only)

}
