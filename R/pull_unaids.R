#' @title Pull clean UNAIDS HIV Estimates and Test & Treat Data
#'
#' @description Pull clean UNAIDS estimates
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

  temp_folder <- glamr::temp_folder(quiet = TRUE)

  if (pepfar_only == TRUE) {
    filename <- glue::glue("UNAIDS_{unaids_year+1}_Clean_Estimates_PEPFAR-only.csv.gz")
  } else {
    filename <- glue::glue("UNAIDS_{unaids_year+1}_Clean_Estimates.csv.gz")
  }

  #download a specific file - test
  piggyback::pb_download(file = filename,
              repo = "USAID-OHA-SI/mindthegap",
              tag = "v2024.11.01", #sending to the latest release
              dest = temp_folder,
              show_progress = FALSE)

  df <- temp_folder %>%
    glamr::return_latest(quiet = TRUE) %>%
    readr::read_csv(
      col_types = list(
        year = "d",
        estimate = "d",
        lower_bound = "d",
        upper_bound = "d",
        estimate_flag = "l",
        pepfar = "l",
        achv_95_plhiv = "l",
        achv_95_relative = "l",
        epi_control = "l",
        epi_ratio_2023 = "d",
        .default = "c")
    )

  if(missing(data_type)){
    df <- dplyr::filter(df, !(indicator == "Number PMTCT Needing ART" & sheet == "HIV Test & Treat"))
  } else {
    df <- dplyr::filter(df, sheet == data_type)
  }

  return(df)
}


#' Pull clean UNAIDS HIV Estimates
#'
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

  pull_unaids("HIV Estimates", pepfar_only)

}

#' Pull clean UNAIDS HIV Test & Treat
#'
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

  pull_unaids("HIV Test & Treat", pepfar_only)

}
