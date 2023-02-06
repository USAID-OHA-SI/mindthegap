#' @title Pull clean UNAIDS 2022 Estimates
#'
#' @description Pull clean UNAIDS 2022 (1990-2021) estimates
#'
#' @param orginal_unaids if TRUE, navigates to the original UNAIDS data received in July 2022.
#' If FALSE, navigates to the additional epicontrol, cascade, and prev indicators received in August 2022
#' @param data_type returns the available data set types
#' eg "HIV Estimates", "HIV Test & Treat" for original UNAIDS data (original_unaids = TRUE) or "epicontrol",
#' "cascade", "prev" for additional UNAIDS data (original_unaids = FALSE)
#' @param pepfar_only filters dataset to only PEPFAR countries if TRUE (default = TRUE)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
#' }
#'

#if extra_unaids_indics is true, the data_type params are: "epicontrol", "cascade", "prev"
#if extra_unaids_indics is false, the data_type param is normal ("HIV Esimates", "HIV Test & Treat")

pull_unaids <- function(orginal_unaids = TRUE, data_type, pepfar_only = TRUE) {

  temp_folder <- glamr::temp_folder()

  if(orginal_unaids == FALSE) {
    filename <- glue::glue("UNAIDS_{data_type}_2022.csv.gz")
    version_tag <- "v2022.08.10"
  } else {
    filename <- glue::glue("UNAIDS_2022_Clean_Estimates.csv.gz")
    version_tag <- "v2022.07.27"
  }

  #download a specific file - test
  piggyback::pb_download(file = filename,
              repo = "USAID-OHA-SI/mindthegap",
              tag = version_tag,
              dest = temp_folder)

  df <- temp_folder %>%
    glamr::return_latest() %>%
    readr::read_csv() %>%
    dplyr::filter(pepfar == pepfar_only)

  if(orginal_unaids == TRUE) {
    df <- df %>%
      dplyr::filter(sheet == data_type)
  }

  return(df)
}
