#' @title Pull clean UNAIDS 2022 Estimates
#'
#' @description Pull clean UNAIDS 2022 (1990-2022) estimates
#'
#' @param data_type returns one of 2  available data set types
#' eg "HIV Estimates", "HIV Test & Treat"
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
  version_tag <- "v2023.08.11"

  if (pepfar_only == TRUE) {
    filename <- glue::glue("UNAIDS_2023_Clean_Estimates_PEPFAR-only.csv.gz")
  } else {
    filename <- glue::glue("UNAIDS_2023_Clean_Estimates.csv.gz")
  }

  #download a specific file - test
  piggyback::pb_download(file = filename,
              repo = "USAID-OHA-SI/mindthegap",
              tag = version_tag,
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
        pepfar = "l",
        `Achieved 95s with PLHIV base in 2022` = "l",
        `Achieved 95s with relative base in 2022` = "l",
        epi_control = "l",
        .default = "c")
    ) %>%
    dplyr::filter(sheet == data_type)

  return(df)
}
