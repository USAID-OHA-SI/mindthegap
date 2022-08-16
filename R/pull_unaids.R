#' @title Pull clean UNAIDS 2022 Estimates
#'
#' @description Pull clean UNAIDS 2022 (1990-2021) estimates
#'
#' @param data_type returns the data type requested
#' eg "HIV Estimates", "Test & Treat"
#' @param pepfar_only return dataset of only PEPFAR countries if TRUE (default = FALSE)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_unaids(data_type = "HIV Estimates", pepfar_only = TRUE)
#' }
#'

pull_unaids <- function(sheetname, pepfar_only = FALSE) {

  if(pepfar_only == TRUE) {
    google_id = pepfar_clean_id
  }

  else {
    google_id = gs_clean_id
  }

  df <- googlesheets4::range_speedread(google_id) %>%
    dplyr::filter(sheet == sheetname)
  return(df)

}
