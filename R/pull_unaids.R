#' @title Pull clean UNAIDS 2021 Estimates
#'
#' @description Pull clean UNAIDS 2021 (1990-2020) estimates
#'
#' @param sheetname returns the tab name from the Google Sheet
#' eg "HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent"
#' @param pepfar_only return dataset of only PEPFAR countries if TRUE (defualt = FALSE)
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_unaids(sheetname = "HIV Estimates - Integer", pepfar_only = TRUE)
#' }
#'

pull_unaids <- function(sheetname, pepfar_only = FALSE) {

  if(pepfar_only == TRUE) {
    google_id = pepfar_clean_id
  }

  else {
    google_id = gs_clean_id
  }

  df <- googlesheets4::range_speedread(google_id, sheetname)
  return(df)

}
