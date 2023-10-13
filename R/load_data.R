
#' Load data for viz
#'
#' Deprecated
#'
#' @param usaid_email USAID email to store file on GDrive
#'
#' @export
#' @importFrom googlesheets4 as_sheets_id
#'
#' @examples
#' \dontrun{
#'    load_data("twhitmore@usaid.gov")}

load_data <- function(usaid_email = NULL){

  lifecycle::deprecate_stop("1.1.0", "load_data()", "pull_unaids()")

  #if not already authenticated, use email or prompt user
  if(!googlesheets4::gs4_has_token() & !is.null(usaid_email))
    googlesheets4::gs4_auth(usaid_email)
  if(!googlesheets4::gs4_has_token() & is.null(usaid_email))
    googlesheets4::gs4_auth()

  #read impatt into global environment
  df_impatt <<-
    suppressMessages(
      googlesheets4::read_sheet(as_sheets_id(gs_id), "ARTshare") %>%
        dplyr::arrange(desc(ageasentered)) %>%
        dplyr::mutate(ageasentered = forcats::as_factor(ageasentered))
    )

  #read unaids into global environment
  df_unaids <<-
    suppressMessages(
      googlesheets4::read_sheet(as_sheets_id(gs_id), "UNAIDS") %>%
        dplyr::mutate(year = as.integer(year))
    )

}

