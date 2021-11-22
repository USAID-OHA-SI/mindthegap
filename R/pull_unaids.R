#' @title Pull clean UNAIDS 2021 Estimates for PEPFAR countries
#'
#' @description Pull clean UNAIDS 2021 (1990-2020) estimates for PEPFAR countries from Google Sheet
#'
#' @param sheetname returns the tab name from the Google Sheet
#' eg "HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent"
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    pull_unaids(sheetname = "HIV Estimates - Integer")
#' }
#'

pull_unaids <- function(sheetname) {

  #run this to get PEPFAR UNAIDS Clean data instead of the all
  file <- googledrive::drive_ls(drive_id, "PEPFAR Only - UNAIDS 2021 Clean Estimates")

  filepath <- list.files(tempdir(), "PEPFAR Only - UNAIDS 2021 Clean Estimates", full.names = TRUE)

  if(length(filepath) == 0){
    filepath <- file.path(tempdir(), "PEPFAR Only - UNAIDS 2021 Clean Estimates.xlsx")
    googledrive::drive_download(googledrive::as_id(file$id),
                                path = filepath,
                                overwrite = TRUE)
  }

   #sheetname <- readxl::excel_sheets(filepath) #get a list of sheetnames

  df <- readxl::read_excel(filepath, sheet = sheetname)

  return(df)

}
