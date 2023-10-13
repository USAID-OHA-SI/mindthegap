#' Check if package exists
#'
#' @param pkg package name
#'
#' @keywords internal

package_check <- function(pkg){

  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "needed for this function to work. Please install it."),
         call. = FALSE)
  }
}


.onAttach <- function(libname, pkgname) {

  if (.Platform$OS.type == "windows")
    grDevices::windowsFonts(GillSans = grDevices::windowsFont("Gill Sans MT"))

}

read_rename <- function(return_type) {

  #to specify NA's when reading in data
  missing <- c("...", " ")

  sheetname = ifelse(return_type == "HIV Estimates", "HIV2023Estimates_ByYear", "HIV-Test-&-Treat_ByYear")
  skip_n = ifelse(sheetname == "HIV2023Estimates_ByYear", 5, 4)

  gdrive_df <- suppressMessages(
    googlesheets4::read_sheet(gs_id_unaids, sheet = sheetname, skip = skip_n, na = missing, col_types = "c") %>%
    dplyr::rename(year = !!names(.[1]),
                  iso =  !!names(.[2]),
                  country =  !!names(.[3]))
  )

  gdrive_df <- validate_cols(gdrive_df, return_type)

  if (return_type == "HIV Test & Treat") {
    gdrive_df <-  suppressWarnings(
      gdrive_df %>%
        dplyr::mutate(across(tidyselect:::where(is.list), ~dplyr::na_if(., "NULL"))) %>%
        dplyr::slice(-c(1,2))
    )
  }

  return(gdrive_df)
}


validate_cols <- function(df, return_type) {

  sheetname <- ifelse(return_type == "HIV Estimates", "HIV estimates - by Year", "HIV Test & Treat - by Year ")

  names_cw <- suppressMessages(
    googlesheets4::read_sheet(gs_id_names, sheet = sheetname) %>%
    #dplyr::filter(sheet == "HIV Test & Treat - by Year ") %>%
    dplyr::select(-sheet) %>%
    tidyr::pivot_wider(names_from = names,
                       values_from = names_original) %>%
    dplyr::select(-value)
  )

  #change column names - stop if length of names is not the same as length of df
  stopifnot(ncol(names_cw) == ncol(df))
  names(df) <- names(names_cw)

  return(df)

}
