#' Year UNAIDS data are released through
#'
#' @keywords internal
#'
unaids_year <- 2023

#' UNAIDS Source Info
#'
#' Returns UNAIDS Source info for consistent sourcing notes
#'
#' @export
#'

source_note <- glue::glue("Source: UNAIDS EDMS Global Data {unaids_year + 1} Release") %>% as.character()


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


#' Import function for reading original UNAIDS estimates
#'
#' @param return_type Either "HIV Estimates" or "HIV Test & Treat"
#' @keywords internal
#' @return returns df to pass into munge_unaids()
read_rename <- function(return_type) {

  #to specify NA's when reading in data
  missing <- c("...", " ")

  sheetname = ifelse(return_type == "HIV Estimates", "HIV2024Estimates_ByYear", "HIV-Test-&-Treat_ByYear")
  skip_n = ifelse(sheetname == "HIV2024Estimates_ByYear", 6, 4)

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
        dplyr::mutate(across(dplyr::where(is.list), ~dplyr::na_if(., "NULL"))) %>%
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


#' UN Defined Regions
#'
#' This list is used to filter the UN regions
#'
#' @export
#' @keywords internal
#'
regions <- c("Global",
             "Asia and the Pacific",
             "Caribbean",
             "Eastern and southern Africa",
             "Eastern Europe and central Asia",
             "Latin America",
             "Middle East and North Africa",
             "Western and central Africa",
             "Western and central Europe and North America")

#' Start up message
#' @keywords internal

startup_msg <- function() {
  cli::cli_inform("To pull UNAIDS time series estimates, use {.fun load_unaids}. See function documentation ({.code ?load_unaids}) for further information.",
                  class = "packageStartupMessage")

}
