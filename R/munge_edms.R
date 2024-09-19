#' Clean UNAIDS Data from EDMS
#'
#' @param path fileoath to the EDMS export (csv)
#'
#' @return
#' @export dataframe
#'
#' @examples
#'  \dontrun{
#'   filepath <- "../DataList_10_1_2030-12_00_00-AM.csv"
#'   df <- munge_edms(filepath)
#' }
munge_edms <- function(path){

  df <- read_edms(path)

  return(df)

}

#' Import EDMS Export
#'
#' Read in the data that has been downloaded from the
#' [UNAIDS EDMS Database](https://edms.unaids.org/)
#'
#' @param path
#'
#' @keywords internal
read_edms <- function(path){

  validate_path(path)

  df <- readr::read_csv(path,
                        col_types = list(Time = "d",
                                         Value = "d",
                                         Rounded = "d",
                                         .default = "c"),
                        name_repair = "universal_quiet") %>%
    dplyr::rename_all(tolower)
}

#' Validate Path
#'
#' @param path
#'
#' @keywords internal
validate_path <- function(path){

  if(tools::file_ext(path) != "csv")
    cli::cli_abort(c("Expecting user to provide a csv as the {.var path}.",
                   i = "Please provide a csv file."))

  if(!file.exists(path))
    cli::cli_abort(c("File does not exist: {.file {path}}."))
}
