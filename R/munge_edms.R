#' Clean UNAIDS Data from EDMS
#'
#' @param path fileoath to the EDMS export (csv)
#'
#' @return df
#' @export
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
#' @inheritParams munge_edms
#'
#' @keywords internal
read_edms <- function(path){

  #validate the file is a csv and exists
  validate_path(path)

  #import csv with column specifications
  df <- readr::read_csv(path,
                        col_types = list(Time = "d",
                                         Value = "d",
                                         Rounded = "d",
                                         .default = "c"),
                        name_repair = "universal_quiet")

  #convert to lowercase
  df <- dplyr::rename_all(df, tolower)

  #validate structure/columns
  validate_columns(df)

  return(df)
}

#' Validate Path
#'
#' Validate file path to EDMS output is a csv and exits.
#'
#' @inheritParams munge_edms
#'
#' @keywords internal
#'
validate_path <- function(path){

  if(tools::file_ext(path) != "csv")
    cli::cli_abort(c("Expecting user to provide a csv as the {.var path}.",
                   i = "Please provide a csv file."))

  if(!file.exists(path))
    cli::cli_abort(c("File does not exist: {.file {path}}."))
}

#' Validate Columns
#'
#' Ensure that all the columns that are needed exists in the import
#'
#' @param df imported dataframe
#' @keywords internal
#'
validate_columns <- function(df){

  #validation to ensure all columns exist
  if(!all(req_cols %in% names(df))){
    col_missing <- setdiff(req_cols, names(df))
    cli::cli_abort("The dataframe is missing {length(col_missing)} key column{?s} needed: {.field {col_missing}}")
  }

}
