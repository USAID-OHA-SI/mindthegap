#' Validate Columns
#'
#' Ensure that all the columns that are needed exists in the import
#'
#' @param df imported dataframe
#' @param expected_cols a string vector of the expected columns that should be
#'   in the dataframe
#'
#' @keywords internal
#'
validate_columns <- function(df, expected_cols){

  #validation to ensure all columns exist
  if(!all(expected_cols %in% names(df))){
    col_missing <- setdiff(expected_cols, names(df))
    cli::cli_abort("The dataframe is missing {length(col_missing)} key column{?s} needed: {.field {col_missing}}")
  }

}
