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
