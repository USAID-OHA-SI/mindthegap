#' Validate Path
#'
#' Validate file path to EDMS output is a csv and exits.
#'
#' @inheritParams munge_edms
#'
#' @keywords internal
#'
validate_path <- function(path){

  if(length(path) > 1)
    cli::cli_abort("The {.var path} param can only accept a vector of length 1")

  if(tools::file_ext(path) != "csv")
    cli::cli_abort(c("Expecting user to provide a csv as the {.var path}.",
                     i = "Please provide a csv file."))

  if(!file.exists(path))
    cli::cli_abort(c("File does not exist: {.file {path}}."))
}
