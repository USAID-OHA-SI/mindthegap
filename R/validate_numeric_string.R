#' Validate Numberic String
#'
#' Ensure that there are not extract character values that are accounted for and
#' handled during the munging process. Those values include: "<", ">", ".", " ",
#' "m", "k". To resolve, address these exceptions in convert_numeric_string()
#' and add them to the `expected_chars` list in validate_numeric_string().
#'
#' @param df dataframe
#' @keywords internal
#'
validate_numeric_string <- function(df){

  non_numeric_chars <- df$formatted %>%
    stringr::str_extract_all("[^0-9]") %>%  # Extract all non-digit characters
    unlist() %>%                            # Flatten the list to a vector
    unique()                                # Keep only unique characters

  expected_chars <- c("<", ">", ".", " ", "m", "k")

  unk_chars <- setdiff(non_numeric_chars, expected_chars)

  if(length(unk_chars) > 0)
    cli::cli_abort(c("The value column {.field format} has non-numeric characters outside those that are handled. Unexpected results may occur.",
                    "Non-numeric values handled/accounted for: {.val {expected_chars}}",
                    "Non-numeric values in dataset not handled: {.val {unk_chars}}"))
}
