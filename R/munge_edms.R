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

  df <- subset_cols(df)

  df <- munge_components(df)

  df <- munge_country(df)

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



#' Remove extraneous columns
#'
#' Keep only necessary columns that are required for output or are needed in the
#' process.
#'
#' @param df imported dataframe
#'
#' @keywords internal
#'
subset_cols <- function(df){

  #validate structure/columns
  validate_columns(df)

  #subset dataset to just key columns
  df <- dplyr::select(df, dplyr::all_of(req_cols))

  return(df)
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

#' Clean up indicator and disaggregate columns
#'
#' @param df datafrane
#' @keywords internal
#'
munge_components <- function(df){

 #parse indicator name
  df <- parse_indicator(df)

  #clean up age and sex
  df <- df %>%
    dplyr::mutate(age = ifelse(age == "allAges", "All", age),
                  sex = dplyr::case_match(sex,
                                          "M+F" ~ "All",
                                          "F" ~ "Female",
                                          "M" ~ "Male"))

  #map acronyms to clean indicator names
  df <- df %>%
    dplyr::left_join(indicator_map %>%
                         dplyr::distinct(indicator, acronym),
                       by = "acronym") %>%
    dplyr::select(-acronym)

  #reoder & drop unnecessary indicator related columns
  df <- df %>%
    dplyr::relocate(indicator, .before = indicator_edms) %>%
    dplyr::select(-c(e_cat, e_ind, indicator_edms))

  #add indicator type
  df <- df %>%
    dplyr::mutate(indicator_type = ifelse(stringr::str_detect(indicator, "Percent") | indicator == "Incidence (per 1,000)", "Percent", "Integer"),
                  .after = indicator)

  return(df)

}

#' Parse indicator
#' Standardize indicator across years and other components (eg bounds)
#'
#' @param df dataframe
#' @keywords internal
parse_indicator <- function(df){

  if(!"e_ind" %in% names(df))
    cli::cli_abort("Cannot find {.var e_ind} in the data frame to parse.")

  df %>%
    dplyr::mutate(indicator_edms = e_ind %>%
                    stringr::str_extract("(?<=-\\s).*") %>%
                    stringr::str_remove("(;| \\(| Male| Female| - Lower| - Upper).*"),
                  .before = e_ind)
}


#' Munge PEPFAR country
#'
#' Apply PEPFAR naming conventions where applicable and flag the countries that
#' received PEPFAR funding.
#'
#' @param df dataframe
#' @keywords internal
munge_country <- function(df){

  #table of PEPFAR countries
  df_pepfar <-  glamr::pepfar_country_list %>%
    dplyr::select(country_pepfar = country, iso3 = country_iso)

  #map PEPFAR countries onto df, use the PEFPAR name in place of UN default
  df <- df %>%
    dplyr::left_join(df_pepfar, by = "iso3") %>%
    dplyr::mutate(country = ifelse(is.na(country_pepfar), e_count, country_pepfar),
                  pepfar = !is.na(country_pepfar)) %>%
    dplyr::select(-c(country_pepfar, e_count)) %>%
    dplyr::rename(iso = iso3)

  #validate all PEFPAR countries are in dataframe
  validate_countries(df)

  return(df)
}


#' Validate PEPFAR countries
#'
#' Ensure there are no missing PEPFAR countries from the EDMS pull
#'
#' @param df dataframe
#' @keywords internal
validate_countries <- function(df){

  pepfar_countries_expected <- glamr::pepfar_country_list$country

  pepfar_countries_edms <- df %>%
    dplyr::distinct(country, pepfar) %>%
    dplyr::filter(pepfar == TRUE) %>%
    dplyr::pull(country)

  missing <- setdiff(pepfar_countries_expected, pepfar_countries_edms)

  if(length(missing) > 0)
    cli::cli_warn(c("{length(missing)} countr{?y/ies} {?is/are} missing from the EDMS data pull.",
                    i = "Missing countr{?y/ies}: {.val {missing}}"))

}


#' Pivot Values
#' Spread estimates and bounds to their own columns. Create a flag where
#' estimate use < or >. Handle character truncation (eg "m").
#'
#' @param df dataframe
#' @keywords internal
spread_values <- function(df){

  #spread estimate and bounds to own columns
  df <- df %>%
    dplyr::mutate(other = dplyr::case_match(other,
                                            "lb" ~ "lower_bound",
                                            "ub" ~ "upper_bound",
                                            .default = "estimate")) %>%
    tidyr::pivot_wider(names_from = other,
                       values_from = formatted)

  #add flag and clean up characters in numeric columns
  df <- df %>%
    dplyr::mutate(estimate_flag = ifelse(stringr::str_detect(estimate, "<|>"), TRUE, FALSE)) %>% #estimate flag
    dplyr::mutate(dplyr::across(c(estimate:upper_bound), ~ gsub(" |<|>", "", .x))) %>% #replace special characters
    dplyr::mutate(dplyr::across(c(estimate:upper_bound), ~ gsub("m","00000", .x))) %>% #replace unit values
    dplyr::mutate(dplyr::across(c(estimate:upper_bound), ~ ifelse(grepl("\\.\\d+00000$", .x), gsub("\\.", "", .x), .x)))


  #convert values to numeric (handling percent and integers differently)
  df <- df %>%
    dplyr::mutate(dplyr::across(estimate:upper_bound, as.numeric),
                  dplyr::across(estimate:upper_bound, ~dplyr::case_when(indicator_type == "Integer" ~ round(.x),
                                                                        indicator_type == "Percent" ~ .x))
    )

  return(df)
}



