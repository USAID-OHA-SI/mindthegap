#' Clean UNAIDS Data from EDMS
#'
#' @param path filepath to the EDMS export (csv)
#' @param epi_95s_flag add variables to dataframe for UNAIDS 95s achievement and
#'   epidemic control status, default = TRUE
#'
#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'   filepath <- "../DataList_10_1_2030-12_00_00-AM.csv"
#'   df <- munge_edms(filepath)
#' }
munge_edms <- function(path, epi_95s_flag = TRUE){

  df <- read_edms(path)

  df <- subset_cols(df)

  df <- munge_components(df, epi_95s_flag)

  df <- munge_country(df)

  df <- spread_values(df)

  df <- convert_numeric_string(df)

  df <- clean_cols(df)

  df <- flag_95s(df, epi_95s_flag)

  df <- flag_epi(df, epi_95s_flag)

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
  validate_columns(df, req_cols)

  #subset dataset to just key columns
  df <- dplyr::select(df, dplyr::all_of(req_cols))

  return(df)
}



#' Clean up indicator and disaggregate columns
#'
#' @inheritParams munge_edms
#' @param df dataframe
#' @keywords internal
#'
munge_components <- function(df, epi_95s_flag = TRUE){

  #parse indicator name
  df <- parse_indicator(df)

  #validate indicators/disaggregates
  validate_ind_disaggs(df, epi_95s_flag)

  #clean up age and sex
  df <- standarize_agesex(df)

  #map acronyms to clean indicator names
  df <- map_indicator(df)

  #add indicator type
  df <- apply_indicator_type(df)

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


#' Standardize Age/Sex
#'
#' @param df dataframe
#' @keywords internal
#'
standarize_agesex <- function(df){

  df %>%
    dplyr::mutate(age = ifelse(age == "allAges", "All", age),
                  sex = dplyr::case_match(sex,
                                          "M+F" ~ "All",
                                          "F" ~ "Female",
                                          "M" ~ "Male"))
}

#' Map working indicator name to EDMS indicator name
#'
#' @param df dataframe
#' @keywords internal

map_indicator <- function(df){

  #table of acronyms with the working indicator name to join onto df
  df_acronym <- indicator_map %>%
    dplyr::distinct(indicator, acronym)

  #map acronyms to clean indicator names
  df <- dplyr::left_join(df, df_acronym, by = "acronym")

  #add indicator_edms as indicator if missing from mapping list
  df <- df %>%
    dplyr::mutate(indicator = ifelse(is.na(indicator), indicator_edms, indicator),
                  .before = indicator_edms)

  #drop unnecessary indicator related columns
  df <- df %>%
    dplyr::select(-c(e_cat, e_ind, indicator_edms, acronym))

  return(df)
}


#' Apply Indicator Type
#'
#' Specify if the indicator's value is a percent, integer, or rate
#'
#' @param df dataframe
#' @keywords internal
#'
apply_indicator_type <- function(df){
  df %>%
    dplyr::mutate(indicator_type =
                    dplyr::case_when(stringr::str_detect(indicator, "Incidence|(P|p)revalence") ~ "Rate",
                                     stringr::str_detect(indicator, "IMR") ~ "Ratio",
                                     stringr::str_detect(indicator, "Percent") ~ "Percent",
                                     stringr::str_detect(indicator, "Number") ~ "Integer",
                                     TRUE ~ "Unknown"),
                  .after = indicator)
}



#' Munge PEPFAR country
#'
#' Apply PEPFAR naming conventions where applicable and flag the countries that
#' received PEPFAR funding.
#'
#' @param df dataframe
#' @keywords internal
munge_country <- function(df){

  #map PEPFAR countries onto df, use the PEFPAR name in place of UN default
  df <- df %>%
    dplyr::left_join(pepfar, by = "iso3") %>%
    dplyr::mutate(country = ifelse(is.na(country_pepfar), e_count, country_pepfar),
                  pepfar = !is.na(country_pepfar)) %>%
    dplyr::select(-c(country_pepfar, e_count)) %>%
    dplyr::rename(iso = iso3)

  #validate all PEFPAR countries are in dataframe
  validate_countries(df)

  return(df)
}


#' Pivot Values
#' Spread estimates and bounds to their own columns. Create a flag where
#' estimate use < or >. Handle character truncation (eg "m").
#'
#' @param df dataframe
#' @keywords internal
spread_values <- function(df){

  #validate non-numeric characters are not outside those that will be handled
  validate_numeric_string(df)

  #spread estimate and bounds to own columns
  df <- df %>%
    dplyr::mutate(other = dplyr::case_match(other,
                                            "lb" ~ "lower_bound",
                                            "ub" ~ "upper_bound",
                                            .default = "estimate")) %>%
    tidyr::pivot_wider(names_from = other,
                       values_from = formatted)


  #check if bounds exist in dataframe
  missing_bounds <- setdiff(c("lower_bound", "upper_bound"), names(df))

  #create new columns if bounds don't exist
  df[missing_bounds] <- NA_character_

  return(df)
}


#' Convert Numeric Values stored as string
#'
#' @param df dataframe
#' @keywords internal
#'
convert_numeric_string <- function(df){

  #numeric columns
  num_cols <- c("estimate", "lower_bound", "upper_bound")

  #add flag where there there is a < or > sign
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols),
                                \(x) x %>%
                                  stringr::str_extract("<|>") %>%
                                  dplyr::case_match("<" ~ "less than",
                                                    ">" ~ "greater than"),
                                .names = "{.col}_flag"))

  #remove special characters (spaces and less/greater than) in numeric columns
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), \(x) stringr::str_remove(x, " |<|>")))

  #convert units from string with m/k to numeric)
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ dplyr::case_when(stringr::str_detect(.x, "m") ~ 1e6,
                                                                            stringr::str_detect(.x, "k") ~ 1e3,
                                                                            TRUE ~ 1),
                                .names = "{.col}_units"),
                  dplyr::across(dplyr::all_of(num_cols), ~ stringr::str_remove_all(.x, "m|k")),
                  dplyr::across(dplyr::all_of(num_cols), ~ as.numeric(.x) * get(paste0(dplyr::cur_column(), "_units")),
                                .names = "{.col}")
                  ) %>%
    dplyr::select(-dplyr::ends_with("_units"))

  #round integers to whole numbers but not percents or rates
  df <- df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ dplyr::case_when(indicator_type == "Integer" ~ round(.x),
                                                                            indicator_type == "Ratio"~ round(.x, 2),
                                                                            TRUE ~ .x))
    )

  return(df)
}

#' Clean Columns
#' Reorder and rename columns
#'
#' @param df dataframe
#' @keywords internal
#'
clean_cols <- function(df){

  #reorder and rename columns
  df %>%
    dplyr::select(year = time,
                  iso,
                  country,
                  pepfar,
                  region,
                  indicator,
                  indicator_type,
                  age,
                  sex,
                  estimate,
                  lower_bound,
                  upper_bound,
                  estimate_flag)

}

#' Flag 95 Achievement
#'
#' Add flags for achievement across all three 95s (for both relative and PLHIV
#' base) across country, year, age, and sex.
#'
#' @param df dataframe
#' @inheritParams munge_edms
#' @keywords internal
#'
flag_95s <- function(df, epi_95s_flag = TRUE){

  #exit if there is no desire to add flags to dataset
  if(epi_95s_flag == FALSE)
    return(df)

  #UNAIDS GOAL
  goal <- 95

  #columns needed
  key_cols_95s <- c("year", "iso", "indicator", "age", "sex")

  #indicators needed
  key_ind_95s <- c("Percent Known Status of PLHIV",
                   "Percent on ART of PLHIV",
                   "Percent VLS of PLHIV",
                   "Percent on ART with Known Status",
                   "Percent VLS on ART")

  #exit if all are missing
  missing <- setdiff(key_ind_95s, unique(df$indicator))
  if(length(missing) == length(key_ind_95s)){
      cli::cli_warn(c(x = "95s target achievement flags cannot be created.",
                      i = "The dataset is missing: {.val {missing}}"))
      return(df)
  }

  #warn if some are missing
  if(length(missing) > 0){
    cli::cli_warn(c("{length(missing)} out of {length(key_ind_95s)} indicators are missing for calculating 95s achievement",
                    i = "The dataset is missing: {.val {missing}}"))
  }

  #subset dataset to what is needed
  df_achv <- df %>%
    dplyr::filter(indicator %in% key_ind_95s) %>%
    dplyr::select(dplyr::all_of(key_cols_95s), estimate) %>%
    dplyr::filter(!is.na(estimate))

  #create achievement goals for each indicator in both bases
  df_achv <- df_achv %>%
    dplyr::mutate(set = dplyr::case_match(indicator,
                                          "Percent on ART of PLHIV" ~ 2,
                                          "Percent VLS of PLHIV" ~ 3,
                                          .default = 1),
                  base = dplyr::case_when(indicator == "Percent Known Status of PLHIV" ~ "Both",
                                          set == 1 ~ "Relative",
                                          TRUE ~ "PLHIV"),
                  goal_rate = (goal/100)^set*100,
                  achv_plhiv = dplyr::case_when(base != "Relative" ~ estimate >= goal_rate),
                  achv_relative = dplyr::case_when(base != "PLHIV" ~ estimate >= goal_rate))

  #full achievement?
  df_achv <- df_achv %>%
    dplyr::group_by(year, iso, age, sex) %>%
    dplyr::mutate(achv_95_plhiv = sum(achv_plhiv, na.rm = TRUE) == 3,
                  achv_95_relative = sum(achv_relative, na.rm = TRUE) == 3) %>%
    dplyr::ungroup()

  #clean up dataset
  df_achv <- df_achv %>%
    dplyr::select(dplyr::all_of(key_cols_95s), -indicator,
                  dplyr::starts_with("achv_95")) %>%
    dplyr::distinct()


  #join onto main dataframe (will not join with Age/Sex pairs that don't exist for the 95s)
  df_j <- dplyr::left_join(df,
                           df_achv,
                           by = dplyr::join_by(year, iso, age, sex))


  return(df_j)

}

#' Flag Epi contol status
#'
#' Add flags for Epi contol status for all age and sex
#'
#' @param df dataframe
#' @inheritParams munge_edms
#' @keywords internal
#'
flag_epi <- function(df, epi_95s_flag = TRUE){

  #exit if there is no desire to add flags to dataset
  if(epi_95s_flag == FALSE)
    return(df)

  #indicators needed
  key_ind_epi <- c("Number Total Deaths to HIV Population",
                   "Number New HIV Infections",
                   "Incidence mortality ratio (IMR)")

  #exit if any are missing
  missing <- setdiff(key_ind_epi, unique(df$indicator))
  if(length(missing) > 0){
    cli::cli_warn(c(x = "Epi Control flags cannot be created.",
                    i = "The dataset is missing: {.val {missing}}"))
    return(df)
  }

  # subset dataset to indicator and columns needed
  df_epi <- df %>%
    dplyr::filter(indicator %in% key_ind_epi,
                  age == "All",
                  sex == "All") %>%
    dplyr::select(iso, year, indicator, age, sex, estimate)

  #spread to calculate epi control
  df_epi <- df_epi %>%
    tidyr::pivot_wider(names_from = indicator,
                       values_from = estimate,
                       names_glue = "{stringr::str_extract_all(tolower(indicator), 'deaths|infections|imr')}") %>% #clean up indicator name to make easier when reshaped to column
    dplyr::arrange(iso, year)

  #by country, check if deaths and infections are declining YoY
  df_epi <- df_epi %>%
    dplyr::group_by(iso) %>%
    dplyr::mutate(dplyr::across(c(deaths, infections),
                                \(x) x - dplyr::lag(x, order_by = year) <= 0,
                                .names = "declining_{.col}")) %>%
    dplyr::ungroup()

  #calculate epi control
  df_epi <- df_epi %>%
    dplyr::mutate(infections_below_deaths = imr < 1, #infections < deaths,
                  imr = round(imr, 2), #infections / deaths,
                  # direction_streak = sequence(rle(declining_deaths)$lengths),
                  achv_epi_control = (declining_deaths == TRUE & declining_infections == TRUE & infections_below_deaths == TRUE))

  #subset
  df_epi <- df_epi %>%
    dplyr::filter(year != min(year), !is.na(achv_epi_control)) %>%
    dplyr::select(year, iso, age, sex, achv_epi_control)

  #join onto main dataframe (will not join with Age/Sex pairs that don't exist for the IMR)
  df_j <- dplyr::left_join(df,
                           df_epi,
                           by = dplyr::join_by(year, iso, age, sex))

  return(df_j)

}




