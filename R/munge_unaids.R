#' @title Cleaning UNAIDS Data
#' @description
#' (Updated July 2023)
#'
#'
#' @description This function fetches and cleans UNAIDS Estimates/Test and Treat Data
#' @param return_type Returns either 'HIV Estimates' or 'HIV Test & Treat' Data
#' @param indicator_type Returns either 'Integer' or 'Percent' indicator values

#' @return df
#' @export
#'
#' @examples
#'  \dontrun{
#'    munge_unaids(return_type = "HIV Test & Treat", indicator_type = "Percent")
#' }
#'
munge_unaids <- function(return_type, indicator_type) {


  if (!requireNamespace('googlesheets4', quietly = TRUE))
    cli::cli_abort("{.pkg googlesheets4} needed for this function to work. Please install it.")

  # Google Sheet ID to original
  #sheet_id <- googledrive::as_id("1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs")

  #sheet_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"

  #Read Data from googlesheet and validate columns
  gdrive_df <- read_rename(return_type)

  #Munge
  gdrive_df_clean <-
    gdrive_df %>%
    #dplyr::mutate(dplyr::across(tidyselect::contains("_"), ~gsub(" |<|>", "", .))) %>%
    #dplyr::mutate(dplyr::across( tidyselect::contains("_"), as.numeric)) %>%
    dplyr::mutate(region = ifelse(country %in% regions, country, NA)) %>%
    tidyr::fill(region) %>% #get regions column
    tidyr::pivot_longer(-c(year, iso, country, region),
                        names_to = c("indicator")) %>%
    tidyr::separate(indicator, sep = "_", into = c("indicator", "age", "sex", "stat")) %>%
    tidyr::pivot_wider(names_from = 'stat', values_from = "value") %>%
    dplyr::mutate(estimate_flag = ifelse(stringr::str_detect(est, "<|>"), TRUE, FALSE)) %>% #estimate flag
    dplyr::mutate(dplyr::across(c(est:high), ~gsub(" |<|>", "", .))) %>% #replace special characters
    dplyr::mutate(dplyr::across(c(est:high), ~ gsub("m","00000", .x))) %>% #replace unit values
    dplyr::mutate(dplyr::across(c(est:high), ~ ifelse(grepl("\\.\\d+00000$", .x), gsub("\\.", "", .x), .x)))

  #Add sheet and indicator type variable
  gdrive_df_clean <- gdrive_df_clean %>%
    dplyr::mutate(sheet = return_type,
                  sex = ifelse(indicator == "pmtct", "female", sex),
                  indic_type = dplyr::case_when(
                    indicator %in% c("prev", "incidence", # HIV Estimates indicators
                                     "knownstatus", "plhivOnArt", "knownstatusOnArt",
                                     "plhivVLS", "onArtVLS", "pmtctArtPct" #T&T indicaotrs
                    ) ~ "percent_indics",
                    TRUE ~ "integer_indics"
                  )
    )

  #Recode indicators and rename columns
  gdrive_df_clean <- gdrive_df_clean %>%
    dplyr::mutate(indicator = dplyr::recode(indicator,
                                            "prev" = "Percent Prevalence",
                                            "deaths" = "Number AIDS Related Deaths",
                                            "plhiv" = "Number PLHIV",
                                            "incidence" = "Percent Incidence",
                                            "pmtct" = "Number PMTCT Needing ART",
                                            "newhiv" = "Number New HIV Infections",
                                            "knownstatus" = "Percent Known Status of PLHIV", #T&T indicators
                                            "plhivOnArt" = "Percent on ART of PLHIV",
                                            "knownstatusOnArt" = "Percent on ART with Known Status",
                                            "plhivVLS" = "Percent VLS of PLHIV",
                                            "onArtVLS" = "Percent VLS on ART",
                                            "knownstatusNum" = "Number Known Status of PLHIV",
                                            "onArtNum" = "Number on ART of PLHIV",
                                            "vlsNum" = "Number VLS of PLHIV",
                                            "pmtct" = "PMTCT", #what to call this
                                            "pmtctArt" = "Number PMTCT on ART",
                                            "pmtctArtPct" = "Percent PMTCT on ART")) %>%
    dplyr::rename(estimate = est,
           lower_bound = low,
           upper_bound = high)

  #add PEPFAR grouping category
  gdrive_df_clean <-  pepfar %>%
    dplyr::select(countryname = country_pepfar, iso = iso3) %>%
    dplyr::left_join(gdrive_df_clean, ., by = "iso") %>%
    dplyr::mutate(country = ifelse(is.na(countryname), country, countryname),
                  pepfar = ifelse(is.na(countryname), FALSE, TRUE)) %>%
    dplyr::select(-countryname)

#Export final df
  final_df <- suppressWarnings(
    gdrive_df_clean %>%
      dplyr::mutate(across(estimate:upper_bound, ~as.numeric(.x)),
                    indic_type =stringr::str_remove(indic_type, "_indics") %>% stringr::str_to_title(),
                    across(estimate:upper_bound, ~dplyr::case_when(indic_type == "Integer" ~ round(.x),
                                                                   indic_type == "Percent" ~ .x)),
                    across(age:sex, ~stringr::str_to_title(.x))) %>%
      dplyr::filter(indic_type == indicator_type)

  )

  return(final_df)

}
