#' @title Cleaning UNAIDS Data
#'
#' @description This function fetches and cleans UNAIDS Estimates/Test and Treat Data
#' @param return_type Returns either 'HIV Estimates' or 'HIV Test & Treat' Data
#' @param indicator_type Returns either 'Integer' or 'Percent' indicator values

#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    munge_unaids(return_type = "HIV Test & Treat", indicator_type = "Percent")
#' }
#'
munge_unaids <- function(return_type, indicator_type) {

  # Google Sheet ID to original
  #sheet_id <- googledrive::as_id("1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs")

  #sheet_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"

  gdrive_df <- read_rename(return_type)

  gdrive_df_clean <-
    gdrive_df %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("_"), ~gsub(" |<|>", "", .))) %>%
    #dplyr::mutate(dplyr::across( tidyselect::contains("_"), as.numeric)) %>%
    dplyr::mutate(regions = ifelse(country %in% regions, country, NA)) %>%
    tidyr::fill(regions) %>% #get regions column
    tidyr::pivot_longer(-c(year, iso, country, regions),
                        names_to = c("indicator")) %>%
    tidyr::separate(indicator, sep = "_", into = c("indicator", "age", "sex", "stat"))

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

  gdrive_df_clean <- gdrive_df_clean %>%
    dplyr::mutate(indicator = dplyr::recode(indicator,
                                            "prev" = "Prevalence",
                                            "deaths" = "AIDS Related Deaths",
                                            "plhiv" = "PLHIV",
                                            "incidence" = "Incidence",
                                            "pmtct" = "PMTCT",
                                            "newhiv" = "New HIV Infections",
                                            "knownstatus" = "KNOWN_STATUS", #T&T indicators
                                            "plhivOnArt" = "PLHIV_ON_ART",
                                            "knownstatusOnArt" = "KNOWN_STATUS_ON_ART",
                                            "plhivVLS" = "VLS",
                                            "onArtVLS" = "ON_ART_VLS",
                                            "knownstatusNum" = "KNOWN_STATUS",
                                            "onArtNum" = "PLHIV_ON_ART",
                                            "vlsNum" = "VLS",
                                            "pmtct" = "PMTCT", #what to call this
                                            "pmtctArt" = "PMTCT_ON_ART",
                                            "pmtctArtPct" = "PMTCT_ON_ART"))

  gdrive_df_clean <- glamr::pepfar_country_list %>%
    dplyr::select(countryname, iso = countryname_iso) %>%
    dplyr::left_join(gdrive_df_clean, ., by = "iso") %>%
    dplyr::mutate(country = ifelse(is.na(countryname), country, countryname),
                  pepfar = ifelse(is.na(countryname), "Non-PEPFAR", "PEPFAR")) %>%
    dplyr::select(-countryname)


  final_df <- suppressWarnings(
    gdrive_df_clean %>%
      dplyr::mutate(value = as.numeric(value),
                    indic_type =stringr::str_remove(indic_type, "_indics") %>% stringr::str_to_title()) %>%
      dplyr::filter(indic_type == indicator_type)

  )

  return(final_df)

}