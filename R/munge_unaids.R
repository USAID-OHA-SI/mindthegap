#' @title Cleaning UNAIDS Data
#'
#' @description This function cleans and fetches UNAIDS Estimates/Test and Treat Data
#' @param return_type returns either HIV Estimates or Test & Treat Data
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#'    munge_unaids(sheet_id = googledrive::as_id('1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs'), tab = 3, skipnum = 4)
#' }
#'
munge_unaids <- function(return_type) {

  #to specify NA's when reading in data
  missing <- c("...", " ")

  # Get valid pepfar list
  pepfar_cntry <- glamr::get_outable() %>%
    dplyr::pull(countryname)

  # Google Sheet ID to original
  sheet_id <- googledrive::as_id("1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs")

  sheet_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"

  #HIV estimates tab - specific
  if (return_type == "HIV Estimates") {

    gdrive_df <- suppressMessages(
      googlesheets4::read_sheet(sheet_id, sheet = 1, skip = 5, na = missing) %>%
        dplyr::rename(year = !!names(.[1]),
                      iso =  !!names(.[2]),
                      country =  !!names(.[3]))
    )

    hiv_est_names <- suppressMessages(
      googlesheets4::read_sheet(sheet_id, range = "HIV estimates - by Year!A5:AY5") %>%
        dplyr::rename(year = !!names(.[1]),
                      iso =  !!names(.[2]),
                      country =  !!names(.[3]))
    )

    names_cw <- suppressMessages(
      googlesheets4::read_sheet(sheet_id_names) %>%
        dplyr::filter(sheet == "HIV estimates - by Year") %>%
        dplyr::select(-sheet) %>%
        tidyr::pivot_wider(names_from = names,
                           values_from = names_original) %>%
        dplyr::select(-value)
    )

    #change column names - stop if length of names is not the same as length of df
    stopifnot(ncol(names_cw) == ncol(gdrive_df))
    names(gdrive_df) <- names(names_cw)

  }

  #Test & Treat tab - specific
  if (return_type == "Test & Treat") {

    gdrive_df <- suppressMessages(
      googlesheets4::read_sheet(sheet_id, sheet = 3, skip = 4, na = missing) %>%
        dplyr::rename(year = !!names(.[1]),
                      iso =  !!names(.[2]),
                      country =  !!names(.[3]))
    )

    hiv_tt_names <- suppressMessages(
      googlesheets4::read_sheet(sheet_id, sheet = 3, range = "HIV Test & Treat - by Year !A5:DF5") %>%
        dplyr::rename(year = !!names(.[1]),
                      iso =  !!names(.[2]),
                      country =  !!names(.[3]))
    )

    names_cw_tt <- suppressMessages(
      googlesheets4::read_sheet(sheet_id_names, sheet = 2) %>%
        dplyr::filter(sheet == "HIV Test & Treat") %>%
        dplyr::select(-sheet) %>%
        tidyr::pivot_wider(names_from = names,
                           values_from = names_original) %>%
        dplyr::select(-value)
    )

    #change column names - stop if length of names is not the same as length of df
    stopifnot(ncol(names_cw_tt) == ncol(gdrive_df))

    names(gdrive_df) <- names(names_cw_tt)

    gdrive_df <-  suppressWarnings(
      gdrive_df %>%
        dplyr::mutate(across(tidyselect:::where(is.list), ~na_if(., "NULL"))) %>%
        dplyr::slice(-c(1,2)) %>%
        dplyr::mutate_at(dplyr::vars(4:110), as.numeric)
    )

  }


  gdrive_df_clean <-
    gdrive_df %>%
    dplyr::mutate(dplyr::across(tidyselect::contains("_"), ~gsub(" |<|>", "", .))) %>%
    #dplyr::mutate(dplyr::across( tidyselect::contains("_"), as.numeric)) %>%
    dplyr::mutate(regions = ifelse(country %in% regions, country, NA)) %>%
    tidyr::fill(regions) %>% #get regions column
    tidyr::pivot_longer(-c(year, iso, country, regions),
                        names_to = c("indicator")) %>%
    tidyr::separate(indicator, sep = "_", into = c("indicator", "age", "sex", "stat"))

  #HIV estimates tab - specific munging
  if (return_type == "HIV Estimates") {
    gdrive_df_clean <- gdrive_df_clean %>%
      dplyr::mutate(sheet = "HIV Estimates",
                    sex = ifelse(indicator == "pmtct", "female", sex),
                    indic_type = dplyr::case_when(
                      indicator %in% c("prev", "incidence") ~ "percent_indics",
                      TRUE ~ "integer_indics"
                    )
      )

    #clean up indicator names
    gdrive_df_clean <- gdrive_df_clean %>%
      dplyr::mutate(indicator = dplyr::recode(indicator,
                                              "prev" = "Prevalence",
                                              "deaths" = "AIDS Related Deaths",
                                              "plhiv" = "PLHIV",
                                              "incidence" = "Incidence",
                                              "pmtct" = "PMTCT",
                                              "newhiv" = "New HIV Infections"))
  }

  #Test and Treat tab - specific munging
  if (return_type == "Test & Treat") {
    gdrive_df_clean <- gdrive_df_clean %>%
      dplyr::mutate(sheet = "HIV Test & Treat",
                    sex = ifelse(indicator == "pmtct", "female", sex),
                    indic_type = dplyr::case_when(
                      indicator %in% c("knownstatus", "plhivOnArt", "knownstatusOnArt",
                                       "plhivVLS", "onArtVLS", "pmtctArtPct") ~ "percent_indics",
                      TRUE ~ "integer_indics"
                    ))

    #clean up indicator names
    gdrive_df_clean <- gdrive_df_clean %>%
      dplyr::mutate(indicator = dplyr::recode(indicator,
                                              "knownstatus" = "KNOWN_STATUS",
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
  }

  #adjust country names and add flag for PEPFAR countries
  gdrive_df_clean <- gdrive_df_clean %>%
    dplyr::mutate(country = dplyr::case_when(country == "Cote dIvoire" ~ "Cote d'Ivoire",
                               country == "United Republic of Tanzania" ~ "Tanzania",
                               country == "Viet Nam" ~ "Vietnam",
                               TRUE ~ country),
           pepfar = ifelse(country %in% pepfar_cntry, "PEPFAR", "Non-PEPFAR"))

  # To fix the formatting, let's return a list of tibbles w/ percentages in one and integers in the other
  hiv_est <- suppressWarnings(
    gdrive_df_clean %>%
      dplyr::group_split(indic_type) %>%
      rlang::set_names(dplyr::group_keys(gdrive_df_clean %>% dplyr::group_by(indic_type)) %>%
                         unlist() %>%
                         stringr::str_remove(., "_indics") %>%
                         stringr::str_c(" indicators") %>%
                         stringr::str_to_title()) %>%
      purrr::map(., ~.x %>% dplyr::mutate(value = as.numeric(value)))
  )

  return(hiv_est)

}
