# PROJECT: 2023 UNAIDS Estimates
# PURPOSE: Test Functions
# AUTHOR: Lemlem Baraki | SI
# REF ID:   621970ea
# LICENSE: MIT
# DATE: 2023-07-14
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(mindthegap)
    library(googlesheets4)
    library(googledrive)


  # SI specific paths/functions
    load_secrets()

  # Grab metadata

  # REF ID for plots
    ref_id <- "621970ea"

    # Steps:
      #1. Load the new gs_ids
      #2. Pull the UNAIDS data with the read_rename() and validate_cols() function
      #3. Run the munge_unaids() to see how it cleans the data
      #4. Test and update until you have a clean dataset


# LOAD DATA ============================================================================

  #NEW IDS
    gs_id_unaids <- "1ENUF8tKegwkbSck489aA2S1C_BEEvkgJG2hOevUKDyw"
    #original UNAIDS data - 2023 estimates (new)

    gs_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"
    #UNAIDS crosswalk (updated)

    #Clean UNAIDS estimates data on google drive (TBD)

    #PEPFAR only Clean UNAIDS data ongoogle drive (TBD)

    drive_id <- googledrive::as_id("1-iCrHGyU-xfDmzdfgXJ1P_wLI90s5RR-")
    #UNAIDS drive folder (same)

# FUNCTIONS ============================================================================

    #check utilities.R

    read_rename <- function(return_type) {

      #to specify NA's when reading in data
      missing <- c("...", " ")

      sheetname = ifelse(return_type == "HIV Estimates", "HIV2023Estimates_ByYear", "HIV-Test-&-Treat_ByYear")
      skip_n = ifelse(sheetname == "HIV2023Estimates_ByYear", 5, 4) #update year and skips

      gdrive_df <- suppressMessages(
        googlesheets4::read_sheet(gs_id_unaids, sheet = sheetname, skip = skip_n, na = missing, col_types = "c") %>% #reads in as column as character string
          dplyr::rename(year = !!names(.[1]),
                        iso =  !!names(.[2]),
                        country =  !!names(.[3]))
      )

      gdrive_df <- validate_cols(gdrive_df, return_type)

      if (return_type == "HIV Test & Treat") {
        gdrive_df <-  suppressWarnings(
          gdrive_df %>%
            dplyr::mutate(across(tidyselect:::where(is.list), ~dplyr::na_if(., "NULL"))) %>%
            dplyr::slice(-c(1,2))
        )
      }

      return(gdrive_df)
    }


    validate_cols <- function(df, return_type) {

      sheetname <- ifelse(return_type == "HIV Estimates", "HIV estimates - by Year", "HIV Test & Treat - by Year ")

      names_cw <- suppressMessages(
        googlesheets4::read_sheet(gs_id_names, sheet = sheetname) %>%
          #dplyr::filter(sheet == "HIV Test & Treat - by Year ") %>%
          dplyr::select(-sheet) %>%
          tidyr::pivot_wider(names_from = names,
                             values_from = names_original) %>% #udpate names in crosswalk to match
          dplyr::select(-value)
      )

      #change column names - stop if length of names is not the same as length of df
      stopifnot(ncol(names_cw) == ncol(df))
      names(df) <- names(names_cw)

      return(df)

    }

  #Check munge_unaids function---------------------------------------------------------------------
    munge_unaids <- function(return_type, indicator_type) {

      # Google Sheet ID to original
      #sheet_id <- googledrive::as_id("1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs") #gdrive

      #sheet_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw" #crosswalk

      #Read Data from googlesheet and validate columns
      gdrive_df <- read_rename(return_type)

      #Munge
      gdrive_df_clean <-
        gdrive_df %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("_"), ~gsub(" |<|>", "", .))) %>% #replace special characters
        dplyr::mutate(dplyr::across(tidyselect::matches("\\_"), ~gsub("m","00000", .x)))%>% #replace unit values - matches uses regular expression
        dplyr::mutate(dplyr::across(tidyselect::matches("\\_"), ~gsub("\\.","",.x))) %>%
        dplyr::mutate(dplyr::across(tidyselect::contains("_"),~ as.numeric(.x)))%>% #indicator columns into numeric
        dplyr::mutate(region = ifelse(country %in% regions, country, NA)) %>%
        tidyr::fill(region) %>% #get regions column
        tidyr::pivot_longer(-c(year, iso, country, region),
                            names_to = c("indicator")) %>% #get indicator column
        tidyr::separate(indicator, sep = "_", into = c("indicator", "age", "sex", "stat")) %>% #separate merged data age/sex/stat
        tidyr::pivot_wider(names_from = 'stat', values_from = "value") #get est/low/high columns

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
      gdrive_df_clean <-  glamr::pepfar_country_list %>%
        dplyr::select(country, iso = country_iso) %>%
        dplyr::rename(countryname = country) %>%
        dplyr::left_join(gdrive_df_clean, ., by = "iso") %>%
        dplyr::mutate(country = ifelse(is.na(countryname), country, countryname),
                      pepfar = ifelse(is.na(countryname), FALSE, TRUE)) %>% #flag to denote pepfar countries
        dplyr::select(-countryname)

      #Export final df
      final_df <- suppressWarnings(
        gdrive_df_clean %>%
          dplyr::mutate(across(estimate:upper_bound, ~as.numeric(.x)),
                        indic_type =stringr::str_remove(indic_type, "_indics") %>% stringr::str_to_title(), #capitalizes "Integer" in indic_type
                        across(estimate:upper_bound, ~dplyr::case_when(indic_type == "Integer" ~ round(.x), #rounds Integer values
                                                                       indic_type == "Percent" ~ .x)),
                        across(age:sex, ~stringr::str_to_title(.x))) %>% #capitalizes "All" in age/sex
          dplyr::filter(indic_type == indicator_type)

      )

      return(final_df)

    }

# TEST IT ============================================================================

  #Test functions
    #read_rename
    gdrive_df <- read_rename("HIV Estimates")
    glimpse(gdrive_df)
    View(gdrive_df)

    #validate_cols
    df <- validate_cols("HIV Estimates")

    #munge_unaids
    final_integer_df <- munge_unaids(return_type = "HIV Estimates", indicator_type = "Integer")
    glimpse(final_integer_df)
    View(final_integer_df)


    final_percent_df <- munge_unaids(return_type = "HIV Test & Treat", indicator_type = "Percent")
    View(final_percent_df)



# SPINDOWN ============================================================================
