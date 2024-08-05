# PROJECT:  C:/Users/tessam/Documents/Github/mindthegap
# PURPOSE:  Add Tableau Documentation for next owner/maintainer
# AUTHOR:   T. Essam | USAID
# REF ID:   b7d76623
# LICENSE:  MIT
# DATE:     2024-07-29
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  #general
  library(tidyverse)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(googledrive)
  library(googlesheets4)
  library(mindthegap)

# INSTRUCTIONS ------------------------------------------------------------

  # The following data sets are needed to refresh the Tableau Dashboard
  # GGG Dashboard - 1MQ2RA_n5eYibCvM534qeMwS2f2C7cIpQ
  # TODO: Ping each dataset and ensure that a 2024 version is created with same width
  # Updated datasets will be rewired to Tableau GGG for updated
  # KP Atlas portion handled by KP team.

  # DECISION: Deprecation the subnational tab as data are not reliable or that useful


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "b7d76623"  #a reference to be places in viz captions

  load_secrets()

  # List the contents of the 2023 GGG dashboard. These are data sources needed, minus subnat
    ggg_list <- drive_ls(as_id("1MQ2RA_n5eYibCvM534qeMwS2f2C7cIpQ"))

  # Latest 2024 data
    drive_id_2024 <- "1ZBXDexM2Vw3fmiLs7KxfGO9WXlSdN6iZ541ttrnA6tU"

  # Create a function to pull the id associated with each keyword from the GGG datasources
    pull_gd_id <- function(keyword = "cascade"){
    ggg_list %>%
      filter(str_detect(name, keyword)) %>%
      select(id) %>%
      pull()
    }

    pull_indics <- function(df){
      indic_list <-  df %>%
        distinct(indicator) %>%
        pull()
      return(indic_list)
    }

    sort_ous <- function(df){
      df %>%
        arrange(country, indicator, year)
    }


# IMPORT ------------------------------------------------------------------

  # Read in 2023 unaids_cascade data as used in the dashboard
  ggg_list
  df_cscd <- range_speedread(ss = pull_gd_id("cascade$"))

  # Read in the 2023 incidence_prevalence, averted deaths, and
  df_incd <- range_speedread(ss = pull_gd_id("incidence"))
  df_avrtd <- range_speedread(ss = pull_gd_id("averted"))
  df_new_infct <- range_speedread(ss = pull_gd_id("new_hiv"))
  df_epi_cntrl <- range_speedread(ss = pull_gd_id("epi"))

  # Read in the new 2024 data
  df_unaids <- range_speedread(ss = drive_id_2024) %>%
    filter(pepfar == TRUE)

 # MUNGE UNAIDS CASCADE ----------------------------------------------------

  # TODO: Recreate the standard 95-95-95 cascade from new data
  # use the df_cscd and the new UNAIDS estimates to update the data

  # Pull indicator list needed from full dataset
  cscd_list <- pull_indics(df_cscd)

  df_cscd_2024 <- df_unaids %>% filter(indicator %in% cscd_list, year >= 2019) %>%
    select(region, iso, country, year, estimate, lower_bound, upper_bound,
           indicator, sex, age, pepfar) %>%
    mutate(cascade_mkr = "rcent", .after = indicator) %>%
    mutate(dataset = "cascade") %>%
    sort_ous()

  setdiff(names(df_cscd), names(df_cscd_2024))

# MUNGE INCIDENCE -------------------------------------------------------------------

  df_incd %>% names()
  df_incd %>% count(year)
  incd_list <- pull_indics(df_incd)

  df_incd_2024 <- df_unaids %>%
    filter(indicator %in% incd_list) %>%
    mutate(dataset = "prevention") %>%
    sort_ous()


# MUNGE TDEATHS DATA ------------------------------------------------------

  # EPI CONTROL
  epi_indic <- pull_indics(df_epi_cntrl)
  epi_indic

  df_epi_cntrl_2024 <-
    df_unaids %>%
    filter(indicator %in% epi_indic) %>%
    mutate(dataset = "epi_control") %>%
    sort_ous() %>%
    mutate(across(c(estimate, lower_bound, upper_bound), \(x) round(x, 0))) %>%
    select(-c(contains("achv")))



# DEATHS AVERTED ----------------------------------------------------------

  avrtd_list <- pull_indics(df_avrtd)
  df_avrtd %>% count(indicator)
  df_avrtd_2024 <- df_unaids %>%
    filter(indicator %in% avrtd_list) %>%
    sort_ous()


# NEW HIV INFECTIONS ------------------------------------------------------

  ifct_list <- pull_indics(df_new_infct)
  df_new_infct_2024 <- df_unaids %>%
    filter(indicator %in% ifct_list) %>%
    sort_ous()


# EXPORT 2024 DATA FOR GGG Story ---------------------------------------------------------------------

  gs_id_tblw <- "1IHGnM2RaqasN0kRABv8lASz31XEuGVXIVmpiLONTY0U"
  sheet_write(df_cscd_2024, ss = gs_id_tblw, sheet = "unaids_cascade_2024")

  # Write the wiring files needed for tableau
  write_csv(df_incd_2024, "../../UNAIDS/2024/unaids_incidence_prevalence.csv", na = "")
  write_csv(df_avrtd_2024 %>% filter(year > 2002), "../../UNAIDS/2024/unaids_averted.csv", na = "")
  write_csv(df_epi_cntrl_2024, "../../UNAIDS/2024/unaids_epi_control.csv", na = "")
  write_csv(df_cscd_2024, "../../UNAIDS/2024/unaids_cascade.csv", na = "")
  write_csv(df_new_infct_2024, "../../UNAIDS/2024/unaids_new_hiv_infection.csv", na = "")





