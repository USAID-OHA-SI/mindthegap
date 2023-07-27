# PROJECT: Assessing the new UNAIDS data dump
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam & Jessica Hoehner | SI
# REF ID:   7c702333
# LICENSE: MIT
# DATE: 2023-07-27
# NOTES: Tim Essam & Jessica Hoehner | SI

# LOCALS & SETUP ===============================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(googlesheets4)
    library(googledrive)
    library(janitor)
    library(gisr)

  # SI specific paths/functions
    load_secrets()

# LOAD DATA ====================================================================

  # Comes from UNAIDS/2023 Custom Pull/input
  # very large file, could take several minutes to load, includes NAT and SUBNAT
   unaids_custom <- read_sheet("157aaDjrHv1wQOP55ycxDXfADWOZddONmuu0kRMVcN9g")

  # KP Atlas data
   df_kp <- read_sheet(ss = "1q-UO51IrxhEVU-53qwA8QC6JCye897dlezXj6t0nHfQ")

  # Load PEPFAR names so countries can be tagged in full dataset
   pepfar_ous <- glamr::pepfar_country_list %>%
      dplyr::select(country, iso = country_iso) %>%
      dplyr::rename(countryname = country)

  # read in indicator crosswalk data to further tidy data used in KP Atlas
   indicator_crosswalk <- read_sheet("1iv5aBHXSqO2Ky4d6zEORl2KwL_7_08w_D0CSTKgoq0A") %>%
     clean_names()

# MUNGE ========================================================================

  # UNAIDS custom pull ----------------------------------------------------
  # where e_cat == "Uncertainty Analysis" there is either
  # a lower or upper bound involved, but there are also
  # upper/lower bounds with "Derived" indicators
  # e_cat == "AIDS (AIM)" is all modeled estimates
  # this pulls in both Nat and Subnat data

   df_un_clean <-  unaids_custom %>%
     clean_names() %>%
       # split the e_ind column into "Code" and "Indicator"
       # code denotes the year, 2023 estimates have a code of "N"
     separate(e_ind,
       into = c("code", "indicator"),
       sep = "- ",
       extra = "merge",
       remove = F) %>%
       # split the indicator column again to create a column for "bound"
       # to show the upper and lower bounds of each estimate
    separate(indicator,
                into = c("indicator", "bound"),
                sep = "; (?=U|L)|- (?=U|L)") %>%
       # split the indicator column again to pull out the sex information
       # UNAIDS provides a "sex" column but we would prefer to format
       # this column differently
    separate(indicator,
                into = c("indicator", "sex_usaid"),
                sep = "; (?=\\()|(?=Female|Male)| (?=\\(\\d)",
                extra = "merge") %>%
     rename(
       country = e_count,
       geo_level = type,
       year = time) %>%
     mutate(
      bound = case_when(
         bound == "Lower bound" ~ "lower bound",
         bound == "Upper bound" ~ "upper bound",
         is.na(bound) ~ "estimate"),
       indicator = trimws(indicator),
       sex_usaid = if_else(sex_usaid == "Male+Female", "all",
                          sex_usaid %>% tolower()),
       age = case_when(
         str_detect(age, "allAges") ~ "all",
         is.na(age) ~ "all",
         # if the string has 0 length, replace with all
         nchar(age) == 0 ~ "all",
         TRUE ~ age),
      dataset = if_else(stringr::str_detect(indicator, "Total"), "total_deaths",
      "infections_deaths_averted")) %>%
       select(region, iso2, iso3, country,
              geo_level, year, indicator,
              sex_usaid, age, value, bound, dataset) %>%
       pivot_wider(names_from = "bound",
                   values_from = "value") %>%
       rename(sex = sex_usaid)

   # create PEPFAR column so we know which estimates come from
   # PEPFAR supported OUs

   df_un_pepfar <- df_un_clean  %>%
     left_join(pepfar_ous, by = c("iso3" = "iso")) %>%
     # Is a country missing in PEPFAR list, use UNAIDS name
     # o/wise use PEPFAR name for consistency
     mutate(country = ifelse(is.na(countryname), country, countryname),
           pepfar = !is.na(countryname))

  # Check how many PEPFAR OUs map in
   countries <- df_un_pepfar %>%
     filter(pepfar == TRUE) %>%
     distinct(country) %>%
     nrow()

   # India, Kazakhstan, Nigeria, and Ukraine are missing from UNAIDS data
   setdiff(pepfar_ous$countryname,
           df_un_pepfar %>% filter(pepfar == TRUE)
           %>% distinct(country) %>% pull())

  # Sub-national data ---------------------------------------------------

  df_subnat <- df_un_pepfar %>%
    filter(geo_level == "Sub1") %>%
    distinct() %>%
     # splitting the country column to create the admin1 column
    separate(country,
             into = c("country", "admin1"), sep = " - ") %>%
    mutate(admin1 = str_replace(admin1, " \\s*\\([^\\)]+\\)", "") %>% trimws(),
           country = str_replace(country, " States", ""))

# EXPORT =======================================================================

   # National data
   df_un_pepfar %>%
     filter(!geo_level %in% "Sub1") %>%
     write_sheet(., ss = "1Brg_v0rXtDcvtdrUkmyjztu4Vwzx_yzUcw4EzzKSA98",
                 sheet = "unaids_nat")

   # sub-National data
   df_subnat %>%
     write_sheet(., ss = "1u2SZFoj2a9BDrSlwi9HWZQya4j2y6XoEK2QM59vUa3Q",
                 sheet = "unaids_subnat")

   # KP Atlas data

   df_kp %>%
     write_sheet("1gR680KtLWu3JqkOyGuJpqjhBujSP3C4N7MWFAt4DGd8",
                 sheet = "unaids_kp_atlas")
