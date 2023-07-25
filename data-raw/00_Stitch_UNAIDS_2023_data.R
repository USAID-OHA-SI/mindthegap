# PROJECT: Assessing the new UNAIDS data dump
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam & Jessica Hoehner | SI
# REF ID:   7c702333
# LICENSE: MIT
# DATE: 2023-07-25
# NOTES: Tim Essam & Jessica Hoehner | SI

# LOCALS & SETUP ============================================================================

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
  # very large file, could take several minutes to load
   unaids_custom <- read_sheet("157aaDjrHv1wQOP55ycxDXfADWOZddONmuu0kRMVcN9g")

  # Load PEPFAR names so countries can be tagged in full dataset
   pepfar_ous <- glamr::pepfar_country_list %>%
      dplyr::select(country, iso = country_iso) %>%
      dplyr::rename(countryname = country)

# MUNGE ============================================================================
  # where e_cat == "Uncertainty Analysis" there is either a lower or upper bound involved, but there
  # are also upper/lower bounds with "Derived" indicators
  # e_cat == "AIDS (AIM)" is all modeled estimates

 df_un_clean <-  unaids_custom %>%
   clean_names() %>%
   separate(e_ind,
     into = c("code", "indicator"),
     sep = "- ",
     extra = "merge",
     remove = F) %>%
  separate(indicator,
              into = c("indicator", "bound"),
              sep = "; (?=U|L)|- (?=U|L)") %>%
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
     #model_est = if_else(e_cat == "AIDS (AIM)", "estimate", str_to_lower(bound)),
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

 # create PEPFAR column

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

# SUBNATIONAL DATA --------------------------------------------------------

  df_subnat <- df_un_pepfar %>%
    filter(geo_level == "Sub1", str_detect(country, "Moldova", negate = T)) %>%
    distinct() %>%
    separate(country, into = c("country", "admin1"), sep = " - ") %>%
    mutate(admin1 = str_replace(admin1, " \\s*\\([^\\)]+\\)", "") %>% trimws(),
           country = str_replace(country, " States", ""))

df_subnat %>%
    write_sheet(., ss = "1u2SZFoj2a9BDrSlwi9HWZQya4j2y6XoEK2QM59vUa3Q", sheet = "unaids_subnat")

# KP ATLAS DATA -----------------------------------------------------------

  df_kp <- read_sheet(ss = "1q-UO51IrxhEVU-53qwA8QC6JCye897dlezXj6t0nHfQ")

  # df_kp %>%
  #   write_csv(., "../thathill/Dataout/UNAIDS_kp_atlas.csv")

# WRITE to DRIVE ==============================================================

# gs_id_new <- drive_create(
#   name = "UNAIDS 2023 AIDSInfo Flat Files - Tidy Data",
#   path = "SI Folder/Analysis, Data & Tools/UNAIDS/2023 Custom Pull/output",
#   type = "spreadsheet")
#
# drive_write <- function(dataset) {
#
#   tab_names <- c(dataset)
#
#   if(dataset == "total_deaths") {
#     df <- df_total_deaths
#   } else if (dataset == "unaids_averted_deaths"){
#     df <- df_inf_death_vert
#   }
#
#   sheet_write(data = df, ss = gs_id_new, sheet = tab_names)
#
# }
#
# drive_write(df_un_pepfar)
