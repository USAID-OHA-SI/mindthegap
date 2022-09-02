# PROJECT: Assesing the new UNAIDS data dump
# PURPOSE: Munge and Analysis of
# AUTHOR: Tim Essam | SI
# REF ID:   7c702333
# LICENSE: MIT
# DATE: 2022-08-10
# NOTES: Tim Essam | SI

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
    library(googlesheets4)
    library(googledrive)

    load_secrets()

  # SI specific paths/functions
  import_file_list <- list.files(path = "../../../Downloads/2022/", pattern = "HIV", full.names = T)

  # REF ID for plots
    ref_id <- "7c702333"

  # Functions


# LOAD DATA ============================================================================

  # Check all the files for names
    # 1994-2000 Data has a few extra columns, o/wise it appears to be consistent across files
   df_un <-  map_dfr(import_file_list, ~vroom::vroom(.x))

  # Load PEPFAR names so countries can be tagged in full dataset
   pepfar_ous <- glamr::pepfar_country_list %>%
        dplyr::select(country, iso = country_iso) %>%
        dplyr::rename(countryname = country)

   un_gsheet_link <- "1cMpDKSUecGtNR4IZ0AG_B9xCFRihHQrsxuozO_FU3vQ"
   un_indic_cw <- read_sheet(un_gsheet_link, sheet = "Indicators")

# MUNGE ============================================================================

  #  Let's look at indicator wrack up
  # df_un %>%
  #   distinct(E_Ind, e_cat, Ind_ID) %>%
  #   write_csv("../../UNAIDS_indicator_list.csv")

  # write_csv(tmp, "../../UNAIDS_combined.csv")
  # where e_cat == "Uncertainty Analysis" there is either a lower or upper bound involved, but there
  # are also upper/lower bounds with "Derived" indicators
  # e_cat == "AIDS (AIM)" is all modeled estimates


  # Split the E_Ind column into new columns for indicator, sex, age, estimate type
  # Stubs needed for regex
  # Grab first occurence of paratheses
  age_stub <- "^.*?\\(([^\\)]+)\\)"
  stat_stub <- "Percent|Per 1000"

 df_un_cwalk <- df_un %>%
   distinct(Ind_ID, e_cat, E_Ind) %>%
   separate(E_Ind,
     into = c("code", "indicator"),
     sep = "- ",
     extra = "merge",
     remove = F
   ) %>%
   separate(indicator,
     into = c("indicator", "bound"),
     sep = "; (?=U|L)|- (?=U|L)"
   ) %>%
   separate(indicator,
     into = c("indicator", "age_unit_sex"),
     sep = "; (?=\\()| (?=F|M|per 1000)| (?=\\(\\d|\\(%)",
     extra = "merge"
   ) %>%
   mutate(
     age_unit_sex = case_when(
       str_detect(age_unit_sex, "%") ~ str_replace(age_unit_sex, "%", "Percent"),
       str_detect(age_unit_sex, "per 1000") ~ str_replace(age_unit_sex, "per 1000", "(Per 1000)"),
       TRUE ~ age_unit_sex
     ),
     model_est = ifelse(e_cat == "AIDS (AIM)", "estimate", str_to_lower(bound)),
     indicator = trimws(indicator),
     # Use the paratheses pattern to determine how to parse age_unit_sex
     parath_count = str_count(age_unit_sex, "\\("),
     sex = str_extract_all(age_unit_sex, "([^)]+$)") %>% trimws(),
     # What does it mean when sex is missing? versus male+female?
     sex = case_when(
       str_detect(sex, "ale", negate = T) | is.na(sex) ~ "all",
       sex == "Male+Female" ~ "all",
       TRUE ~ str_to_lower(sex)
     ),
     age = str_extract_all(age_unit_sex, age_stub, simplify = T) %>% str_remove_all(., "\\(|\\)"),
     est_type = str_extract_all(age_unit_sex, stat_stub, simplify = T),
     age = case_when(
       str_detect(age, "Per") ~ "all",
       is.na(age) ~ "all",
       # if the string has 0 length, replace with all
       nchar(age) == 0 ~ "all",
       TRUE ~ age
     ),
     model_est = case_when(
       is.na(model_est) ~ "estimate",
       model_est == "lower" ~ "lower bound",
       model_est == "upper" ~ "upper bound",
       TRUE ~ model_est
     ),
     # NEED TO SET INDICIDENCE PER 1000 as sep indicator
     indicator = case_when(
       indicator == "Incidence" & est_type == "Per 1000" ~ "Incidence Per 1000",
       indicator == "Incidence" & est_type == "Percent" ~ "Incidence",
       TRUE ~ indicator
     )
   )


  # Now, take the crosswalk and glue it on the df_un dataset, so we can have something to work with
 # df_un_cwalk %>%
 #   count(indicator) %>%
 #   format_tsv %>%
 #   writeLines()

 df_un_clean <-
   df_un %>%
   left_join(df_un_cwalk) %>%
   left_join(pepfar_ous, by = c("ISO3" = "iso")) %>%
   # Is a country missing in PEPFAR list, use UNAIDS name
   # o/wise use PEPFAR name for consistency
   mutate(country = ifelse(is.na(countryname), E_Count, countryname),
         pepfar = !is.na(countryname))    %>%
   #Clean up names
   rename(region = Region,
          iso2 = ISO2,
          iso3 = ISO3,
          area_id = Area_ID,
          geo_level = Type,
          ind_code = Ind_ID,
          ind_type = e_cat,
          ind_desc = E_Ind,
          year = Time,
          value = Value,
          rounded = Rounded,
          formatted = Formatted,
          status = Status,
          note = Note)


 # Join in the preferred names for indicators from google drive, for consistency moving forward
 df_un_clean <-
   df_un_clean %>%
   left_join(., un_indic_cw %>% select(indicator, `preferred name`, dataset), by = c("indicator"))

# Check how many PEPFAR OUs map in (1 missing -- Panama, why oh why?)
 df_un_clean %>%
   filter(pepfar == T) %>%
   distinct(country) %>%
   nrow()

 # Panama is missing from UNAIDS data
 setdiff(pepfar_ous$countryname,
         df_un_clean %>% filter(pepfar == T) %>% distinct(country) %>% pull())

 # Function to look at indicators and breakdown
 sum_indic <- function(ind_pat){
   df_un_clean %>%
     filter(str_detect(indicator, ind_pat)) %>%
     filter(country == "Zambia") %>%
     View()
 }

 sum_indic(c("prevalence"))
# BREAK OUT CASCADE DATA ----------------------------------------------------------

 max_yr <- max(df_un_clean$year)
 cascade_flt <- c("Percent on ART|Percent Known Status|Percent VLS")

 # Below generates a wide dataframe with mode_est varying parameters omitted (flattens data out)
 # This general framework
 # Steps for each dataset
 # 1) filter to the SI created dataset that you need (epi control, prevention, cascade, etc..)
 # 2) Determine if you need all or only PEPFAR OUs
 # 3) Determine year threshold you need
 # 4) Determine subset of columns that you need
 # 5) Determine if data need to be pivoted wider or not

 df_un_clean_sub <-
   df_un_clean %>%
   select(region, iso2, iso3, country, geo_level, ind_type, ind_desc, year, value,
          indicator = `preferred name`, # Do we need both indicator names?
          model_est, sex, age, pepfar, countryname, dataset)


# CASDCADE 95-95-95 -------------------------------------------------------

  df_cscd <-
    df_un_clean %>%
    mutate(cascade_mkr = substr(ind_desc, 3, 8)) %>%
    select(region, iso2, iso3, country, geo_level, ind_type, year, value,
           indicator = `preferred name`, cascade_mkr,
           model_est, sex, age, pepfar, countryname, dataset) %>%
    filter(dataset == "cascade",
           pepfar == T,
           year >= max_yr-3) %>%
   distinct() %>%
    # For the cascade, we can grab the 3rd - 8th character in the ind_desc to create a cascade marker
    # pivot so that est, upper, lower are columns
    pivot_wider(names_from = model_est, values_from = value)

  # May want to create a flag for the following
  # OUs that have reached epidemic control

  # Export a cut to Tableau for LMA cascade plots
  write_csv(df_cscd, "data-raw/UNAIDS_cascade.csv")


# EPI DATA ----------------------------------------------------------------

  df_epi <-
    df_un_clean_sub %>%
    select(-c(ind_desc, ind_type)) %>% # this guy mucks up pivot wide
    filter(dataset == "epi control") %>%
    mutate(value = round(value, 0)) %>%
    #janitor::get_dupes() %>% view()
    distinct() %>% #value col coming in as list because of duplicates
    pivot_wider(names_from = model_est, values_from = value)

  write_csv(df_epi,"data-raw/UNAIDS_epi_control.csv")
  write_csv(df_epi, "../../Last Mile Analytics/Data/UNAIDS_epi_control.csv")


# Prevalence --------------------------------------------------------------

  # Incidence indicator names needed to be renamed due to potenial duplication
  # resolved upstream
  df_prev <-
    df_un_clean_sub %>%
    select(-c(ind_desc, ind_type)) %>% # this guy mucks up pivot wide
    filter(dataset == "prevention",
           str_detect(indicator, "prevalence|Incidence")) %>%
    distinct() %>%
    pivot_wider(names_from = model_est, values_from = value)

  write_csv(df_prev, "data-raw/UNAIDS_prev.csv")

# WRITE TO DRIVE ============================================================================

  #add file to drive
  gs_id_new <- drive_create(name = "UNAIDS 2022 AIDSInfo Flat Files - Tidy Data", path = "SI Folder/Analysis, Data & Tools/UNAIDS/2022 - New AIDSInfo Flat Files: Tidy", type = "spreadsheet")

  #data_type == epi_control
  drive_write <- function(dataset) {

    tab_names <- c(dataset)

    if(dataset == "epi_control") {
      df <- df_epi
    } else if (dataset == "cascade"){
      df <- df_cscd
    } else if(dataset == "prevention") {
      df <- df_prev
    }

    sheet_write(data = df, ss = gs_id_new, sheet = tab_names)

  }

# AD HOC ============================================================================

  # Request to add infections averted by PMTCT and Number deaths averated by ART
  # Most data only available for a given span of time -- Starts in 2004
  df_averted <-
    df_un_clean_sub %>%
    select(-c(ind_desc, ind_type)) %>%
    filter(str_detect(indicator, "averted by PMTCT|Averted by ART"),
           pepfar == TRUE,
           year > 2003) %>%
    distinct() %>%
    pivot_wider(names_from = model_est, values_from = value)

  df_averted %>%
    write_csv("../thathill/Dataout/unaids_averted_deaths.csv")


# SUBNATIONAL DATA --------------------------------------------------------

  df_subnat <- df_un_clean %>%
    filter(geo_level == "Sub1", str_detect(country, "Moldova", negate = T))

  df_subnat <-
    df_subnat %>%
    mutate(cascade_mkr = substr(ind_desc, 3, 8)) %>%
    select(region, iso2, iso3, country, geo_level, ind_type, year, value,
            indicator = `preferred name`, cascade_mkr,
            model_est, sex, age, pepfar, countryname, dataset) %>%
    filter(dataset == "cascade",
           year >= max_yr-3) %>%
    distinct() %>%
    separate(country, into = c("country", "admin1"), sep = " - ") %>%
    mutate(admin1 = str_replace(admin1, " \\s*\\([^\\)]+\\)", "") %>% trimws(),
           country = str_replace(country, " States", "")) %>%
    # For the cascade, we can grab the 3rd - 8th character in the ind_desc to create a cascade marker
    # pivot so that est, upper, lower are columns
    pivot_wider(names_from = model_est, values_from = value)

  library(gisr)
  library(sf)

  eth <- get_admin1("Ethiopia")
  zim <- get_admin1("Zimbabwe")
  ind <- get_admin1("India")
  ken <- get_admin1("Kenya")

  # Need to have an identifier to join with the Admin 1 names
  df_subnat %>%
    distinct(country, admin1) %>%
    arrange(country) %>%
    write_sheet(., ss = "1UZWt3-Ywco_2K5v3jTaZ253QGv0u64W8JgGUveWx5CM", sheet = "unaids_subnat")

  df_subnat %>%
  write_csv(df_subnat, "../thathill/Dataout/unaids_cascade_subnat.csv")


# KP ATLAS DATA -----------------------------------------------------------

  df_kp <- read_sheet(ss = "1Jxj2PlJKSr_LrU2uNs-DBtl-6hwmzaha6Gg6wDQuAxw")

  df_kp %>%
    write_csv(., "../thathill/Dataout/UNAIDS_kp_atlas.csv")
