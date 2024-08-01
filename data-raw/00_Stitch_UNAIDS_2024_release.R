# PROJECT:  C:/Users/tessam/Documents/Github/mindthegap
# PURPOSE:  Stitch UNAIDS datasets together for a release
# AUTHOR:   T. Essam | USAID
# REF ID:   860ddb00
# LICENSE:  MIT
# DATE:     2024-08-01
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


# GLOBAL VARIABLES --------------------------------------------------------
  load_secrets()

  ref_id <- "860ddb00"  #a reference to be places in viz captions

  # Latest 2024 data without Total Deaths Included
  drive_id_2024 <- "1cQLq37UonYCskMDaaxT87C87X5lrlILTecede_BIvN8"

  # Latest total deaths from EDMS
  drive_id_tdth_2024 <- "1SCK4kh0GTT7a1VBFAwVLg67MEycMPd2Dpj-krRSm3sg"


# IMPORT ------------------------------------------------------------------

  # Read in the new 2024 data
  df_hivest <- range_speedread(ss = drive_id_2024, sheet = "HIV Estimates")
  df_tntest <- range_speedread(ss = drive_id_2024, sheet = "HIV T&T")

  df_tdths <- range_speedread(ss = drive_id_tdth_2024) %>%
    janitor::clean_names()

  # Load PEPFAR names so countries can be tagged in full dataset
  pepfar_ous <- glamr::pepfar_country_list %>%
    dplyr::select(country, iso = country_iso) %>%
    dplyr::rename(countryname = country) %>%
    dplyr::mutate(pepfar = TRUE)

# MUNGE TOTAL DEATHS -------------------------------------------------------------------

  # Create Total deaths data frame compatible with cleaned UNAIDS data
  df_tdths_2024 <- df_tdths %>%
    full_join(pepfar_ous, by = c("iso3" = "iso")) %>%
    # Fix country names in the UNAIDS data using PEPFAR names
    mutate(e_count = ifelse(!is.na(countryname), countryname, e_count)) %>%
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
        bound == "Lower bound" ~ "lower_bound",
        bound == "Upper bound" ~ "upper_bound",
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
      dataset = if_else(stringr::str_detect(indicator, "Total"), "total_deaths_hiv_pop",
                        "infections_deaths_averted")) %>%
    select(region, iso2, iso3, country,
           geo_level, year, indicator,
           sex_usaid, age, value, bound, dataset,
           pepfar) %>%
    pivot_wider(names_from = "bound",
                values_from = "value") %>%
    rename(sex = sex_usaid) %>%
    # Cleaning up names that look off -- need to verify full list -- done above
    # mutate(country = case_when(
    #   str_detect(country, "Viet") ~ "Vietnam",
    #   str_detect(country, "Tanzania") ~ "Tanzania",
    #   str_detect(country, "Laos") ~ "Laos"
    #   TRUE ~ country
    # )) %>%
    mutate(across(c(age, sex), ~stringr::str_to_title(.x)),
           indic_type = "Integer")


# CREATE 95s FLAGS TO FULL JOIN -------------------------------------------

  #UNAIDS GOAL
  goal <- 95

  # PLHIV BASE
  plhiv_base_95 <- df_tntest %>%
    filter(year == max(year),
           indicator %in% c("Percent Known Status of PLHIV", "Percent on ART of PLHIV", "Percent VLS of PLHIV"),
           age == "All",
           sex == "All") %>%
    select(year, country, iso, indicator, estimate) %>%
    filter(!is.na(estimate)) %>%
    mutate(indicator = recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                              "Percent on ART of PLHIV" = "On\nART",
                              "Percent VLS of PLHIV" = "VLS"),
           set = recode(indicator, "Known\nStatus" = 1,
                        "On\nART" = 2,
                        "VLS" = 3),
           goal_rate = round((goal/100)^set*100),
           achv = estimate >= goal_rate) %>%
    count(country,iso, achv) %>%
    mutate(plhiv_base_95s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
    distinct(country, iso, plhiv_base_95s) %>%
    rename(achv_95_plhiv = plhiv_base_95s)

  # RELATIVE BASE
  rel_base_95 <- df_tntest %>%
    filter(year == max(year),
           indicator %in% c("Percent Known Status of PLHIV", "Percent on ART with Known Status", "Percent VLS on ART"),
           age == "All",
           sex == "All") %>%
    select(year, country, iso, indicator, estimate) %>%
    filter(!is.na(estimate)) %>%
    mutate(indicator = recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                              "Percent on ART with Known Status" = "On\nART",
                              "Percent VLS on ART" = "VLS"),
           set = recode(indicator, "Known\nStatus" = 1,
                        "On\nART" = 2,
                        "VLS" = 3),
           # goal_rate = round((goal/100)^set*100),
           achv = estimate >= goal) %>%
    group_by(country) %>%
    count(country, iso, achv) %>%
    mutate(rel_base_95s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
    distinct(country, iso, rel_base_95s) %>%
    rename(achv_95_relative = rel_base_95s)

  flag_list <- full_join(plhiv_base_95, rel_base_95, by = c("country", "iso"))


# BIND TOGETHER TO GET EPI CONTROL MERGE FLAGS ----------------------------

  df_tdths_2024 <- df_tdths_2024 %>%
    select(year, iso = iso3, country, region, indicator, age, sex, estimate, lower_bound,
           upper_bound, indic_type, pepfar)

  # Bind them all together
  df_unaids_2024 <- bind_rows(df_hivest, df_tntest, df_tdths_2024) %>%
    filter(!is.na(indicator))

  # CHECK INDICATORS
  df_unaids_2024 %>% distinct(indicator)

  # Calculate epi control
  df_epi_cntrl_flag <- df_unaids_2024 %>%
    filter(indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections"),
           age == "All",
           sex == "All") %>%
    select(region, country, year, iso, indicator, estimate) %>%
    pivot_wider(names_from = "indicator",
                values_from = estimate) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    rename(deaths = `Total deaths to HIV Population`,
           infections = `Number New HIV Infections`) %>%
    mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>%
    ungroup() %>%
    mutate(infections_below_deaths = infections < deaths,
           ratio = infections / deaths,
           direction_streak = sequence(rle(declining_deaths)$lengths),
           epi_control = (declining_deaths == TRUE & infections_below_deaths == TRUE)) %>%
    filter(year == max(year)) %>%
    select(iso, epi_ratio_2023 = ratio, epi_control)


# CREATE RELEASE DATA -----------------------------------------------------

  df_unaids_2024_release <-
    df_unaids_2024 %>%
    full_join(flag_list) %>%
    full_join(df_epi_cntrl_flag)

# TEST THE RELEASE

  # DOES IT PROVIDE CORRECT #s for Zambia?
  get_counts <- function(col = epi_control, ...){
    df_unaids_2024_release %>%
      filter(pepfar == TRUE) %>%
      distinct(country, {{col}}, ...) %>%
      count({{col}}, ...)
  }

  # How many outs reached epi control
  get_counts(epi_control, achv_95_plhiv)
  get_counts(achv_95_relative)
  get_counts(achv_95_plhiv)

  # ARE the OU names consistent?
  setdiff(df_unaids_2024_release %>% filter(pepfar == TRUE) %>% distinct(country),
          df_hivest %>% filter(pepfar == TRUE) %>% distinct(country))

  # Do the disaggregates look correct?
  df_unaids_2024_release %>%
    distinct(indicator, age, sex) %>%
    prinf()

  summary(df_unaids_2024_release)


# WRITE DATA LOCALLY ------------------------------------------------------

  #write_csv(df_unaids_2024_release, "../../UNAIDS/2024/df_unaids_2024_release.csv")


# FIRST UNAIDS RELEASE - 2024-08-01 -----------------------------------------

  library(piggyback)
  #add version tag for date
  date_release <- format(Sys.Date(), "%Y.%m.%d")
  new_tag <- glue::glue("v{date_release}")

  #create a new release (default will go to the USAID-OHA-SI/mindthegap repo)
  pb_new_release(tag = new_tag)

  #set up a temp folder
  glamr::temp_folder()

  #save data to temp folder
  curr_year <- format(Sys.Date(), "%Y")
  readr::write_csv(df_unaids_2024_release, file.path(folderpath_tmp, glue("UNAIDS_{curr_year}_Clean_Estimates.csv.gz")))
  saveRDS(df_unaids_2024_release, file.path(folderpath_tmp, glue("UNAIDS_{curr_year}_Clean_Estimates.rds")))

  readr::write_csv(df_unaids_2024_release %>% filter(pepfar == TRUE), file.path(folderpath_tmp, glue("UNAIDS_{curr_year}_Clean_Estimates_PEPFAR-only.csv.gz")))
  saveRDS(df_unaids_2024_release %>% filter(pepfar == TRUE), file.path(folderpath_tmp, glue("UNAIDS_{curr_year}_Clean_Estimates_PEPFAR-only.rds")))

  #upload multiple files to release for archiving
  list.files(folderpath_tmp, full.names = TRUE) %>%
    pb_upload(tag = new_tag)

  #upload multiple files to latest release for ease of access
  pb_delete(
    file = NULL,
    repo = "USAID-OHA-SI/mindthegap",
    tag = "latest",
    .token = gh::gh_token()
  )

  list.files(folderpath_tmp, full.names = TRUE) %>%
    pb_upload(tag = "latest")

  #download a specific file - test
  pb_download(file = glue("UNAIDS_{curr_year}_Clean_Estimates.rds"),
              repo = "USAID-OHA-SI/mindthegap",
              dest = glamr::si_path("path_downloads"))

# Optional, test
  #tmp <- read_rds("../../../Downloads/UNAIDS_2024_Clean_Estimates.rds")

  # Post to google drive
  sheet_id <- "1ZBXDexM2Vw3fmiLs7KxfGO9WXlSdN6iZ541ttrnA6tU"
  write_sheet(df_unaids_2024_release, ss = as_id(sheet_id))



