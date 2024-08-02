# PROJECT:  C:/Users/tessam/Documents/Github/mindthegap
# PURPOSE:  Document the public released data for LLM training purposes
# AUTHOR:   T. Essam | USAID
# REF ID:   f5c5f68e
# LICENSE:  MIT
# DATE:     2024-08-02
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
  library(dataReporter)
  library(labelled)


# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "f5c5f68e"  #a reference to be places in viz captions

  load_secrets()

  # Latest 2024 data
  drive_id_2024 <- "1ZBXDexM2Vw3fmiLs7KxfGO9WXlSdN6iZ541ttrnA6tU"

# IMPORT ------------------------------------------------------------------

  # Read in the new 2024 data
  UNAIDS_2024_Clean_Estimates <- range_speedread(ss = drive_id_2024)

  names(UNAIDS_2024_Clean_Estimates)
  check(UNAIDS_2024_Clean_Estimates$estimate)

  # Improve the codebook by adding in additional information about the variables
  UNAIDS_2024_Clean_Estimates %>% get_variable_labels()

  # Set the column labels
  UNAIDS_2024_Clean_Estimates <- UNAIDS_2024_Clean_Estimates %>%
    set_variable_labels(
      year = "Year of estimate",
      iso = "Three digit ISO country code",
      country = "Country name",
      region = "UNAIDS regional groupings",
      indicator = "Epidemic indicator",
      age = "Age groups",
      sex = "Sex groups",
      estimate = "Modeled estimate value",
      lower_bound = "Lower bound estimate from model",
      upper_bound = "Upper bound estimate from model",
      estimate_flag = "demarcates estimates that have < | > signs associated with them",
      sheet = "Sheet from which data are derived",
      indic_type = "nNature of the estimate, percent or integer",
      pepfar = "Logical flagging PEPFAR operating units",
      achv_95_plhiv = "Flag indicating that a country has achieved all 3 95 UNAIDS goals using the PLHIV base",
      achv_95_relative = "Flag indicating that a country has achieved all 3 95 UNAIDS goals using the relative (sliding) base",
      epi_ratio_2023 = "Ratio of new HIV Infections to total deaths to HIV population in 2023",
      epi_control = "Flag indicating a country has reached epi countrol -- where total number of new HIV infections falls below the total number of deaths to HIV population"
    )

  makeDataReport(UNAIDS_2024_Clean_Estimates, codebook = TRUE, replace = TRUE)
