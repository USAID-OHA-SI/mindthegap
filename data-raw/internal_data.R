## REQUIRED COLUMNS
req_cols <- c("region", "e_count", "iso3", "e_cat", "e_ind", "acronym","age", "sex",  "other", "time", "formatted")

usethis::use_data(req_cols, overwrite = TRUE)


# INDICATOR MAPPING AND VALIDATION ----------------------------------------

df_ind <- path %>%
  read_edms() %>%
  dplyr::filter(e_cat != "Uncertainty Analysis") %>%
  dplyr::distinct(e_cat, e_ind, acronym, age, sex) %>%
  parse_indicator() %>%
  dplyr::distinct()


indicator_validation <- df_ind %>%
  dplyr::arrange(indicator_edms) %>%
  dplyr::select(indicator_edms, age, sex, source = e_cat) %>%
  dplyr::distinct()

usethis::use_data(indicator_validation, overwrite = TRUE)

# df_ind %>%
#   dplyr::distinct(acronym, indicator_edms)

indicator_map <-
  tibble::tribble(
            ~acronym,                                                        ~indicator_edms,                              ~indicator,
              "Prev",                                                 "Adult HIV prevalence",                    "Percent Prevalence",
              "Prev",                                                     "Adult prevalence",                    "Percent Prevalence",
                "AM",                                                          "AIDS deaths",            "Number AIDS Related Deaths",
                "AM",                                                   "Annual AIDS deaths",            "Number AIDS Related Deaths",
              "INC%",                                                            "Incidence",                 "Incidence (per 1,000)",
          "INCper1k",                                                   "Incidence per 1000",                 "Incidence (per 1,000)",
         "PMTCTneed",                                                "Mothers needing PMTCT",              "Number PMTCT Needing ART",
                "NI",                                                   "New HIV infections",             "Number New HIV Infections",
         "TM-HIVpop",                                       "Total deaths to HIV Population", "Number Total Deaths to HIV Population",
    "PMTCTreceiving",                                              "Mothers receiving PMTCT",            "Number PMTCT Receiving ART",
      "AMavertedART",                                                "Deaths averted by ART",          "Number Deaths Averted by ART",
    "NIavertedPMTCT",                                          "Infections averted by PMTCT",    "Number Infections Averted by PMTCT",
             "2nd81",                     "Among people living with HIV- the percent on ART",               "Percent on ART of PLHIV",
            "HIVPop",                                                       "HIV population",                          "Number PLHIV",
             "ART-D",                                                        "Number on ART",                "Number on ART of PLHIV",
             "1st90",      "Among people living with HIV- the percent who know their status",         "Percent Known Status of PLHIV",
             "2nd90",           "Among people who know their HIV status- the percent on ART",      "Percent on ART with Known Status",
             "kos-D",                                     "Number who know their HIV status",          "Number Known Status of PLHIV",
             "vls-D",                                    "Number with suppressed viral load",                   "Number VLS of PLHIV",
             "3rd73", "Among people living with HIV- the percent with suppressed viral load",                  "Percent VLS of PLHIV",
             "3rd90",          "Among people on ART- the percent with suppressed viral load",                    "Percent VLS on ART"
    )

usethis::use_data(indicator_map, overwrite = TRUE)
