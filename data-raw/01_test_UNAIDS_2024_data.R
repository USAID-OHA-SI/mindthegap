# PROJECT: 2024 UNAIDS Estimates
# PURPOSE: Test Functions
# AUTHOR: Lemlem Baraki | SI
# REF ID:   621970ea
# LICENSE: MIT
# DATE: 2024-08-02
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(gisr)
library(gophr)
library(tidyverse)
library(googlesheets4)
library(googledrive)


# SI specific paths/functions
load_secrets()

# Grab metadata

# REF ID for plots
ref_id <- "98b6f582"

# LOAD DATA ============================================================================

#NEW IDS
gs_id_unaids <- "1HWuvdI_IhdvV5pOLQngujq2kjkFLhPlHh4dMiLIQCe8"
#original UNAIDS data - 2024 estimates

gs_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"
#UNAIDS crosswalk

gs_clean_id <- "1TWmGqVMBzJvPQP6w2JtCYbeTKE7SdvrQxfEtHhyXZlg"
#Clean UNAIDS estimates data on google drive (update)

# FUNCTIONS ============================================================================

#Check pull_unaids function---------------------------------------------------------------------
#data_type: "HIV Estimates", "HIV Test & Treat", default is joint (includes Total deaths)
#pepfar_only: TRUE pulls from PEPFAR Only Clean Estimates, FALSE pulls from Clean Estimates
#note - now have a sheet column & Total deaths indicator is housed under default

pull_unaids <- function(data_type, pepfar_only = TRUE) {

  temp_folder <- glamr::temp_folder(quiet = TRUE)

  if (pepfar_only == TRUE) {
    filename <- glue::glue("UNAIDS_2024_Clean_Estimates_PEPFAR-only.csv.gz") #update
  } else {
    filename <- glue::glue("UNAIDS_2024_Clean_Estimates.csv.gz") #update
  }

  #download a specific file - test
  piggyback::pb_download(file = filename,
                         repo = "USAID-OHA-SI/mindthegap",
                         tag = "latest",
                         dest = temp_folder,
                         show_progress = FALSE)

  df <- temp_folder %>%
    glamr::return_latest(quiet = TRUE) %>%
    readr::read_csv(
      col_types = list(
        year = "d",
        estimate = "d",
        lower_bound = "d",
        upper_bound = "d",
        estimate_flag = "l",
        pepfar = "l",
        achv_95_plhiv = "l",
        achv_95_relative = "l",
        epi_control = "l",
        .default = "c")
    )

  if(missing(data_type)){
    df <- df %>%
      filter(!(indicator == "Number PMTCT Needing ART" & sheet == "HIV Test & Treat"))
  } else {
    df <- df %>%
      dplyr::filter(sheet == data_type)
  }

  return(df)
}

#Check epi_plot function---------------------------------------------------------------------
#Update data_type to pull joint for Total deaths

epi_plot <- function(df = df_epi_pepfar, sel_cntry = c("All PEPFAR")){

  df_epi <- pull_unaids(pepfar_only = TRUE) %>% #pull from joint PEPFAR only estimates
    dplyr::filter(age == "All", sex == "All",
                  indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections")) %>% #grab indicators
    dplyr::select(year, country,indicator, estimate) %>%
    dplyr::arrange(country, indicator, year) #order rows by these variables

  # Perform necessary munging
  df_epi_ous <-
    df_epi %>%
    #dplyr::mutate(indicator = stringr::word(indicator, -1) %>% tolower) %>% #filters indicator name to last word
    tidyr::pivot_wider(names_from = indicator, #pivots data wide into deaths and infections column
                       values_from = estimate,
                       names_glue = "{indicator %>% stringr::str_extract_all('deaths|Infections') %>% tolower}") #new death indicator

  # Add in ALL PEPFAR data
  df_epi_pepfar <-
    df_epi_ous %>%
    dplyr::bind_rows(df_epi_ous %>%
                       dplyr::mutate(country = "All PEPFAR") %>%
                       dplyr::group_by(country, year) %>%
                       dplyr::summarise(across(where(is.numeric),
                                               \(x) sum(x,na.rm = TRUE)),
                                        .groups = "drop")) #sums PEPFAR country estimates

  # Create epi control flag
  df_epi_pepfar <-
    df_epi_pepfar %>%
    dplyr::mutate(declining_deaths = deaths - dplyr::lag(deaths, order_by = year) <= 0, by = c(country)) %>% #TRUE/FALSE declining
    dplyr::mutate(infections_below_deaths = infections < deaths,
                  ratio = infections / deaths,
                  direction_streak = sequence(rle(declining_deaths)$lengths),
                  epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>%
    tidyr::pivot_longer(c(infections, deaths), names_to = "indicator") %>% #put back indicators in column
    dplyr::arrange(country, indicator, year) %>%
    dplyr::mutate(val_mod = ifelse(indicator == "deaths", -value, value), #create dual-axis
                  fill_color = ifelse(indicator == "deaths", glitr::old_rose, glitr::denim)) #add colors to indicate flip axis

  # OU list to check entries
  #ou_list <- glamr::pepfar_country_list %>% dplyr::distinct(country) %>% dplyr::pull()
  ou_list <- df_epi_pepfar %>% dplyr::distinct(country) %>% dplyr::pull()

  # Check if each value is valid
  is_valid <- all(sel_cntry %in% ou_list)

  # Output the result
  stopifnot("Please enter PEPFAR supported countries only" = is_valid != FALSE)

  df_viz <-
    df_epi_pepfar %>%
    dplyr::filter(country %in% sel_cntry) %>% #change to listed countries
    dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~
                                               scales::number(value, 1, scale = 0.001, suffix = "k")),
                  max_plot_pt = max(value),
                  min_plot_pt = min(val_mod),
                  lab_pt = dplyr::case_when(year == max(year) ~ val_mod),
                  indicator = ifelse(indicator == "deaths", "Total Deaths to HIV Population", "New HIV Infections"), #creating labels
                  new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),  #assigning label location to min/max
                  tot_death_label = dplyr::case_when(val_mod == min_plot_pt ~ indicator)) %>%
    dplyr::mutate(cntry_order = max(value, na.rm = T), .by = country) %>%
    dplyr::mutate(country = forcats::fct_reorder(country, cntry_order, .desc = T))

  suppressWarnings(df_viz %>%
                     ggplot2::ggplot(ggplot2::aes(year, val_mod, group = indicator, fill = fill_color, color = fill_color)) +
                     ggplot2::geom_blank(ggplot2::aes(y = max_plot_pt)) + #sets max y-axis above
                     ggplot2::geom_blank(ggplot2::aes(y = -max_plot_pt)) + #sets max y-axis below
                     ggplot2::geom_area(alpha = 0.25) +
                     ggplot2::geom_hline(yintercept = 0,color = glitr::grey80k) +
                     ggplot2::geom_line() +
                     ggplot2::geom_point(ggplot2::aes(y = lab_pt), na.rm = TRUE, shape = 21, color = "white", size = 3) +
                     ggplot2::geom_text(ggplot2::aes(label = val_lab), na.rm = TRUE, #value label text
                                        hjust = -0.3,
                                        family = "Source Sans Pro Light") +
                     ggplot2::facet_wrap(~country) + #small multiples of countries
                     #scale_y_continuous(labels = ~(scales::label_number_si())(abs(.))) + #deprecated - use 'scale_cut'
                     ggplot2::scale_y_continuous(labels = ~ (scales::label_number(scale_cut = scales::cut_short_scale())(abs(.))),
                                                 expand = c(0, 0)) +
                     ggplot2::scale_x_continuous(breaks = seq(min(df_epi$year), max(df_epi$year),5)) + #automatic x-axis min/max
                     #ggplot2::scale_x_continuous(breaks = seq(1990, 2025, 5)) + #manual x-axis breaks
                     ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
                     geom_text(aes(label = new_hiv_label, x = 2005, y = (max_plot_pt)), na.rm = TRUE,
                               hjust = -0.3, family = "Source Sans Pro Light") +
                     geom_text(aes(label = tot_death_label, x = 2005, y = (min_plot_pt)), na.rm = TRUE,
                               hjust = -0.3, family = "Source Sans Pro Light") +
                     #ggplot2::annotate(geom = "text", x = 2008, y = 2.8e6, label = c("New HIV Infections"), hjust = 0,
                     #      family = "Source Sans Pro Light", color = glitr::denim) +  #add labels to plot
                     #ggplot2::annotate(geom = "text", x = 2008, y = -1.5e6, label = c("Total Deaths to HIV Population"), hjust = 0,
                     #               family = "Source Sans Pro Light", color = glitr::old_rose) +
                     ggplot2::labs(x = NULL, y = NULL) + ggplot2::coord_cartesian(expand = T, clip = "off") +
                     glitr::si_style_ygrid(facet_space = 0.75) + #adjusted y-axis grid spacing with facet_space
                     ggplot2::theme(axis.text.y = ggtext::element_markdown()) +
                     ggplot2::labs(caption = "Source: UNAIDS Data 2023 Release"))

}


#Check base_plot function---------------------------------------------------------------
#might need to update the data_type to both

base_plot <- function(sel_base, sel_cntry){

  #Pull percent indicators from Test & Treat data
  df_tt <- pull_unaids(data_type = "HIV Test & Treat", pepfar_only = TRUE) %>%
    dplyr::filter(indic_type == "Percent")

  goal <- 95

  #PLHIV base
  suppressWarnings({
    if (sel_base == "PLHIV") {
      df_tt <- df_tt %>%
        dplyr::filter(year == max(year),
                      country == sel_cntry,
                      indicator %in% c("Percent Known Status of PLHIV",
                                       "Percent on ART of PLHIV",
                                       "Percent VLS of PLHIV"),
                      age == "All",
                      sex == "All") %>%
        dplyr::mutate(set = dplyr::recode(indicator, "Percent Known Status of PLHIV" = 1,
                                          "Percent on ART of PLHIV" = 2,
                                          "Percent VLS of PLHIV" = 3),
                      goal_rate = round((goal/100)^set*100),
                      achieved = estimate >= goal_rate) %>%
        dplyr::select(year, country, indicator, estimate, goal_rate, achieved) %>%
        gt::gt() %>%
        gt::cols_hide(c(year, country)) %>%
        gt::fmt_percent(columns = c(estimate, goal_rate),
                        decimals = 0, scale_values = FALSE) %>%
        gt::cols_label(goal_rate = "goal") %>%
        gtExtras::gt_theme_nytimes() %>%
        gt::tab_source_note(source_note = gt::md(source_note)) %>%
        gt::tab_options(source_notes.font.size = gt::px(8),
                        data_row.padding = gt::px(1),
                        table.font.size = gt::px(12)) %>%
        #gtExtras::gt_color_rows(achieved, palette = RColorBrewer::brewer.pal("Set1", n=3), domain = c(0,1)) %>%
        gtExtras::gt_color_rows(achieved,
                                palette = c(glitr::burnt_sienna, glitr::scooter), #change palette to raw values
                                #palette = "ggthemes::Traffic",
                                domain = c(0,1),
                                pal_type = "discrete") %>%
        gt::tab_header(title = glue::glue("{toupper(sel_cntry)}'S 2023 TREATMENT TARGET GOALS: PLHIV BASE"))

      #Relative base
    } else if (sel_base == "Relative") {
      df_tt <- df_tt %>%
        dplyr::filter(year == max(year),
                      country == sel_cntry,
                      indicator %in% c("Percent Known Status of PLHIV",
                                       "Percent on ART with Known Status",
                                       "Percent VLS on ART"),
                      age == "All",
                      sex == "All") %>%
        dplyr::mutate(goal_rate = 95, # Use 95 as the goal metric for each indicator
                      achieved = estimate >= goal_rate) %>%
        dplyr::select(year, country, indicator, estimate, goal_rate, achieved) %>%
        gt::gt() %>%
        gt::cols_hide(c(year, country)) %>%
        gt::fmt_percent(columns = c(estimate, goal_rate),
                        decimals = 0, scale_values = FALSE) %>%
        gt::cols_label(goal_rate = "goal") %>%
        gtExtras::gt_theme_nytimes() %>%
        gt::tab_source_note(source_note = gt::md(source_note)) %>%
        gt::tab_options(source_notes.font.size = gt::px(8),
                        data_row.padding = gt::px(1),
                        table.font.size = gt::px(12)) %>%
        #gtExtras::gt_color_rows(achieved, palette = RColorBrewer::brewer.pal("Set1", n=3), domain = c(0,1)) %>%
        gtExtras::gt_color_rows(achieved,
                                palette = c(glitr::burnt_sienna, glitr::scooter), #change palette to raw values
                                #palette = "ggthemes::Traffic",
                                domain = c(0,1),
                                pal_type = "discrete") %>%
        gt::tab_header(title = glue::glue("{toupper(sel_cntry)}'S 2023 TREATMENT TARGET GOALS: RELATIVE BASE"))
    }
  })
  return(df_tt)

}


# TEST IT ============================================================================

#pull_unaids
library(magrittr)

pull_unaids("HIV Estimates", pepfar_only = T) %>%  #6 of 7 - missing total deaths indicator
  distinct(indicator)

pull_unaids("HIV Test & Treat", pepfar_only = T) %>% #11 of 11 - all indicators present
  distinct(indicator)

pull_unaids(pepfar_only = T) %>% #19 - excluding "PMTCT on ART"
  distinct(indicator) #adds "Deaths averted by ART", "Infections averted by PMTCT", "Total deaths"

#epi_plot
epi_plot() #default is "ALL PEPFAR"
epi_plot(sel_cntry = "Zambia")
epi_plot(sel_cntry = c("South Africa", "Zambia", "Kenya", "Malawi")) #specify countries
epi_plot(sel_cntry = "USA") #break with non-PEPFAR countries

#base_plot
base_plot(sel_base = "Relative", sel_cntry = "Zambia")



# SPINDOWN ============================================================================
