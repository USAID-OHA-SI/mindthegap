#' @title 95's Table Plot
#' @description This function creates a summary table showing OU progress toward the 95-95-95's
#' @param sel_base Returns one of 2 table types eg "PLHIV", "Relative"
#' @param sel_cntry  PEPFAR country to visualize (list OU name)
#'
#' @return df_tt
#' @export
#'
#' @examples
#'  \dontrun{
#'    base_plot(sel_base = "PLHIV", sel_cntry = "Lesotho")
#'    base_plot(sel_base = "Relative", sel_cntry = "Lesotho")
#' }
#'

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
      gt::tab_source_note(source_note = gt::md(glue::glue("Source: UNAIDS Data 2022 Release"))) %>%
      gt::tab_options(source_notes.font.size = gt::px(8),
                  data_row.padding = gt::px(1),
                  table.font.size = gt::px(12)) %>%
      gtExtras::gt_color_rows(achieved, palette = RColorBrewer::brewer.pal("Set1", n=3), domain = c(0,1)) %>%
      gt::tab_header(title = glue::glue("{toupper(sel_cntry)}'S 2022 TREATMENT TARGET GOALS: PLHIV BASE"))

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
      gt::tab_source_note(source_note = gt::md(glue::glue("Source: UNAIDS Data 2022 Release"))) %>%
      gt::tab_options(source_notes.font.size = gt::px(8),
                  data_row.padding = gt::px(1),
                  table.font.size = gt::px(12)) %>%
      gtExtras::gt_color_rows(achieved, palette = RColorBrewer::brewer.pal("Set1", n=3), domain = c(0,1)) %>%
      gt::tab_header(title = glue::glue("{toupper(sel_cntry)}'S 2022 TREATMENT TARGET GOALS: RELATIVE BASE"))
  }
  })
  return(df_tt)

}

