#' @title 95's Table Plot
#' @description This function creates a summary table showing OU progress toward the 95-95-95's
#'
#' @param df dataframe from `load_unaids`
#' @param cntry  PEPFAR country to visualize (list OU name)
#' @param denom which denominator/base to use, "PLHIV" (default) or "Relative"
#' @param grp age/sex group, c("All", "Female 15+", "Male 15+", "Peds <15")
#' @param yr year in question, defaults to max year
#'
#' @return gt table
#' @export
#' @seealso [load_unaids()]
#'
#' @examples
#'  \dontrun{
#'    tab_95s(load_unaids(), "Kenya", denom = "PLHIV")
#'    tab_95s(load_unaids(), "Kenya", denom = "Relative")
#' }
#'

tab_95s <- function(df, cntry,
                      denom = c("PLHIV", "Relative"),
                      grp = c("All", "Female 15+", "Male 15+", "Peds <15"),
                      yr = NULL){

  if(length(cntry) == 0){
    cli::cli_abort("No country provided. Please specify the country using the {.arg cntry} param.")
  }

  if(length(cntry) > 1){
    cli::cli_abort("Multiple countries provided. Please specify only one country using the {.arg cntry} param.")
  }

  if(length(denom) > 1){
    denom <- denom[1]
    # cli::cli_warn("No selection was made for the {.arg denom}; choosing {.val {denom}}.")
  }

  if(is.null(yr))
    yr <- unaids_year

  key_ind_95s <- c("Percent Known Status of PLHIV",
                   "Percent on ART of PLHIV",
                   "Percent VLS of PLHIV",
                   "Percent on ART with Known Status",
                   "Percent VLS on ART")

  df_tt <- df %>%
    dplyr::filter(year == {yr},
                  country == {cntry},
                  indicator %in% key_ind_95s)

  if(nrow(df_tt) == 0)
    cli::cli_abort("No data available in dataset for {.code cntry = {cntry}}.")

  df_tt <- df_tt %>%
    dplyr::select(year, country, indicator, age, sex, estimate, lower_bound, upper_bound) %>%
    tidyr::unite(group, c(sex, age), sep = " ") %>%
    dplyr::mutate(group = stringr::str_remove(group, "All "),
                  group = ifelse(group == "0-14", "Peds <15", group),
                  indicator = factor(indicator, key_ind_95s),
                  bounds = glue::glue("[{lower_bound}%-{upper_bound}%]")) %>%
    dplyr::relocate(bounds, .after = estimate) %>%
    dplyr::filter(group %in% grp) %>%
    dplyr::arrange(group, indicator) %>%
    calc_95_goals()

  df_viz <- df_tt %>%
    dplyr::filter(base %in% c("Both", {denom})) %>%
    dplyr::mutate(achieved = estimate >= goal_rate) %>%
    dplyr::select(-c(set, base, achv_plhiv, achv_relative, lower_bound, upper_bound))

  df_viz %>%
    gt::gt(groupname_col = "group") %>%
    gt::cols_hide(c(year, country)) %>%
    gt::fmt_percent(columns = c(estimate, goal_rate),
                    decimals = 0, scale_values = FALSE) %>%
    gt::cols_label(goal_rate = "goal") %>%
    gt::cols_align("left", indicator) %>%
    gtExtras::gt_theme_nytimes() %>%
    gt::tab_source_note(source_note = gt::md(source_note)) %>%
    gt::tab_options(source_notes.font.size = gt::px(8),
                    data_row.padding = gt::px(1),
                    table.font.size = gt::px(12)) %>%
    gtExtras::gt_color_rows(achieved,
                            palette = c(glitr::si_palettes$tango_t[3], glitr::viking),
                            domain = c(0,1),
                            pal_type = "discrete") %>%
    gt::tab_header(title = glue::glue("{toupper(cntry)}'S TREATMENT TARGET GOALS"),
                   subtitle = glue::glue("{yr} | {denom} Base"))

}











#' @title 95's Table Plot
#' @description This function creates a summary table showing OU progress toward the 95-95-95's (Deprecated)
#' @details Use `tab_95s_progress` instead.
#' @param df dataframe from `load_unaids`
#' @param cntry  PEPFAR country to visualize (list OU name)
#' @param denom which denominator/base to use, "PLHIV" (default) or "Relative"
#' @param grp age/sex group, c("All", "Female 15+", "Male 15+", "Peds <15")
#' @param yr year in question, defaults to max year
#'
#' @return gt table
#' @export
#' @seealso [load_unaids()]
#'
#' @examples
#'  \dontrun{
#'   df_unaids <- load_unaids()
#'    base_plot(df_unaids, "Kenya", denom = "PLHIV")
#'    base_plot(df_unaids, "Kenya", denom = "Relative")
#' }
#'

base_plot <- function(df, cntry,
                      denom = c("PLHIV", "Relative"),
                      grp = c("All", "Female 15+", "Male 15+", "Peds <15"),
                      yr = NULL){

  lifecycle::deprecate_warn("2.0.0", base_plot(), tab_95s())

  if(length(cntry) == 0){
    cli::cli_abort("No country provided. Please specify the country using the {.arg cntry} param.")
  }

  if(length(cntry) > 1){
    cli::cli_abort("Multiple countries provided. Please specify only one country using the {.arg cntry} param.")
  }

  if(length(denom) > 1){
    denom <- denom[1]
    # cli::cli_warn("No selection was made for the {.arg denom}; choosing {.val {denom}}.")
  }

  if(is.null(yr))
    yr <- unaids_year

  key_ind_95s <- c("Percent Known Status of PLHIV",
                   "Percent on ART of PLHIV",
                   "Percent VLS of PLHIV",
                   "Percent on ART with Known Status",
                   "Percent VLS on ART")

  df_tt <- df %>%
    dplyr::filter(year == {yr},
                  country == {cntry},
                  indicator %in% key_ind_95s)

  if(nrow(df_tt) == 0)
    cli::cli_abort("No data available in dataset for {.code cntry = {cntry}}.")

  df_tt <- df_tt %>%
    dplyr::select(year, country, indicator, age, sex, estimate, lower_bound, upper_bound) %>%
    tidyr::unite(group, c(sex, age), sep = " ") %>%
    dplyr::mutate(group = stringr::str_remove(group, "All "),
                  group = ifelse(group == "0-14", "Peds <15", group),
                  indicator = factor(indicator, key_ind_95s),
                  bounds = glue::glue("[{lower_bound}%-{upper_bound}%]")) %>%
    dplyr::relocate(bounds, .after = estimate) %>%
    dplyr::filter(group %in% grp) %>%
    dplyr::arrange(group, indicator) %>%
    calc_95_goals()

  df_viz <- df_tt %>%
    dplyr::filter(base %in% c("Both", {denom})) %>%
    dplyr::mutate(achieved = estimate >= goal_rate) %>%
    dplyr::select(-c(set, base, achv_plhiv, achv_relative, lower_bound, upper_bound))

  df_viz %>%
    gt::gt(groupname_col = "group") %>%
    gt::cols_hide(c(year, country)) %>%
    gt::fmt_percent(columns = c(estimate, goal_rate),
                    decimals = 0, scale_values = FALSE) %>%
    gt::cols_label(goal_rate = "goal") %>%
    gt::cols_align("left", indicator) %>%
    gtExtras::gt_theme_nytimes() %>%
    gt::tab_source_note(source_note = gt::md(source_note)) %>%
    gt::tab_options(source_notes.font.size = gt::px(8),
                    data_row.padding = gt::px(1),
                    table.font.size = gt::px(12)) %>%
    gtExtras::gt_color_rows(achieved,
                            palette = c(glitr::si_palettes$tango_t[3], glitr::viking),
                            domain = c(0,1),
                            pal_type = "discrete") %>%
    gt::tab_header(title = glue::glue("{toupper(cntry)}'S TREATMENT TARGET GOALS"),
                   subtitle = glue::glue("{yr} | {denom} Base"))

}


