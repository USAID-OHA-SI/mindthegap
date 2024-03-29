#' Combo Plot
#'
#' @param ctry_sel country to visualize
#' @param folderpath_output folderpath where outputs will be stored, if NULL (default) no outputsaved
#'
#' @return combined plots of 90-90-90 trends and ART gap by age/sex
#'
#' @keywords internal
#'

combo_plot <- function(ctry_sel, folderpath_output = NULL){

  lifecycle::deprecate_stop("1.1.0", "viz_unaids()", "plot_epi()")

  print(ctry_sel)

  v1 <- viz_unaids(ctry_sel)

  v2 <- viz_impatt(ctry_sel)

  vc <- gridExtra::grid.arrange(v1, v2, nrow = 1)

  plotname <- glue::glue("GapAnalysis_{ctry_sel}.png")

  if(!is.null(folderpath_output)){
    ggsave(plotname, vc, path = folderpath_output, dpi = 300, width = 10, height = 5.63)
  } else {
    return(vc)
  }

}





#' Visualize 90-90-90 Trends from UNAIDS
#'
#' @param ctry_sel country to visualize
#'
#' @return trend plots
#'
#' @import ggplot2
#'
#' @keywords internal

viz_unaids <- function(ctry_sel){

  lifecycle::deprecate_stop("1.1.0", "viz_unaids()", "plot_epi()")

  if(exists("df_unaids") == FALSE)
    stop("No data. Run load_data()")

  df_viz <- df_unaids %>%
    dplyr::filter(country == ctry_sel) %>%
    dplyr::mutate(label = dplyr::case_when(year %in% c(2015, 2019) ~ label))

  df_viz %>%
    ggplot(aes(year, value, group = indicator, color = indicator)) +
    geom_hline(yintercept = .9, color = "gray50") +
    geom_hline(yintercept = 0, color = "gray30") +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(size = 4, na.rm = TRUE) +
    geom_text(aes(label = label),
              size = 2.5, family = "GillSans",
              vjust = -1.7,  na.rm = TRUE) +
    scale_y_continuous(labels = scales::percent_format(1), position = "right") +
    scale_x_continuous(breaks =seq(2015, 2019, 2)) +
    scale_color_manual(values = blue_palette) +
    expand_limits(y = c(0, 1.2), x = c(2014.5, 2019.5)) +
    labs(x = NULL, y = NULL,
         title = toupper(ctry_sel),
         subtitle = "Treatment Cascade (15+)",
         caption = "Source: UNAIDS, Progress towards 90-90-90 targets") +
    facet_grid(sex ~ indicator, switch = "y") +
    mtg_theme() +
    theme(axis.text.y = element_blank())

}




#' Visualize share of PLHIV on ART
#'
#' @param ctry_sel country to visualize
#'
#' @return gap plot
#'
#' @import ggplot2
#'
#' @keywords internal

viz_impatt <- function(ctry_sel){

  lifecycle::deprecate_stop("1.1.0", "viz_impatt()")

  if(exists("df_impatt") == FALSE)
    stop("No data. Run load_data()")

  df_viz <- dplyr::filter(df_impatt, countryname == ctry_sel)

  df_viz %>%
    ggplot(aes(ageasentered, fill = sex, color = sex)) +
    geom_blank(aes(y = PLHIV * 1.1)) +
    geom_col(aes(y = PLHIV), fill = "white", width = .75, na.rm = TRUE) +
    geom_col(aes(y = TX_CURR_SUBNAT ), width = .75, na.rm = TRUE) +
    geom_hline(yintercept = 0) +
    geom_text(aes(y = PLHIV, label = scales::percent(share_on_ART, 1)),
              size = 3.5, family = "GillSans",
              hjust = -.5, na.rm = TRUE) +
    scale_y_continuous(labels = scales::comma, expand = c(0.005, 0.005)) +
    scale_fill_manual(values = blue_palette) +
    scale_color_manual(values = blue_palette) +
    coord_flip() +
    facet_grid(sex ~ .) +
    labs(x = NULL, y = NULL,
         title = " ",
         subtitle =  "PLHIV and Share on Treatment",
         caption = "Source: FY20Q4 NAT_SUBNAT dataset") +
    mtg_theme() +
    theme(panel.grid.major.x = element_line(color = grid_gray))

}
