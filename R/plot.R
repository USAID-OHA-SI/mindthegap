#' Combo Plot
#'
#' @param ctry_sel country to visualize
#' @param folderpath_output folderpath where outputs will be stored, if NULL (default) no outputsaved
#'
#' @return combined plots of 90-90-90 trends and ART gap by age/sex
#' @export
#'

combo_plot <- function(ctry_sel, folderpath_output = NULL){

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
#' @export

viz_unaids <- function(ctry_sel){

  if(exists("df_unaids") == FALSE)
    stop("No data. Run load_data()")

  df_viz <- dplyr::filter(df_unaids, country == ctry_sel)

  df_viz %>%
    ggplot(aes(year, value, group = indicator, color = indicator)) +
    geom_hline(yintercept = .9, color = "gray30") +
    geom_hline(yintercept = 0, color = "gray30") +
    geom_line(size = 1, na.rm = TRUE) +
    geom_point(size = 5, na.rm = TRUE) +
    geom_text(aes(label = label),
              size = 3.5, family = "Gill Sans MT",
              vjust = -1.7,  na.rm = TRUE) +
    scale_y_continuous(labels = scales::percent_format(1), position = "right") +
    scale_x_continuous(breaks =seq(2015, 2019, 2)) +
    scale_color_manual(values = c("#26456a", "#335B8E", "#739bcc")) +
    expand_limits(y = c(0, 1.2)) +
    labs(x = NULL, y = NULL,
         title = toupper(ctry_sel),
         subtitle = "Treatment Cascade (15+)",
         caption = "Source: UNAIDS, Progress towards 90-90-90 targets") +
    facet_grid(sex ~ indicator, switch = "y") +
    theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11, color = "#595959"),
          panel.grid = element_line(color = "#ebebeb"),
          plot.title = element_text(size = 15, face = "bold", color = "black"),
          plot.subtitle =element_text(size = 15, color = "#595959"),
          plot.caption = element_text(size = 9,  color = "#595959"))
}




#' Visualize share of PLHIV on ART
#'
#' @param ctry_sel country to visualize
#'
#' @return gap plot
#'
#' @import ggplot2
#'
#' @export

viz_impatt <- function(ctry_sel){

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
              size = 3.5, family = "Gill Sans MT",
              hjust = -.5, na.rm = TRUE) +
    scale_y_continuous(labels = scales::comma, expand = c(0.005, 0.005)) +
    scale_fill_manual(values = c("#26456a", "#335B8E")) +
    scale_color_manual(values = c("#26456a", "#335B8E")) +
    coord_flip() +
    facet_grid(sex ~ .) +
    labs(x = NULL, y = NULL,
         title = " ",
         subtitle =  "PLHIV and Share on Treatment",
         caption = "Source: FY20Q3c NAT_SUBNAT dataset") +
    theme(text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.background = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 11, color = "#595959"),
          panel.grid = element_line(color = "#ebebeb"),
          plot.title = element_text(size = 15, face = "bold", color = "black"),
          plot.subtitle =element_text(size = 15, color = "#595959"),
          plot.caption = element_text(size = 9,  color = "#595959"))

}
