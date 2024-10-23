#' Prepare UNAIDS epidemiological data for analysis
#'
#' This function processes UNAIDS data by filtering for relevant indicators
#' (HIV total deaths and new infections), selects specific columns, and reshapes
#' the data into a wide format. The resulting dataset has renamed columns for
#' deaths and infections.
#'
#' @return A data frame with columns for region, country, year, iso code, PEPFAR status,
#'         and estimates of deaths and new infections.
#' @export
prep_epi_data <- function() {
  df <- load_unaids() %>%
    dplyr::filter(
      indicator %in% c("Number Total Deaths to HIV Population", "Number New HIV Infections"),
      age == "All",
      sex == "All"
    ) %>%
    dplyr::select(region, country, year, iso, indicator, estimate, pepfar) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = estimate) %>%
    dplyr::rename_with(~ stringr::str_replace(., ".*Deaths.*", "deaths"), tidyselect::contains("Deaths")) %>%
    dplyr::rename_with(~ stringr::str_replace(., ".*Infections.*", "infections"), tidyselect::contains("Infections"))

  return(df)
}


#' Add Annotation to a ggplot
#'
#' Adds an angled text annotation to a ggplot at a specified position.
#'
#' @param p A ggplot2 plot object.
#' @param annotation_label A character string specifying the text to annotate the plot with. Default is "<-- Epidemic control".
#' @param annotation_proportion A numeric value between 0 and 1 indicating the position of the annotation relative to the axis ranges. Default is 0.9.
#'
#' @return A ggplot2 plot object with the annotation added.
#' @export
adorn_annotation <- function(p, annotation_label = "<-- Epidemic control",
                             annotation_proportion = 0.9) {

  # Build the plot to access axis limits
  built_plot <- ggplot2::ggplot_build(p)

  # Extract axis limits
  x_max <- built_plot$layout$panel_params[[1]]$x.range[2]
  y_max <- built_plot$layout$panel_params[[1]]$y.range[2]

  # Calculate annotation position based on the specified proportion
  x_pos <- x_max * annotation_proportion
  y_pos <- y_max * annotation_proportion

  # Add the annotation to the plot
  p <- p +
    ggplot2::annotate(
      "text",
      x = x_pos,
      y = y_pos,
      label = annotation_label,
      angle = 45,
      hjust = 1,    # Slightly to the left
      vjust = 1.5,  # Slightly below
      color = glitr::grey50k,
      size = 10 / ggplot2::.pt
    )

  return(p)
}

# Broken down into bite-sized nuggets
#' Prepare Data for Plotting
#'
#' Filters data based on country and year range and identifies min and max years.
#'
#' @param .data Data frame containing the data.
#' @param country Character vector of countries to include.
#' @param year_range Numeric vector of years to include.
#' @return A list containing filtered data and min-max year data frames.
#' @export
prepare_plot_data <- function(.data, country, year_range) {
  # Ensure country is treated as a vector
  country <- as.vector(country)

  # Filter the dataset
  filtered_data <- .data %>%
    dplyr::filter(year %in% year_range, country %in% {{country}})

  # Min and max year data
  min_max_year <- filtered_data %>%
    dplyr::filter(year == min(year) | year == max(year))

  list(filtered_data = filtered_data, min_max_year = min_max_year)
}


#' Calculate Axis Limits
#'
#' Calculates global min and max for x and y axes.
#'
#' @param filtered_data Data frame with filtered data.
#' @return A numeric vector with global min and max.
#' @export
calculate_axis_limits <- function(filtered_data) {
  global_min <- min(c(filtered_data$deaths, filtered_data$infections), na.rm = TRUE)
  global_max <- max(c(filtered_data$deaths, filtered_data$infections), na.rm = TRUE)
  c(global_min, global_max)
}


#' Construct Epidemic Control Plot
#'
#' Builds the ggplot object with all layers except the annotation.
#'
#' @param filtered_data Data frame with filtered data.
#' @param min_max_year Data frame with min and max year data.
#' @param axis_limits Numeric vector with global min and max.
#' @return A ggplot2 plot object.
#' @export
construct_epi_plot <- function(filtered_data, min_max_year, axis_limits) {
  global_min <- axis_limits[1]
  global_max <- axis_limits[2]

  ggplot2::ggplot(data = filtered_data, ggplot2::aes(x = deaths, y = infections, group = country)) +
    ggplot2::geom_path(
      ggplot2::aes(color = country),
      arrow = grid::arrow(angle = 25, ends = "last", type = "closed", length = grid::unit(0.1, "inches")),
      linewidth = 1.5, lineend = "round",
      show.legend = c(linetype = FALSE, color = TRUE)
    ) +
    ggplot2::geom_point(
      data = min_max_year %>% dplyr::filter(year == min(year)),
      ggplot2::aes(color = country),
      shape = 16, size = 5
    ) +
    ggrepel::geom_text_repel(
      data = min_max_year %>% dplyr::filter(year == max(year)),
      ggplot2::aes(label = paste(country, year, sep = " "), color = country),
      size = 10 / ggplot2::.pt,
      family = "Source Sans Pro"
    ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, color = glitr::grey30k, linetype = "dashed"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(global_min, global_max),
      labels = scales::comma
    ) +
    ggplot2::scale_y_continuous(
      limits = c(global_min, global_max),
      labels = scales::comma
    ) +
    glitr::si_style() +
    glitr::scale_color_si(palette = "hunter_d", discrete = TRUE) +
    ggplot2::scale_shape_manual(guide = "none") +
    ggplot2::labs(
      x = "<-- Total Deaths -->", y = "<-- New Infections -->",
      color = "Group",
      caption = mindthegap::source_note
    ) +
    ggplot2::coord_fixed(ratio = 1)
}


#' Plot Epidemic Control Trajectories
#'
#' Generates a plot showing trajectories of deaths and new infections over time for specified countries.
#'
#' @param .data Data frame containing the data.
#' @param country Character vector of countries to include.
#' @param year_range Numeric vector of years to include. Default is \code{2010:2023}.
#' @return A ggplot2 plot object.
#' @export
plot_epi_cntrl <- function(.data, country, year_range = 2010:2023) {

  # Data Preparation
  data_list <- prepare_plot_data(.data, country, year_range)
  filtered_data <- data_list$filtered_data
  min_max_year <- data_list$min_max_year

  # Axis Limits Calculation
  axis_limits <- calculate_axis_limits(filtered_data)

  # Plot Construction
  plot <- construct_epi_plot(filtered_data, min_max_year, axis_limits)

  # Add Annotation
  p <- adorn_annotation(plot)

  return(p)
}
