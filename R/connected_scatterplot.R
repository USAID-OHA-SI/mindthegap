#' Prepare UNAIDS Epidemiological Data for Analysis
#'
#' This function filters and processes UNAIDS epidemiological data by selecting relevant indicators
#' ("Number Total Deaths to HIV Population" and "Number New HIV Infections"), reshaping the data into
#' a wide format, and renaming the resulting columns for readability. It is intended to prepare
#' the dataset for a race to the bottom visualization.
#'
#' @param df A data frame containing the pre-processed UNAIDS EDMS data.
#' @param sel_cntry A character vector of country names for which data should be filtered. The
#' function will filter the dataset to include only these countries.
#' @param year_range A numeric vector specifying the range of years to filter the dataset by. Only
#' data within this range will be retained.
#'
#' @return A filtered and reshaped data frame containing the selected countries, indicators, and
#' years. The resulting data frame includes columns for `region`, `country`, `year`, `iso`, `pepfar`,
#' and estimates of `deaths` and `infections`.
#'
#' @details The function first filters the data to include only the selected countries (`sel_cntry`),
#' specified year range (`year_range`), and the indicators "Number Total Deaths to HIV Population"
#' and "Number New HIV Infections". It selects columns relevant for further analysis, reshapes the
#' data into a wide format, and renames the indicator columns to
#' `deaths` and `infections` for better readability. If no data matches the provided filters,
#' an error is thrown to inform the user.
#'
#' @note This function assumes that the dataset includes all age and sex categories set to "All".
#' Make sure to provide a correctly formatted dataset and verify country names to avoid errors.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Load a sample UNAIDS dataset
#'   df_unaids <- load_unaids() # Assume this function loads your dataset
#'
#'   # Prepare data for Lesotho for the years 2010-2023
#'   df_prepared <- prep_epi_data(df_unaids, sel_cntry = "Lesotho", year_range = 2010:2023)
#' }
prep_epi_data <- function(df, sel_cntry, year_range) {
  df_epi_fltrd <- df %>%
    dplyr::filter(
      country %in% sel_cntry,
      indicator %in% c("Number Total Deaths to HIV Population", "Number New HIV Infections"),
      age == "All",
      sex == "All",
      year %in% year_range
    ) %>%
    dplyr::select(region, country, year, iso, indicator, estimate, pepfar) %>%
    tidyr::pivot_wider(names_from = "indicator", values_from = estimate) %>%
    dplyr::rename_with(~ stringr::str_replace_all(., c(".*Deaths.*" = "deaths", ".*Infections.*" = "infections")))

  # Add a check for empty data or countries missing from dataset
  if(nrow(df_epi_fltrd) == 0)
    cli::cli_abort("No data available in dataset for the input country(ies). Maybe double check for spelling?")

  return(df_epi_fltrd)
}


#' Add Annotation to a ggplot Object
#'
#' This internal function adds a custom text annotation to a ggplot object, positioning it at a specified
#' proportion of the plot's x and y axis ranges. The default annotation is used to indicate epidemic control.
#'
#' @param p A ggplot2 plot object to which the annotation will be added.
#' @param annotation_label A character string specifying the text to annotate the plot with.
#'        The default value is "<-- Epidemic control".
#' @param annotation_proportion A numeric value between 0 and 1 indicating the position of the annotation
#'        relative to the axis ranges. Default is 0.9, placing the annotation close to the top-right of the plot.
#'
#' @return The original ggplot2 plot object with the annotation added.
#'
#' @details This function calculates the annotation position based on the specified proportion of the
#' plot's axis limits. The annotation is angled at 45 degrees to provide visual emphasis, typically used to
#' indicate a threshold or a notable point in the data.
#'
adorn_annotation <- function(p, annotation_label = "<-- Epidemic control",
                             annotation_proportion = 0.9) {

  # Build the plot to access axis limits
  built_plot <- ggplot2::ggplot_build(p)

  # Extract axis limits for varying data ranges
  x_max <- built_plot$layout$panel_params[[1]]$x.range[2]
  y_max <- built_plot$layout$panel_params[[1]]$y.range[2]

  # Calculate annotation position based on the specified proportion
  # This offsets the Epidemic control text from the dotted line
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


#' Prepare Data for Plotting
#'
#' This internal function identifies the minimum and maximum year data points for use in plotting geometries. It could
#' probably be removed but is kept in case the `df_epi_fltrd` data frame is large.
#'
#' @param df_epi_fltrd A data frame created by `prep_epi_data` containing epidemiological data filtered by country and year.
#'
#' @return A list containing the original filtered data (`filtered_data`) and a data frame with the minimum and maximum year data (`min_max_year`).
#'
#' @keywords internal
prepare_plot_data <- function(df_epi_fltrd) {

  # Min and max year data to keep it tiny and use for start end geometries
  min_max_year <- df_epi_fltrd %>%
    dplyr::filter(year == min(year) | year == max(year))

  list(filtered_data = df_epi_fltrd, min_max_year = min_max_year)
}


#' Calculate Axis Limits
#'
#' This internal function calculates the global minimum and maximum values for the x and y axes.
#'
#' @param filtered_data A data frame containing columns `deaths` and `infections` to be used for axis limit calculation.
#'
#' @return A numeric vector of length two containing the global minimum and maximum values.
#'
#' @keywords internal
calculate_axis_limits <- function(filtered_data) {
  range(c(filtered_data$deaths, filtered_data$infections), na.rm = TRUE)
}


#' Construct Epidemic Control Plot
#'
#' This internal function builds a ggplot object to visualize the relationship between deaths and infections.
#'
#' @param filtered_data A data frame containing the filtered data with columns `deaths`, `infections`, and `country`.
#' @param min_max_year A data frame containing the data points for the minimum and maximum years for each country.
#' @param axis_limits A numeric vector of length two containing the global minimum and maximum values for the axes.
#' @param year_limits A numeric vector of length two containing the global min and max of the year ranges
#'
#' @return A ggplot2 plot object that visualizes the epidemic control trajectories.
#'
#' @keywords internal
construct_epi_plot <- function(filtered_data, min_max_year, axis_limits, year_limits) {
  global_min <- axis_limits[1]
  global_max <- axis_limits[2]

  year_min <- year_limits[1]
  year_max <- year_limits[2]

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
      caption = mindthegap::source_note,
      subtitle = glue::glue("Epidemic control from {year_min}:{year_max}")
    ) +
    ggplot2::coord_fixed(ratio = 1)
}


#' Plot Epidemic Control Trajectories
#'
#' Generates a connected scatterplot of trajectories for deaths and new infections for specified countries.
#'
#' @param .data A data frame containing epidemiological data. It should include columns such as `country`, `year`, `indicator`, `estimate`, and others required for processing.
#' @param sel_cntry A character vector of countries to include in the plot.
#' @param year_range A numeric vector specifying the range of years to include in the analysis. Default is `2010:2023`.
#'
#' @return A ggplot2 plot object showing the epidemic control trajectories with deaths on the x-axis and new infections on the y-axis.
#'
#' @details This function serves as a wrapper, orchestrating the preparation, calculation, and visualization of the data. It processes the input data, calculates appropriate axis limits, constructs the epidemic control plot, and finally adds an annotation for epidemic control.
#'
#' @export
#' @examples
#'  \dontrun{
#'    df_unaids <- load_unaids(pepfar_only = FALSE)
#'    plot_connected_scatter(df_unaids)
#'    plot_connected_scatter(df_unaids, sel_cntry = "Lesotho")
#'    plot_connected_scatter(df_unaids, sel_cntry = c("South Africa", "Zambia", "Kenya", "Malawi"))
#' }
plot_connected_scatter <- function(.data, sel_cntry, year_range = 2010:2023) {

  epi_data <- prep_epi_data(.data, sel_cntry, year_range)

  # Data Preparation
  data_list <- prepare_plot_data(epi_data)
  filtered_data <- data_list$filtered_data
  min_max_year <- data_list$min_max_year

  # Axis Limits Calculation
  axis_limits <- calculate_axis_limits(filtered_data)
  year_limits <- range(year_range)

  # Plot Construction
  plot <- construct_epi_plot(filtered_data, min_max_year, axis_limits, year_limits)

  # Add Annotation
  p <- adorn_annotation(plot)

  return(p)
}



