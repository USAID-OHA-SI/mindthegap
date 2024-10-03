#' Epidemic Control Plot
#' @description This function creates epidemic control curves for "ALL PEPFAR" or selected OU's
#'
#' @param df UNAIDS data frame loaded from `load_unaids()`
#' @param sel_cntry  PEPFAR country to visualize ("ALL PEPFAR" as default or list OU names)
#'
#' @return Epidemic control plot showing trends in new infections and total deaths to HIV population
#' @export
#' @seealso [load_unaids()]
#'
#' @examples
#'  \dontrun{
#'    df_unaids <- load_unaids(pepfar_only = FALSE)
#'    epi_plot(df_unaids)
#'    epi_plot(df_unaids, sel_cntry = "Lesotho")
#'    epi_plot(df_unaids, sel_cntry = c("South Africa", "Zambia", "Kenya", "Malawi"))
#'    epi_plot(df_unaids, sel_cntry = "USA") #breaks with non-PEPFAR countries
#' }
#'

epi_plot <- function(df, sel_cntry = c("All PEPFAR")){

  df_epi <- subset_epi_data(df)

  df_epi <- add_pepfar_rollup(df_epi, sel_cntry)

  df_viz <- prepare_viz_data(df_epi, df_edms = df, sel_cntry)

  viz_epi(df_viz)

}

#' Subset data to select indicators needed
#'
#' Filter down data to only what is needed for the epi control plots
#'
#' @inheritParams epi_plot
#' @keywords internal
subset_epi_data <- function(df) {

  validate_columns(df, c("year", "country", "pepfar", "indicator", "age", "sex", "estimate"))

  validate_epi_disaggs(df)

  df_epi <- df %>%
    dplyr::filter(indicator %in% c("Number Total Deaths to HIV Population",
                                   "Number New HIV Infections"),
                  age == "All",
                  sex == "All") %>%
    dplyr::select(year, country, indicator, estimate) %>%
    dplyr::arrange(country, indicator, year)

  return(df_epi)
}


#' Validate Epi indicator/disaggs
#'
#' @inheritParams epi_plot
#' @keywords internal
#'
validate_epi_disaggs <- function(df){

  df_exp <- tibble::tibble(indicator = c("Number Total Deaths to HIV Population",
                                  "Number New HIV Infections"),
                 age = "All",
                 sex = "All") %>%
    dplyr::mutate(ind_combo = stringr::str_glue("{indicator}: {sex}|{age} [AIDS (AIM)]"))

  df_ind <- df %>%
    dplyr::distinct(indicator, age, sex)

  missing <- dplyr::anti_join(df_exp, df_ind,
                     by = dplyr::join_by(indicator, age, sex)) %>%
    dplyr::pull()

  if(length(missing) > 0){
    cli::cli_abort(c(
      "The following {length(missing)} expected item{?s} {?is/are} missing from the EDMS output:",
      stats::setNames(missing, rep("*", length(missing)))
    ))
  }

  df_extra <- tibble::tibble(indicator = "Incidence mortality ratio (IMR)",
                           age = "All",
                           sex = "All") %>%
    dplyr::mutate(ind_combo = stringr::str_glue("{indicator}: {sex}|{age} [AIDS (AIM)]"))

  df_ind <- df %>%
    dplyr::distinct(indicator, age, sex)

  missing_warn <- dplyr::anti_join(df_extra, df_ind,
                              by = dplyr::join_by(indicator, age, sex)) %>%
    dplyr::pull()

  if(length(missing_warn) > 0){
    cli::cli_warn(c(
      "The following {length(missing_warn)} extra (non-required) item{?s} {?is/are} missing from the EDMS output:",
      stats::setNames(missing_warn, rep("*", length(missing_warn)))
    ))
  }


}

#' Add PEPFAR Rollup
#'
#' If desired by user, add in a PEPFAR roll up
#'
#' @inheritParams epi_plot
#' @keywords internal
add_pepfar_rollup <- function(df, sel_cntry) {

  #exit if PEPFAR is not in the country selection list
  if (!"All PEPFAR" %in% sel_cntry)
    return(df)

  #add aggregation of PEPFAR countries
  df <- df %>%
    dplyr::bind_rows(df %>%
                       dplyr::filter(indicator != "Incidence mortality ratio (IMR)",
                                     pepar == TRUE) %>%
                       dplyr::mutate(country = "All PEPFAR") %>%
                       dplyr::group_by(country, year, indicator) %>%
                       dplyr::summarise(estimate = sum(estimate, na.rm = TRUE),
                                        .groups = "drop"))

  return(df)
}


#' Prepare viz data
#'
#' @inheritParams epi_plot
#' @param df_edms original df provided
#' @keywords internal
#'
prepare_viz_data <- function(df, df_edms, sel_cntry) {

  #subset country/countries
  df_cntry <- dplyr::filter(df, country %in% sel_cntry)

  #create viz df with equal plot bounds and position of indicator labels on plot
  df_viz <- df_cntry %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(plot_max = max(estimate, na.rm = TRUE)) %>% #equal bounds above/below axis
    dplyr::group_by(country, indicator) %>%
    dplyr::mutate(peak_val = max(estimate, na.rm = TRUE)) %>% #position of the indicator label
    dplyr::ungroup()

  #format country label
  df_fmt_cntry <- df_viz %>%
    dplyr::select(year, country, indicator, estimate) %>%
    dplyr::group_by(country, indicator) %>%
    dplyr::mutate(direction = dplyr::case_when(estimate == dplyr::lag(estimate, order_by = year) ~ "\u25B6", #no change
                                               estimate > dplyr::lag(estimate, order_by = year) ~ "\u25B2", #Increasing
                                               estimate < dplyr::lag(estimate, order_by = year) ~ "\u25BC"
    )) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::select(-estimate) %>%
    tidyr::pivot_wider(names_from = indicator,
                       values_from = direction,
                       names_glue = "{stringr::str_extract_all(tolower(indicator), 'deaths|infections')}") %>% #clean up indicator name to make easier when reshaped to column
    dplyr::mutate(cntry_lab = stringr::str_glue("**{country}**<br>{year}: *Total Deaths {deaths} | New Infections {infections}*")) %>%
    dplyr::select(country, cntry_lab)

  #add to viz df, including point labels and colors
  df_viz <- df_viz %>%
    dplyr::left_join(df_fmt_cntry, by = "country") %>%
    dplyr::mutate(val_mod = estimate,
                  dplyr::across(c(val_mod, peak_val, plot_max), \(x) ifelse(indicator == "Number Total Deaths to HIV Population", -x, x)),
                  peak_val = dplyr::case_when(year == 2005 ~ peak_val),
                  fill_color = ifelse(indicator == "Number Total Deaths to HIV Population", glitr::orchid_bloom, glitr::electric_indigo),
                  indicator = stringr::str_remove(indicator, "Number "), #creating labels
                  ind_label = dplyr::case_when(year == 2005 ~ indicator),  #assigning label location to min/max
                  val_lab = dplyr::case_when(year == max(year) ~
                                               scales::number(estimate, 1, scale_cut = scales::cut_si("")) %>% stringr::str_remove(" ")),
                  lab_pt = dplyr::case_when(year == max(year) ~ val_mod))

  #include IMR with country name for countries that have it
  if("Incidence mortality ratio (IMR)" %in% unique(df_edms$indicator)){
    df_imr <- df_edms %>%
      dplyr::filter(indicator == "Incidence mortality ratio (IMR)",
                    age == "All",
                    sex == "All",
                    country %in% sel_cntry,
                    year == max(year)) %>%
      dplyr::select(country, imr = estimate)

    df_viz <- df_viz %>%
      dplyr::left_join(df_imr, by = "country") %>%
      dplyr::mutate(cntry_lab = stringr::str_glue("{cntry_lab} *| IMR {round(imr, 1)}*")) %>%
      dplyr::select(-imr)
  }

  #order countries desc (for if there is more than one)
  df_viz <- df_viz %>%
    dplyr::mutate(cntry_order = max(estimate, na.rm = T), .by = country) %>%
    dplyr::mutate(cntry_lab = forcats::fct_reorder(cntry_lab, cntry_order, .desc = T))

  return(df_viz)
}


#' Standard Epi plot
#'
#' @inheritParams epi_plot
#' @keywords internal
#'
viz_epi <- function(df){

  suppressWarnings(
    df %>%
      ggplot2::ggplot(ggplot2::aes(year, val_mod,
                                   group = indicator,
                                   fill = fill_color,
                                   color = fill_color)) +
      ggplot2::geom_blank(ggplot2::aes(y = plot_max)) + #sets max y-axis above/below
      ggplot2::geom_area(alpha = 0.25) +
      ggplot2::geom_hline(yintercept = 0, color = glitr::grey80k) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(y = lab_pt), na.rm = TRUE,
                          shape = 21, color = "white", size = 3) +
      ggplot2::geom_text(ggplot2::aes(label = val_lab), na.rm = TRUE, #value label text
                         hjust = -0.3, family = "Source Sans Pro Light") +
      geom_text(aes(label = ind_label, y = peak_val), na.rm = TRUE,
                hjust = -0.3, family = "Source Sans Pro Light") +
      ggplot2::facet_wrap(~cntry_lab) + #small multiples of countries
      ggplot2::labs(x = NULL, y = NULL, caption = source_note) +
      ggplot2::scale_y_continuous(labels = ~ (scales::label_number(scale_cut = scales::cut_short_scale())(abs(.))),
                                  expand = c(0, 0)) +
      ggplot2::scale_x_continuous(breaks = seq(min(df$year), max(df$year),5)) + #automatic x-axis min/max
      ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
      ggplot2::coord_cartesian(expand = TRUE, clip = "off") + #keeps value labels from being cut off at right
      glitr::si_style_ygrid(facet_space = 0.75) + #adjusted y-axis grid spacing with facet_space
      ggplot2::theme(strip.text = ggtext::element_markdown())
  )
}
