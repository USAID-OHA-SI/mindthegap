#' Epidemic Control Plot
#' @description This function creates epidemic control curves for "ALL PEPFAR" or selected OU's
#' @param df UNAIDS based data frame
#' @param sel_cntry  PEPFAR country to visualize ("ALL PEPFAR" as default or list OU names)
#'
#' @return Epidemic control plot showing trends in new infections and total deaths to HIV population
#' @export
#'
#' @examples
#'  \dontrun{
#'    epi_plot()
#'    epi_plot(sel_cntry = "Lesotho")
#'    epi_plot(sel_cntry = c("South Africa", "Zambia", "Kenya", "Malawi"))
#'    epi_plot(sel_cntry = "USA") #breaks with non-PEPFAR countries
#' }
#'

epi_plot <- function(df = df_epi_pepfar, sel_cntry = c("All PEPFAR")){

  df_epi <- mindthegap::pull_unaids(data_type = "HIV Estimates",pepfar_only = TRUE) %>% #pull from PEPFAR Only estimates
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
    df %>%
    dplyr::filter(country %in% sel_cntry) %>% #change to listed countries
    dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~
                                               scales::number(value, 1, scale = 0.001, suffix = "k")),
                  max_plot_pt = max(value),
                  min_plot_pt = min(val_mod),
                  lab_pt = dplyr::case_when(year == max(year) ~ val_mod)) %>%
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
                     ggplot2::scale_x_continuous(breaks = seq(min(df$year), max(df$year),5)) + #automatic x-axis min/max
                     #ggplot2::scale_x_continuous(breaks = seq(1990, 2025, 5)) + #manual x-axis breaks
                     ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
                     #ggplot2::annotate(geom = "text", x = 2010, y =2.8e5, label = c("New HIV Infections"), hjust = 0,
                     #        family = "Source Sans Pro Light", color = glitr::denim) +  #add labels to plot
                     #ggplot2::annotate(geom = "text", x = 2010, y = -2.8e5, label = c("Total Deaths to HIV Population"), hjust = 0,
                     #                 family = "Source Sans Pro Light", color = glitr::old_rose) +
                     ggplot2::labs(x = NULL, y = NULL) + ggplot2::coord_cartesian(expand = T, clip = "off") +
                     glitr::si_style_ygrid(facet_space = 0.75) + #adjusted y-axis grid spacing with facet_space
                     ggplot2::theme(axis.text.y = ggtext::element_markdown()) +
                     ggplot2::labs(caption = "Source: UNAIDS Data 2022 Release"))

}
