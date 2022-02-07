#' Epidemic Control Plot
#' @description This function creates epidemic control curves for selected OU or "ALL PEPFAR"
#' @param sel_cntry country to visualize (OU name or "ALL PEPFAR")
#'
#' @return epidemic control plot showing trends in new infections and AIDS-related deaths
#' @export
#'
#' @examples
#'  \dontrun{
#'    epi_plot(sel_cntry = "Lesotho")
#'    epi_plot(sel_cntry = "ALL PEPFAR")
#' }
#'

epi_plot <- function(sel_cntry) {

  df_epi <- mindthegap::pull_unaids(sheetname = "HIV Estimates - Integer", pepfar_only = TRUE)

  #Filter down to the estimate and all ages, and the indicators you need
  df_epi_pepfar <- df_epi %>%
    dplyr::filter(stat == "est",
                  age == "all",
                  indicator %in% c("AIDS Related Deaths", "New HIV Infections")) %>%
    # semi_join(pepfar_country_list, by = c("iso" = "countryname_iso")) %>%
    dplyr::select(year, country, indicator, value) %>%
    dplyr::arrange(country, indicator, year)

  df_epi_pepfar <- df_epi_pepfar %>%
    dplyr::mutate(indicator = stringr::word(indicator, -1) %>% tolower) %>% #simplifies the indicator names
    tidyr::pivot_wider(names_from = "indicator") %>% #pivots data wide into deaths and infections column
    dplyr::group_by(country) %>%
    dplyr::mutate(declining_deaths = deaths - dplyr::lag(deaths, order_by = year) <= 0) %>% #create a value that if true indicates declining deaths
    dplyr::ungroup() %>%
    dplyr::mutate(infections_below_deaths = infections < deaths,
                  ratio = infections / deaths,
                  direction_streak = sequence(rle(declining_deaths)$lengths),
                  epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) #epi control definition

  #Add colors to indicators and flip axis
  df_epi_pepfar <- df_epi_pepfar %>%
    tidyr::pivot_longer(c(infections, deaths), names_to = "indicator") %>%
    dplyr::arrange(country, indicator, year) %>%
    dplyr::mutate(value_mod = ifelse(indicator == "deaths", -value, value),
                  fill_color = ifelse(indicator == "deaths", glitr::old_rose, glitr::denim))

  if (sel_cntry == "ALL PEPFAR") {

    #PEPFAR
    df_viz_pepfar <- df_epi_pepfar %>%
      dplyr::mutate(country = "All PEPFAR") %>%
      dplyr::group_by(country, year, indicator, fill_color) %>%
      dplyr::summarise(dplyr::across(c(value, value_mod), sum, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~ scales::number(value, 1, scale = 1e-3, suffix = "k")),
                    max_plot_pt = max(value),
                    min_plot_pt = min(value_mod),
                    lab_pt = dplyr::case_when(year == max(year) ~ value_mod),
                    indicator = ifelse(indicator == "deaths", "AIDS Related Deaths", "New HIV Infections"),
                    new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),
                    aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))

    viz <- df_viz_pepfar %>%
      ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
      # geom_blank(aes(y = ymax)) +
      #  geom_blank(aes(y = -ymax)) +
      geom_area(alpha = .25) +
      geom_hline(yintercept = 0, color = glitr::grey80k) +
      geom_line() +
      geom_point(aes(y = lab_pt), na.rm = TRUE,
                 shape = 21, color = "white", size = 3) +
      geom_text(aes(label = val_lab), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      geom_text(aes(label = new_hiv_label, x = 2010, y = (max_plot_pt + 1000)), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      geom_text(aes(label = aids_label, x = 2010, y = (min_plot_pt + 1000)), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      facet_wrap(~country) +
      scale_y_continuous(labels = ~ scales::label_number_si()(abs(.))) +
      scale_x_continuous(breaks = seq(1990, 2025, 5)) +
      scale_fill_identity(aesthetics = c("fill", "color")) +
      # annotate(geom = "text", x = 1994, y = 2.8e6, label = c("New HIV Infections"), hjust = 0,
      #          family = "Source Sans Pro Light", color =  glitr::denim) +
      # annotate(geom = "text", x = 1994, y = -1.5e6, label = c("AIDS-related Deaths"), hjust = 0,
      #          family = "Source Sans Pro Light", color =  glitr::old_rose) +
      labs(x = NULL, y = NULL) +
      coord_cartesian(expand = T, clip = "off") +
      glitr::si_style_ygrid() +
      theme(axis.text.y = ggtext::element_markdown())

    suppressWarnings(print(viz))
  }

  else{

    #COUNTRY
    df_viz_cntry <- df_epi_pepfar %>%
      dplyr::filter(country %in% sel_cntry) %>%
      dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~ scales::number(value, 1, scale = 1e-3, suffix = "k")),
                    max_plot_pt = max(value),
                    min_plot_pt = min(value_mod),
                    lab_pt = dplyr::case_when(year == max(year) ~ value_mod),
                    country = factor(country, sel_cntry),
                    indicator = ifelse(indicator == "deaths", "AIDS Related Deaths", "New HIV Infections"),
                    #ind_lab = dplyr::case_when(value == max(value) ~ indicator),
                    new_hiv_label = dplyr::case_when(value == max_plot_pt ~ indicator),
                    aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))

    #VIZ
    viz <- df_viz_cntry %>%
      ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
      #geom_blank(aes(y = ymax)) +
      # geom_blank(aes(y = -ymax)) +
      geom_area(alpha = .25) +
      geom_hline(yintercept = 0, color = glitr::grey80k) +
      geom_line() +
      geom_point(aes(y = lab_pt), na.rm = TRUE,
                 shape = 21, color = "white", size = 3) +
      geom_text(aes(label = val_lab), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      geom_text(aes(label = new_hiv_label, x = 2010, y = (max_plot_pt + 1000)), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      geom_text(aes(label = aids_label, x = 2010, y = (min_plot_pt + 1000)), na.rm = TRUE,
                hjust = -0.3,
                family = "Source Sans Pro Light") +
      facet_wrap(~country) +
      scale_y_continuous(labels = ~ scales::label_number_si()(abs(.))) +
      scale_x_continuous(breaks = seq(1990, 2025, 10)) +
      scale_fill_identity(aesthetics = c("fill", "color")) +
      labs(x = NULL, y = NULL) +
      coord_cartesian(expand = T, clip = "off") +
      glitr::si_style_ygrid() +
      theme(axis.text.y = ggtext::element_markdown())

    suppressWarnings(print(viz))
  }

}

