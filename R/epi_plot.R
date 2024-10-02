#' Epidemic Control Plot
#' @description This function creates epidemic control curves for "ALL PEPFAR" or selected OU's
#'
#' @param df UNAIDS data frame loaded from `load_unaids()`
#' @param sel_cntry  PEPFAR country to visualize ("ALL PEPFAR" as default or list OU names)
#'
#' @return Epidemic control plot showing trends in new infections and total deaths to HIV population
#' @export
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

  #subset to PEPFAR
    # df <- dplyr::filter(df, pepfar == TRUE)

  #subset to epi indicators
  df_epi <- df %>%
    dplyr::filter(indicator %in% c("Number Total Deaths to HIV Population",
                                   "Number New HIV Infections"),
                  age == "All",
                  sex == "All") %>%
    dplyr::select(year, country, indicator, estimate) %>%
    dplyr::arrange(country, indicator, year)

  #include pepfar roll up if user specifies
  if("All PEPFAR" %in% sel_cntry)
    df_epi <- df_epi %>%
      dplyr::bind_rows(df_epi %>%
                         dplyr::filter(indicator != "Incidence mortality ratio (IMR)") %>%
                         dplyr::mutate(country = "All PEPFAR") %>%
                         dplyr::group_by(country, year, indicator) %>%
                         dplyr::summarise(estimate =  sum(estimate,na.rm = TRUE),
                                          .groups = "drop")) #sums PEPFAR country estimates

  #subset to only selected countries
  df_epi <- dplyr::filter(df_epi, country %in% sel_cntry)

  #create viz df, including point labels and colors
  df_viz <- df_epi %>%
    dplyr::group_by(country) %>%
    dplyr::mutate(plot_max = max(estimate, na.rm = TRUE)) %>% #equal bounds above/below axis
    dplyr::group_by(country, indicator) %>%
    dplyr::mutate(peak_val = max(estimate, na.rm = TRUE)) %>% #position of the indicator label
    dplyr::ungroup() %>%
    dplyr::mutate(val_mod = estimate,
                  dplyr::across(c(val_mod, peak_val, plot_max), \(x) ifelse(indicator == "Number Total Deaths to HIV Population", -x, x)),
                  peak_val = dplyr::case_when(year == 2005 ~ peak_val),
                  # fill_color = ifelse(indicator == "Number Total Deaths to HIV Population", glitr::old_rose, glitr::denim),
                  fill_color = ifelse(indicator == "Number Total Deaths to HIV Population", glitr::orchid_bloom, glitr::electric_indigo),
                  indicator = stringr::str_remove(indicator, "Number "), #creating labels
                  ind_label = dplyr::case_when(year == 2005 ~ indicator),  #assigning label location to min/max
                  val_lab = dplyr::case_when(year == max(year) ~
                                               scales::number(estimate, 1, scale_cut = scales::cut_si("")) %>% stringr::str_remove(" ")),
                  lab_pt = dplyr::case_when(year == max(year) ~ val_mod),
                  cntry_lab = country
    )

  #include IMR with country name for countries that have it
  if("Incidence mortality ratio (IMR)" %in% unique(df$indicator)){
    df_imr <- df %>%
      dplyr::filter(indicator == "Incidence mortality ratio (IMR)",
                    age == "All",
                    sex == "All",
                    country %in% sel_cntry,
                    year == max(year)) %>%
      dplyr::select(year, country, imr = estimate) %>%
      dplyr::mutate(cntry_lab_imr = stringr::str_glue("{country}\n({year} IMR = {round(imr, 1)})")) %>%
      dplyr::select(country, cntry_lab_imr)

    df_viz <- df_viz %>%
      dplyr::left_join(df_imr, by = "country") %>%
      dplyr::mutate(cntry_lab = ifelse(is.na(cntry_lab_imr), cntry_lab, cntry_lab_imr)) %>%
      dplyr::select(-cntry_lab_imr)
  }

  #order countries desc (for if there is more than one)
  df_viz <- df_viz %>%
    dplyr::mutate(cntry_order = max(estimate, na.rm = T), .by = country) %>%
    dplyr::mutate(cntry_lab = forcats::fct_reorder(cntry_lab, cntry_order, .desc = T))

  #plot
  suppressWarnings(
    df_viz %>%
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
      ggplot2::scale_x_continuous(breaks = seq(min(df_viz$year), max(df_viz$year),5)) + #automatic x-axis min/max
      ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
      ggplot2::coord_cartesian(expand = TRUE, clip = "off") + #keeps value labels from being cut off at right
      glitr::si_style_ygrid(facet_space = 0.75) #adjusted y-axis grid spacing with facet_space
  )

}
