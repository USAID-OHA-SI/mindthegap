

# Step 1. Pull the epi control data you need for all countries, store in a df.
  # Recommendation: Make functions to pull the data

# Step 2. Make a dataframe that combines the two dataframes and preps the epi data

# Step 3. Create a plotting function that will return the epi curves

# Step 4. Create an ou list against which entried are checked

# Step 5. Test



# PRELIMINARIES -----------------------------------------------------------

  library(magrittr)
  library(extrafont)
  glamr::load_secrets()



# FUNCTIONS ---------------------------------------------------------------

  # Help function to pull unaids data
  create_epi_df <- function(){
    df_epi <- mindthegap::pull_unaids( #pull_unaids grabs from July 2022 UNAIDS data by default
      data_type = "HIV Estimates", #change sheetname to data_type - 1 of 2 options
      pepfar_only = TRUE) %>%
      dplyr::filter(#stat == "est",
        age == "All",
        indicator %in% c("Number New HIV Infections")) %>% #grab new infections indicator
      dplyr::select(year, country,indicator, estimate) %>% #change indicator value to estimate
      dplyr::arrange(country, indicator, year)

    return(df_epi)
  }


  # helper function to pull total deaths
  create_tot_dths_df <- function(g_id = "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"){
    df_deaths <- googlesheets4::range_speedread(ss = g_id, sheet = "UNAIDS_epi_control") %>%
      dplyr::filter(indicator == "Number Total Deaths HIV Pop",
                    geo_level == "Country", #drop regional and sub-nat data
                    age == "all",
                    sex == "all")  %>%
      dplyr::select(c(country, year, indicator, estimate)) %>%
      tidyr::spread(indicator, estimate) %>%
      janitor::clean_names() %>%
      dplyr::rename(total_deaths = number_total_deaths_hiv_pop)
    return(df_deaths)
  }


  # Create the data frame you need
  get_epi_curve_df <- function(){

    # Pull in total deaths and epi data
    df_deaths <- create_tot_dths_df()
    df_epi <- create_epi_df()

    # Perform necessary munging
    df_epi_ous <-
      df_epi %>%
      dplyr::mutate(indicator = word(indicator, -1) %>% tolower) %>%
      tidyr::pivot_wider(names_from = "indicator", values_from = "estimate") %>%
      dplyr::left_join(df_deaths, by = c("year", "country"))

    # Add in ALL PEPFAR data
    df_epi_pepfar <-
      df_epi_ous %>%
      dplyr::bind_rows(df_epi_ous %>%
                  dplyr::mutate(country = "All PEPFAR") %>%
                  dplyr::group_by(country, year) %>%
                  dplyr::summarise(across(where(is.numeric), \(x) sum(x,na.rm = TRUE)), .groups = "drop"))

    # Create necessary vars
    df_epi_pepfar <-
      df_epi_pepfar %>%
      dplyr::mutate(declining_deaths = total_deaths - dplyr::lag(total_deaths, order_by = year) <= 0, by = c(country)) %>% #TRUE/FALSE declining
      dplyr::mutate(infections_below_deaths = infections < total_deaths,
             ratio = infections / total_deaths,
             direction_streak = sequence(rle(declining_deaths)$lengths),
             epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>%
      tidyr::pivot_longer(c(infections, total_deaths), names_to = "indicator") %>% #put back indicators in column
      dplyr::arrange(country, indicator, year) %>%
      dplyr::mutate(value_mod = ifelse(indicator == "total_deaths", -value, value),
             fill_color = ifelse(indicator == "total_deaths", glitr::old_rose, glitr::denim))

    return(df_epi_pepfar)

  }


  # Probably should make this an object that is bundled with package
  # So it does not need to be called each time
  pull_ou_list <- function(){
    ou_list <- df_epi %>% dplyr::distinct(country) %>% pull()
    return(ou_list)
  }

  # Plotting function to make the epi curves
  # By default, it will produce the All PEPFAR curve
  epi_plot <- function(df = df_epi, sel_cntry = c("All PEPFAR")){

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
                    min_plot_pt = min(value_mod),
                    lab_pt = dplyr::case_when(year == max(year) ~ value_mod)) %>%
      dplyr::mutate(cntry_order = max(value, na.rm = T), .by = country) %>%
      dplyr::mutate(country = fct_reorder(country, cntry_order, .desc = T))

    suppressWarnings(df_viz %>%
      ggplot2::ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
      ggplot2::geom_blank(aes(y = max_plot_pt)) + #sets max y-axis above
      ggplot2::geom_blank(aes(y = -max_plot_pt)) + #sets max y-axis below
      ggplot2::geom_area(alpha = 0.25) +
      ggplot2::geom_hline(yintercept = 0,color = glitr::grey80k) +
      ggplot2::geom_line() +
      ggplot2::geom_point(aes(y = lab_pt), na.rm = TRUE, shape = 21, color = "white", size = 3) +
      ggplot2::geom_text(aes(label = val_lab), na.rm = TRUE, #value label text
                         hjust = -0.3,
                         family = "Source Sans Pro Light") +
      ggplot2::facet_wrap(~country) + #small multiples of countries
      #scale_y_continuous(labels = ~(scales::label_number_si())(abs(.))) + #deprecated - use 'scale_cut'
      ggplot2::scale_y_continuous(labels = ~ (scales::label_number(scale_cut = scales::cut_short_scale())(abs(.))),
                                  expand = c(0, 0))+
      ggplot2::scale_x_continuous(breaks = seq(1990, 2025, 5)) +
      ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
      ggplot2::labs(x = NULL, y = NULL) + coord_cartesian(expand = T, clip = "off") +
      glitr::si_style_ygrid(facet_space = 0.75) +
      ggplot2::theme(axis.text.y = ggtext::element_markdown()) +
      ggplot2::labs(caption = "Source: UNAIDS Data 2022 Release"))

  }



# TEST IT -----------------------------------------------------------------

  # Get the dataframe
  df_epi <- get_epi_curve_df()

  # This should be bundled with the package as an object we can call internally
  ou_list <- pull_ou_list()

  # Test a few cases
  epi_plot(df_epi, sel_cntry = c("South Africa", "Zambia", "Malawi", "Kenya"))

  # Break it with NON-PEPFAR entries
  epi_plot(df_epi, sel_cntry = "USA")

  # Will it batch?
  purrr::map(ou_list, ~epi_plot(df_epi, sel_cntry = .x))






