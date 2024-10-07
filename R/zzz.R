.onLoad <- function (libname, pkgname)
{

  startup_msg()

  invisible ()
}

.onAttach <- function(...) {
  if(requireNamespace("gagglr", quietly = TRUE))
    gagglr::oha_check("mindthegap", suppress_success = TRUE)
}


utils::globalVariables(c("where", "operatingunit",
                         "countryname", ".", "sheet",
                         "names_original", "value", "country",
                         "year", "iso", "indicator", "sex",
                         "indic_type", "headrs", "standardizeddisaggregate",
                         "fiscal_year", "ageasentered", "targets", "TX_CURR_SUBNAT",
                         "PLHIV", "ind", "type", "lower", "upper", "df_impatt",
                         "share_on_ART", "df_unaids", "label", "desc", "across",
                         "age", "deaths", "infections", "declining_deaths",
                         "infections_below_deaths", "fill_color",
                         "value_mod", "lab_pt", "val_lab", "new_hiv_label",
                         "max_plot_pt", "aids_label", "min_plot_pt", "countryname_iso",
                         "high", "region", "low", "est", "country_iso", "estimate",
                         "upper_bound", "pepfar", "cntry_order", "gs_id",
                         "tot_death_label", "val_mod", "set", "goal_rate", "achieved",
                         "achv_plhiv", "achv_relative", "acronym", "country_pepfar",
                         "declining_infections", "e_cat", "e_count", "e_ind",
                         "epi_control", "epi_ratio", "estimate_flag", "formatted",
                         "indicator_edms", "indicator_map", "indicator_type",
                         "indicator_validation", "iso3", "lower_bound",
                         "other", "req_cols", "status", "time", "expected_ind",
                         "in_data", "expected", "achv_epi_control", "base",
                         "bounds", "cntry_lab", "direction", "epi_ratio_2023",
                         "group", "imr", "ind_label", "peak_val", "plot_max"
))
