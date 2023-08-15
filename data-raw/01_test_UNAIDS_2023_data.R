# PROJECT: 2023 UNAIDS Estimates
# PURPOSE: Test Functions
# AUTHOR: Lemlem Baraki | SI
# REF ID:   621970ea
# LICENSE: MIT
# DATE: 2023-07-14
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(gophr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(mindthegap)
    library(googlesheets4)
    library(googledrive)


  # SI specific paths/functions
    load_secrets()

  # Grab metadata

  # REF ID for plots
    ref_id <- "621970ea"

    # Steps:
      #1. Load the new gs_ids
      #2. Pull the UNAIDS data with the read_rename() and validate_cols() function
      #3. Run the munge_unaids() to see how it cleans the data
      #4. Test and update until you have a clean dataset


# LOAD DATA ============================================================================

  #NEW IDS
    gs_id_unaids <- "1ENUF8tKegwkbSck489aA2S1C_BEEvkgJG2hOevUKDyw"
    #original UNAIDS data - 2023 estimates (new)

    gs_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw"
    #UNAIDS crosswalk (updated)

    gs_clean_id <- "1TivNwrgVKGfm7maCIr9CwZgVyakS_zxbYvVPMX4VKcw"
    #Clean UNAIDS estimates data on google drive (new)

    pepfar_clean_id <- "1FGLyx0lFsJGEsPU_eTc1tRPuoS-swopc_78eOweNd-M"
    #PEPFAR only Clean UNAIDS data on google drive (new)

    drive_id <- googledrive::as_id("1-iCrHGyU-xfDmzdfgXJ1P_wLI90s5RR-")
    #UNAIDS drive folder (same)

    nat_unaids <- "1Brg_v0rXtDcvtdrUkmyjztu4Vwzx_yzUcw4EzzKSA98"
    #national data from EDMS - 2023 estimates (updated)

# FUNCTIONS ============================================================================

  #Check utilities.R------------------------------------------------------------------------------

    read_rename <- function(return_type) {

      #to specify NA's when reading in data
      missing <- c("...", " ")

      sheetname = ifelse(return_type == "HIV Estimates", "HIV2023Estimates_ByYear", "HIV-Test-&-Treat_ByYear")
      skip_n = ifelse(sheetname == "HIV2023Estimates_ByYear", 5, 4) #update year and skips

      gdrive_df <- suppressMessages(
        googlesheets4::read_sheet(gs_id_unaids, sheet = sheetname, skip = skip_n, na = missing, col_types = "c") %>% #reads in as column as character string
          dplyr::rename(year = !!names(.[1]),
                        iso =  !!names(.[2]),
                        country =  !!names(.[3]))
      )

      gdrive_df <- validate_cols(gdrive_df, return_type)

      if (return_type == "HIV Test & Treat") {
        gdrive_df <-  suppressWarnings(
          gdrive_df %>%
            dplyr::mutate(across(tidyselect:::where(is.list), ~dplyr::na_if(., "NULL"))) %>%
            dplyr::slice(-c(1,2))
        )
      }

      return(gdrive_df)
    }


    validate_cols <- function(df, return_type) {

      sheetname <- ifelse(return_type == "HIV Estimates", "HIV estimates - by Year", "HIV Test & Treat - by Year ")

      names_cw <- suppressMessages(
        googlesheets4::read_sheet(gs_id_names, sheet = sheetname) %>%
          #dplyr::filter(sheet == "HIV Test & Treat - by Year ") %>%
          dplyr::select(-sheet) %>%
          tidyr::pivot_wider(names_from = names,
                             values_from = names_original) %>% #udpate names in crosswalk to match
          dplyr::select(-value)
      )

      #change column names - stop if length of names is not the same as length of df
      stopifnot(ncol(names_cw) == ncol(df))
      names(df) <- names(names_cw)

      return(df)

    }

  #Check munge_unaids function---------------------------------------------------------------------
    munge_unaids <- function(return_type, indicator_type) {

      # Google Sheet ID to original
      #sheet_id <- googledrive::as_id("1tkwP532mPL_yy7hJuHNAHaZ1_K_wd7zo_8AjeOe7fRs") #gdrive

      #sheet_id_names <- "1vaeac7hb7Jb6RSaMcxLXCeTyim3mtTcy-a1DQ6JooCw" #crosswalk

      #Read Data from googlesheet and validate columns
      gdrive_df <- read_rename(return_type)

      #Munge
      gdrive_df_clean <-
        gdrive_df %>%
        #dplyr::mutate(dplyr::across(tidyselect::contains("_"), ~gsub(" |<|>", "", .))) %>% #replace special characters
        #dplyr::mutate(dplyr::across(tidyselect::matches("\\_"), ~gsub("m","00000", .x)))%>% #replace unit values - matches uses regular expression
        #dplyr::mutate(dplyr::across(tidyselect::matches("\\_"), ~gsub("\\.","",.x))) %>%
        #dplyr::mutate(dplyr::across(tidyselect::contains("_"),~ as.numeric(.x)))%>% #indicator columns into numeric
        dplyr::mutate(region = ifelse(country %in% regions, country, NA)) %>%
        tidyr::fill(region) %>% #get regions column
        tidyr::pivot_longer(-c(year, iso, country, region),
                            names_to = c("indicator")) %>% #get indicator column
        tidyr::separate(indicator, sep = "_", into = c("indicator", "age", "sex", "stat")) %>% #separate merged data age/sex/stat
        tidyr::pivot_wider(names_from = 'stat', values_from = "value") %>% #get est/low/high columns
        #dplyr::mutate(estimate_flag = ifelse(str_detect(est, "<"), TRUE, FALSE)) %>% #estimate flag
        dplyr::mutate(dplyr::across(c(est:high), ~gsub(" |<|>", "", .))) %>% #replace special characters
        dplyr::mutate(dplyr::across(c(est:high), ~ gsub("m","00000", .x))) %>% #replace unit values
        dplyr::mutate(dplyr::across(c(est:high), ~ ifelse(grepl("\\.\\d+00000$", .x), gsub("\\.", "", .x), .x)))
        #dplyr::mutate(dplyr::across(c(est:high), ~ as.numeric(.x)))

      #Add sheet and indicator type variable
      gdrive_df_clean <- gdrive_df_clean %>%
        dplyr::mutate(sheet = return_type,
                      sex = ifelse(indicator == "pmtct", "female", sex),
                      indic_type = dplyr::case_when(
                        indicator %in% c("prev", "incidence", # HIV Estimates indicators
                                         "knownstatus", "plhivOnArt", "knownstatusOnArt",
                                         "plhivVLS", "onArtVLS", "pmtctArtPct" #T&T indicaotrs
                        ) ~ "percent_indics",
                        TRUE ~ "integer_indics"
                      )
        )

      #Recode indicators and rename columns
      gdrive_df_clean <- gdrive_df_clean %>%
        dplyr::mutate(indicator = dplyr::recode(indicator,
                                                "prev" = "Percent Prevalence",
                                                "deaths" = "Number AIDS Related Deaths",
                                                "plhiv" = "Number PLHIV",
                                                "incidence" = "Percent Incidence",
                                                "pmtct" = "Number PMTCT Needing ART",
                                                "newhiv" = "Number New HIV Infections",
                                                "knownstatus" = "Percent Known Status of PLHIV", #T&T indicators
                                                "plhivOnArt" = "Percent on ART of PLHIV",
                                                "knownstatusOnArt" = "Percent on ART with Known Status",
                                                "plhivVLS" = "Percent VLS of PLHIV",
                                                "onArtVLS" = "Percent VLS on ART",
                                                "knownstatusNum" = "Number Known Status of PLHIV",
                                                "onArtNum" = "Number on ART of PLHIV",
                                                "vlsNum" = "Number VLS of PLHIV",
                                                "pmtct" = "PMTCT", #what to call this
                                                "pmtctArt" = "Number PMTCT on ART",
                                                "pmtctArtPct" = "Percent PMTCT on ART")) %>%
        dplyr::rename(estimate = est,
                      lower_bound = low,
                      upper_bound = high)

      #add PEPFAR grouping category
      gdrive_df_clean <-  glamr::pepfar_country_list %>%
        dplyr::select(country, iso = country_iso) %>%
        dplyr::rename(countryname = country) %>%
        dplyr::left_join(gdrive_df_clean, ., by = "iso") %>%
        dplyr::mutate(country = ifelse(is.na(countryname), country, countryname),
                      pepfar = ifelse(is.na(countryname), FALSE, TRUE)) %>% #flag to denote pepfar countries
        dplyr::select(-countryname)

      #Export final df
      final_df <- suppressWarnings(
        gdrive_df_clean %>%
          dplyr::mutate(across(estimate:upper_bound, ~as.numeric(.x)),
                        indic_type =stringr::str_remove(indic_type, "_indics") %>% stringr::str_to_title(), #capitalizes "Integer" in indic_type
                        across(estimate:upper_bound, ~dplyr::case_when(indic_type == "Integer" ~ round(.x), #rounds Integer values
                                                                       indic_type == "Percent" ~ .x)),
                        across(age:sex, ~stringr::str_to_title(.x))) %>% #capitalizes "All" in age/sex
          dplyr::filter(indic_type == indicator_type)

      )

      #read national data from EDMS
      #filter for "Total Deaths" indicator
      df_nat <- read_sheet("1Brg_v0rXtDcvtdrUkmyjztu4Vwzx_yzUcw4EzzKSA98") %>%
        filter(indicator == "Total deaths to HIV Population")

      #rename columns & format to match clean data
      df_nat <- df_nat %>%
        rename(iso = iso3,
               #sheet = dataset,
               lower_bound = `lower bound`,
               upper_bound = `upper bound`) %>%
        mutate(year = as.character(year)) %>%
        mutate(across(age:sex, ~stringr::str_to_title(.x)))  #capitalizes "All" in age/sex

      #Add sheet and indicator type variable
      df_nat <- df_nat %>%
        mutate(sheet = "HIV Estimates",
               indic_type = "Integer") %>%
        #mutate(region = ifelse(country == "Global", "Global", region))%>% #fill in missing regions
        #mutate(region = ifelse(country == "Latin America", "Latin America", region))%>%
        #mutate(region = ifelse(country == "Caribbean", "Caribbean", region))%>%
        mutate(across(estimate:upper_bound, ~dplyr::case_when(indic_type == "Integer" ~ round(.x)))) %>% #round estimate values
        #distinct(country, iso, estimate)
        select(year, iso, country, region, indicator, age, sex, estimate,
               lower_bound, upper_bound,
               sheet, indic_type, pepfar)

      #Bind together
      final_df <- final_df %>%
        bind_rows(df_nat)

      return(final_df)

    }


  #Check pull_unaids function---------------------------------------------------------------------
    #Clean up parameters - remove `original_unaids` param to replace with more efficient workflow
      #data_type: "HIV Estimates" and "HIV Test & Treat"
      #pepfar_only: TRUE pulls from PEPFAR Only Clean Estimates, FALSE pulls from Clean Estimates

    pull_unaids <- function(data_type, pepfar_only = TRUE) {

      if(pepfar_only == TRUE) {
        google_id = pepfar_clean_id
      }

      else {
        google_id = gs_clean_id
      }

      df <- googlesheets4::range_speedread(google_id) %>%
        dplyr::filter(sheet == data_type)

      return(df)
    }

  #Check epi_plot function---------------------------------------------------------------------
    #Clean up parameters (plug in new total death indicator)
        #pull epi control data you need for all countries - store in df
        #create epi flag
        #create an ou list against which entries are checked
        #create a plotting function that will return epi curve

    epi_plot()
    # Helper function to load the `HIV Estimates` data
      #filter data down to 2 indicators needed - "Total deaths to HIV Population" & "Number New HIV Infections"
      #filter for all ages and all sexes
    create_epi_df <- function(){
      df_epi <- #mindthegap::
        pull_unaids(data_type = "HIV Estimates",pepfar_only = TRUE) %>% #pull from PEPFAR Only estimates
        dplyr::filter(age == "All", sex == "All",
          indicator %in% c("Total deaths to HIV Population", "Number New HIV Infections")) %>% #grab indicators
        dplyr::select(year, country,indicator, estimate) %>%
        dplyr::arrange(country, indicator, year) #order rows by these variables

      return(df_epi)
    }


    # Create the data frame you need
    get_epi_curve_df <- function(){

      # Pull in epi data
      df_epi <- create_epi_df()

      # Perform necessary munging
      df_epi_ous <-
        df_epi %>%
        #dplyr::mutate(indicator = stringr::word(indicator, -1) %>% tolower) %>% #filters indicator name to last word
        tidyr::pivot_wider(names_from = indicator, #pivots data wide into deaths and infections column
                           values_from = estimate,
                           names_glue = "{indicator %>% str_extract_all('deaths|Infections') %>% tolower}") #new death indicator

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

      return(df_epi_pepfar)

    }


    #OU list to check entries
    pull_ou_list <- function(df = df_epi){
      ou_list <- df %>% dplyr::distinct(country) %>% dplyr::pull()
      return(ou_list)
    }

    # Plotting function to make the epi curves
    # By default, it will produce the All PEPFAR curve
    epi_plot <- function(df = df_epi_pepfar, sel_cntry = c("All PEPFAR")){

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
                         ggplot2::ggplot(aes(year, val_mod, group = indicator, fill = fill_color, color = fill_color)) +
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
                                                     expand = c(0, 0)) +
                         ggplot2::scale_x_continuous(breaks = seq(min(df$year), max(df$year),5)) + #automatic x-axis min/max
                         #ggplot2::scale_x_continuous(breaks = seq(1990, 2025, 5)) + #manual x-axis breaks
                         ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
                         ggplot2::labs(x = NULL, y = NULL) + coord_cartesian(expand = T, clip = "off") +
                         glitr::si_style_ygrid(facet_space = 0.75) + #adjusted y-axis grid spacing with facet_space
                         ggplot2::theme(axis.text.y = ggtext::element_markdown()) +
                         ggplot2::labs(caption = "Source: UNAIDS Data 2022 Release"))

    }



# TEST IT ============================================================================

  #Test functions
    #read_rename
    gdrive_df <- read_rename("HIV Estimates")
    glimpse(gdrive_df)
    #View(gdrive_df)

    #validate_cols
    df <- validate_cols(gdrive_df, "HIV Estimates")
    glimpse(df)

    #munge_unaids
    final_df <- munge_unaids(return_type = "HIV Estimates", indicator_type = "Integer")
    glimpse(final_df)
    #View(final_df)

    #final_percent_df <- munge_unaids(return_type = "HIV Test & Treat", indicator_type = "Percent")
    #View(final_percent_df)

    #pull_unaids
    df_epi <- pull_unaids("HIV Estimates", pepfar_only = TRUE)
    #df_test <- pull_unaids("HIV Test & Treat", pepfar_only = TRUE)

    #epi_plot
      df_epi <- get_epi_curve_df()
      ou_list <- pull_ou_list()
      epi_plot() #default is "ALL PEPFAR"
      epi_plot(df_epi, sel_cntry = c("South Africa", "Zambia", "Kenya", "Malawi")) #specify countries
      epi_plot(df_epi, sel_cntry = "USA") #break with non-PEPFAR countries



# SPINDOWN ============================================================================
