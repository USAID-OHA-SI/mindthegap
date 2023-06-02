# PROJECT: agitprop
#PURPOSE: Editing epi_plot function
# AUTHOR: Lemlem Baraki & Karishma Srikanth| SI
# REF ID: 01869b8a
# LICENSE: MIT
# DATE: 2023-05-22
# NOTES: derived from agitprop/08_epi_ann_unaids-pepfar-epi-control.R & catch-22/ctip-ou-unaids_plus_epi.R

# LOCALS & SETUP  -----------------------------------------------------------

#Libraries
library(tidyverse)
library(glitr)
library(glamr)
library(ICPIutilities)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gisr)
library(googlesheets4)
library(readxl)
library(stringi)
# remotes::install_github("https://github.com/USAID-OHA-SI/mindthegap.git", ref = "unaids-data")
library(mindthegap) #remotes::install_github("USAID-OHA-SI/mindthegap")


#SI specific paths
load_secrets()

#REF ID for plots
ref_id <- "01869b8a"

#TO DO:
#review epi_plot() function - creates epidemic control curves for selected OU or "ALL PEPFAR"
#use ALL PEPFAR example
#merge 2 datasets --> total deaths + new infections
#use facets for multiple countries (11 countries) - ex) floats/free scales
#use sel_cntry parameter


# LOAD DATA ---------------------------------------------------------------

df_epi <- pull_unaids("HIV Estimates", TRUE)

#pull Total PLHIV death data
g_id <- "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"

df_deaths <- range_speedread(ss = g_id, sheet = "UNAIDS_epi_control") %>%
  filter(indicator == "Number Total Deaths HIV Pop")

#country variables
#sel_cntry <- c("Uganda", "Kenya", "Namibia", "Eswatini") #hardcoded focal countries example

epi_cntry <- df_epi_pepfar %>% #countries that have reached epidemic control
  filter(year == max(year),
         indicator == "infections",
         epi_control == TRUE) %>%
  arrange(desc(value)) %>%
  pull(country)

# MUNGE -------------------------------------------------------------------

#Change
#total_deaths: need to merge total deaths metric + with new infections metric in df_epi_pepfar df
#loop over function and create ggplots for each country listed

#grab new infections
df_epi_pepfar <- df_epi %>%
  filter(
    #stat == "est",
    age == "All",
    indicator %in% c("Number New HIV Infections")) %>%
  # semi_join(pepfar_country_list, by = c("iso" = "countryname_iso")) %>%
  select(year, country, indicator, estimate) %>%
  arrange(country, indicator, year)

view(df_epi_pepfar)

#grab total deaths
total_deaths <- df_deaths %>%
  #select(-c(iso2, geo_level)) %>%
  filter(age == "all",
         sex == "all") %>%
  select(c(country, year, indicator, estimate)) %>%
  spread(indicator, estimate) %>%
  janitor::clean_names() %>%
  rename(total_deaths = number_total_deaths_hiv_pop)

view(total_deaths)


#check merge compatible & names of countries in both datasets
#note some countries have subregions (Kenya, Ethiopia, Zimbabwe)
map(list(df_epi_pepfar$country, total_deaths$country), ~summary(.x))
setdiff(unique(df_epi_pepfar$country) %>%sort(), unique(total_deaths$country)) #2nd df missing 3 countries
setdiff(unique(total_deaths$country) %>%sort(), unique(df_epi_pepfar$country)) #2nd df missing 125 countries


#merge total deaths & pull out metrics
#merged_df <- left_join(df_epi_pepfar, total_deaths, by = c("country", "year"))

df_epi_pepfar <- df_epi_pepfar %>%
  mutate(indicator = word(indicator, -1) %>% tolower) %>%
  pivot_wider(names_from = "indicator", values_from = "estimate") %>%
  left_join(total_deaths, by = c("year", "country")) %>% #merge total deaths into df
  group_by(country) %>%
  mutate(declining_deaths = total_deaths - lag(total_deaths, order_by = year) <= 0) %>% #TRUE/FALSE declining deaths variable
  ungroup() %>%
  mutate(infections_below_deaths = infections < total_deaths,
         ratio = infections / total_deaths,
         direction_streak = sequence(rle(declining_deaths)$lengths),
         epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) #TRUE/FALSE epi control variable
view(df_epi_pepfar)

#Add color to indicators
df_epi_pepfar <- df_epi_pepfar %>%
  pivot_longer(c(infections, total_deaths), names_to = "indicator") %>% #put back indicators in column
  arrange(country, indicator, year) %>%
  mutate(value_mod = ifelse(indicator == "total_deaths", -value, value),
         fill_color = ifelse(indicator == "total_deaths", old_rose, denim))#red for total deaths, blue for infections

#Epi control curve 1 df- of ALL PEPFAR
df_viz_pepfar <- df_epi_pepfar %>%
  mutate(country = "All PEPFAR") %>% #trends across ALL PEPFAR countries
  group_by(country, year, indicator, fill_color) %>%
  #summarise(across(c(value, value_mod), sum, na.rm = TRUE), .groups = "drop") %>% #deprecated
  #before: across(a:b, mean, na.rm = TRUE) --> now: across(a:b, \(x) mean(x, na.rm = TRUE))
  summarise(across(c(value, value_mod),\(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
  mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")), #adding value label for most recent year (ex. 605k)
         max_plot_pt = max(value), #max threshold
         lab_pt = case_when(year == max(year) ~ value_mod)) #adding a point at the most recent year's value

#Epi control curve 2 df - selected countries
  #grab country names from df_epi_pepfar
sel_cntry <- unique(df_epi_pepfar$country) #creates a unique vector/df for countries

df_viz_cntry <- df_epi_pepfar %>%
  filter(country %in% sel_cntry) %>%
  mutate(val_lab = case_when(year == max(year) ~ number(value, 1, scale = 1e-3, suffix = "k")),
         max_plot_pt = max(value),
         lab_pt = case_when(year == max(year) ~ value_mod),
          country = factor(country, sel_cntry)) #encodes vector as factor


# VIZ ---------------------------------------------------------------------


#Curve 1
v_p <- df_viz_pepfar %>%
  ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
  geom_blank(aes(y = max_plot_pt)) +
  geom_blank(aes(y = -max_plot_pt)) +
  geom_area(alpha = .25) +
  geom_hline(yintercept = 0, color = grey80k) +
  geom_line() +
  geom_point(aes(y = lab_pt), na.rm = TRUE,
             shape = 21, color = "white", size = 3) +
  geom_text(aes(label = val_lab), na.rm = TRUE,
            hjust = -0.3,
            family = "Source Sans Pro Light") +
  facet_wrap(~country) +
  #scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
  scale_y_continuous(labels = ~ (scales::label_number(scales_cut = cut_short_scale())(abs(.))))+
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = T, clip = "off") +
  si_style_ygrid() +
  theme(axis.text.y = element_markdown())

#Curve 2
v_c <- df_viz_cntry %>%
  ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
  geom_blank(aes(y = max_plot_pt)) +
  geom_blank(aes(y = -max_plot_pt)) +
  geom_area(alpha = .25) +
  geom_hline(yintercept = 0, color = grey80k) +
  geom_line() +
  geom_point(aes(y = lab_pt), na.rm = TRUE,
             shape = 21, color = "white", size = 3) +
  geom_text(aes(label = val_lab), na.rm = TRUE,
            hjust = -0.3,
            family = "Source Sans Pro Light") +
  facet_wrap(~country, nrow = 4, scales = "free_y") +
  #scale_y_continuous(labels = ~ label_number_si()(abs(.))) +
  scale_y_continuous(labels = ~ (scales::label_number(scales_cut = cut_short_scale())(abs(.))))+
  scale_x_continuous(breaks = seq(1990, 2025, 10)) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = T, clip = "off") +
  si_style_ygrid() +
  theme(axis.text.y = element_markdown(),
        panel.spacing.x = unit(20, "pt"),
        panel.spacing.y = unit(0, "pt"))

#Combined curves
v_p + v_c +
  plot_annotation(title = plot_title,
                  subtitle = epi_control,
                  caption = glue("Source: {source} [{date_pulled}]
                        SI analytics: {paste(authors, collapse = '/')}
                     US Agency for International Development"),
                  theme = si_style_ygrid()) &
  theme(axis.text.y = element_markdown(),
        panel.spacing.x = unit(20, "pt"),
        panel.spacing.y = unit(0, "pt"),
        plot.title = element_markdown())


#Generate visualizations for selected countries
map(.x = df_epi_pepfar$country, .f = epi_plot) #runs through each country on the list

map(.x = c("Kenya", "Ethiopia", "South Sudan"), #input specific names for the vector
        .f = ~ epi_plot(sel_cntry = .x)) #call the function with parameter set to country names - only produces 1st one

map_chr(sel_cntry, .f = ~ epi_plot(df_epi_pepfar, country = .x))


# FUNCTION ----------------------------------------------------------------
#Changes
  #Remove original unaids parameter
  #adjust function to pass through multiple countries in else condition
  #facet_wrap: to show small multiples of countries - adjust scales


epi_plot <- function (sel_cntry)
{

    df_epi <- mindthegap::pull_unaids( #pull_unaids grabs from July 2022 UNAIDS data by default
      data_type = "HIV Estimates", #change sheetname to data_type - 1 of 2 options
        pepfar_only = TRUE) #filters to only PEPFAR countries if TRUE
    df_epi_pepfar <- df_epi %>% dplyr::filter(#stat == "est",
        age == "All",
        indicator %in% c("Number New HIV Infections")) %>% #grab new infections indicator
      dplyr::select(year, country,indicator, estimate) %>% #change indicator value to estimate
      dplyr::arrange(country, indicator, year)
    total_deaths <- df_deaths %>% filter(age == "all", #grab total deaths indicator
                                         sex == "all") %>%
      select(c(country, year, indicator, estimate)) %>%
      spread(indicator, estimate) %>%
      janitor::clean_names() %>%
      rename(total_deaths = number_total_deaths_hiv_pop)
    df_epi_pepfar <- df_epi_pepfar %>% dplyr::mutate(indicator = stringr::word(indicator,-1) %>% tolower) %>%  #merge in total deaths
      tidyr::pivot_wider(names_from = "indicator", values_from = "estimate") %>%
      dplyr::left_join(total_deaths, by = c("year", "country"))%>% #merge in total deaths
      dplyr::group_by(country) %>% dplyr::mutate(declining_deaths = total_deaths - #declining_deaths metric
        dplyr::lag(total_deaths, order_by = year) <= 0) %>% dplyr::ungroup() %>%
        dplyr::mutate(infections_below_deaths = infections < total_deaths, #infections_below_deaths metric
                      ratio = infections/total_deaths,
                      direction_streak = sequence(rle(declining_deaths)$lengths),
            epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) #epi_control metric
    df_epi_pepfar <- df_epi_pepfar %>% tidyr::pivot_longer(c(infections,total_deaths), names_to = "indicator")%>%
      dplyr::arrange(country,indicator, year) %>%
      dplyr::mutate(value_mod = ifelse(indicator =="total_deaths", -value, value),
                    fill_color = ifelse(indicator == "total_deaths", glitr::old_rose, glitr::denim)) #red for total_deaths, blue for new infections
    if (any(sel_cntry == "ALL PEPFAR")) { #use if any condition for length > 1
        df_viz_pepfar <- df_epi_pepfar %>% dplyr::mutate(country = "All PEPFAR") %>% #epi_control curve 1: ALL PEPFAR
            dplyr::group_by(country, year, indicator, fill_color) %>%
            #dplyr::summarise(dplyr::across(c(value, value_mod),sum, na.rm = TRUE), .groups = "drop") %>% #deprecated
          dplyr::summarise(across(c(value, value_mod),\(x) sum(x,na.rm = TRUE)), .groups = "drop") %>%
          dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~ scales::number(value, 1, scale = 0.001,
            suffix = "k")), max_plot_pt = max(value), min_plot_pt = min(value_mod),
            lab_pt = dplyr::case_when(year == max(year) ~ value_mod))
        #indicator = ifelse(indicator == "total_deaths", #"AIDS Related Deaths", #double check if labels needed
        #   "Number New HIV Infections"), new_hiv_label = dplyr::case_when(value ==
        #  max_plot_pt ~ indicator), aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))
        viz <- df_viz_pepfar %>%  #epi_control plot
          ggplot(aes(year, value_mod,group = indicator, fill = fill_color, color = fill_color)) +
          geom_blank(aes(y = max_plot_pt)) + #sets max y-axis above
          geom_blank(aes(y = -max_plot_pt)) + #sets max y-axis below
          geom_area(alpha = 0.25) +
          geom_hline(yintercept = 0,color = glitr::grey80k) +
          geom_line() +
          geom_point(aes(y = lab_pt), na.rm = TRUE, shape = 21, color = "white", size = 3) +
            geom_text(aes(label = val_lab), na.rm = TRUE, #value label text
                      hjust = -0.3,
                      family = "Source Sans Pro Light") +
           #geom_text(aes(label = new_hiv_label, x = 2010, y = (max_plot_pt + 1000)), na.rm = TRUE,
            #         hjust = -0.3,
             #        family = "Source Sans Pro Light") +
            #geom_text(aes(label = aids_label, x = 2010, y = (min_plot_pt + 1000)), na.rm = TRUE,
             #         hjust = -0.3,
              #        family = "Source Sans Pro Light") +
            facet_wrap(~country) + #small multiples of countries
          #scale_y_continuous(labels = ~(scales::label_number_si())(abs(.))) + #deprecated - use 'scale_cut'
          scale_y_continuous(labels = ~ (scales::label_number(scales_cut = cut_short_scale())(abs(.))))+
            scale_x_continuous(breaks = seq(1990, 2025, 5)) +
            scale_fill_identity(aesthetics = c("fill", "color")) +
            labs(x = NULL, y = NULL) + coord_cartesian(expand = T, clip = "off") +
          glitr::si_style_ygrid() +
          theme(axis.text.y = ggtext::element_markdown())
        suppressWarnings(print(viz))
    }
       else {
         #map(sel_cntry, ~ {
        df_viz_cntry <- df_epi_pepfar %>% #epi_control curve 2: country specific
          dplyr::filter(country %in% sel_cntry) %>% #change to listed countries
          dplyr::mutate(val_lab = dplyr::case_when(year == max(year) ~
                                                     scales::number(value, 1, scale = 0.001, suffix = "k")),
            max_plot_pt = max(value), min_plot_pt = min(value_mod),
            lab_pt = dplyr::case_when(year == max(year) ~ value_mod),
            country = factor(country, sel_cntry))
        #indicator = ifelse(indicator == "total_deaths", #"AIDS Related Deaths",
        #                  "New HIV Infections"), new_hiv_label = dplyr::case_when(value ==
        # max_plot_pt ~ indicator), aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))
        viz <- df_viz_cntry %>% #epi_control plot
          ggplot(aes(year, value_mod, group = indicator, fill = fill_color, color = fill_color)) +
          geom_blank(aes(y = max_plot_pt)) + #sets max y-axis above
          geom_blank(aes(y = -max_plot_pt)) + #sets max y-axis below
          geom_area(alpha = 0.25) +
            geom_hline(yintercept = 0, color = glitr::grey80k) +
            geom_line() + geom_point(aes(y = lab_pt), na.rm = TRUE,
            shape = 21, color = "white", size = 3) +
          geom_text(aes(label = val_lab), na.rm = TRUE,
                    hjust = -0.3,
                    family = "Source Sans Pro Light") +
            #geom_text(aes(label = new_hiv_label, x = 2010, y = (max_plot_pt +1000)), na.rm = TRUE,
             #         hjust = -0.3,
              #        family = "Source Sans Pro Light") +
            #geom_text(aes(label = aids_label, x = 2010, y = (min_plot_pt + 1000)), na.rm = TRUE,
             #         hjust = -0.3,
              #        family = "Source Sans Pro Light") +
            facet_wrap(~country, nrow = 4, scales = "free_y") + #small multiples of countries - use scales "free" on y-axis
          #scale_y_continuous(labels = ~(scales::label_number_si())(abs(.))) + #deprecated
          scale_y_continuous(labels = ~ (scales::label_number(scales_cut = cut_short_scale())(abs(.))))+
            scale_x_continuous(breaks = seq(1990, 2025, 10)) +
            scale_fill_identity(aesthetics = c("fill", "color")) +
            labs(x = NULL, y = NULL) + coord_cartesian(expand = T, clip = "off") +
          glitr::si_style_ygrid() +
          theme(axis.text.y = ggtext::element_markdown())
        suppressWarnings(print(viz))
         #})
    }
}


else if (length(sel_cntry) > 1){ #generate plots for multiple selected countries
  map(sel_cntry, ~ epi_plot(.x)) #call the function for each country
}

#indicator = ifelse(indicator == "total_deaths", #"AIDS Related Deaths", #double check if labels needed
#   "Number New HIV Infections"), new_hiv_label = dplyr::case_when(value ==
#  max_plot_pt ~ indicator), aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))


#indicator = ifelse(indicator == "total_deaths", #"AIDS Related Deaths",
#                  "New HIV Infections"), new_hiv_label = dplyr::case_when(value ==
# max_plot_pt ~ indicator), aids_label = dplyr::case_when(value_mod == min_plot_pt ~ indicator))


