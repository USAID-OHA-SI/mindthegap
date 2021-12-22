# Flagging epi control countries and 90's progress

#date: 12/22/2021

#load packages
library(mindthegap)
library(tidyverse)
library(purrr)

#read clean data from munge_unaids
df_est <- munge_unaids("HIV Estimates", "Integer")
df_tt <- munge_unaids("HIV Test & Treat", "Percent")

# EPI CONTROL FLAG: INFECTIONS + DEATHS

df_est_lim <- df_est %>%
  filter(indicator %in% c("PLHIV", "AIDS Related Deaths", "New HIV Infections"),
         age == "all",
         sex == "all",
         stat == "est") %>%
  select(year, country, iso, indicator, value)

#reshape wide to align with T&T
df_est_lim <- df_est_lim %>%
  pivot_wider(names_from = indicator,
              names_glue = "{indicator %>% str_extract_all('Deaths|Infections|PLHIV') %>% tolower}")

#plhiv for reference
df_plhiv <- df_est_lim %>%
  filter(year == max(year)) %>%
  select(year, country, iso, plhiv)

#identify if epi control or not
df_epi_cntry <- df_est_lim %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>%
  ungroup() %>%
  mutate(infections_below_deaths = infections < deaths,
         ratio = infections / deaths,
         direction_streak = sequence(rle(declining_deaths)$lengths),
         epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>%
  filter(year == max(year),
         !is.na(ratio)) %>%
  select(country, iso, epi_control)

#90'S PROGRESS - PLHIV BASE

#UNAID GOAL - 90 or 95
goal <- 90

df_tt_lim <- df_tt %>%
  filter(year == max(year),
         indicator %in% c("KNOWN_STATUS", "PLHIV_ON_ART", "VLS"),
         age == "all",
         sex == "all",
         stat == "est") %>%
  select(year, country, iso, indicator, value)

plhiv_base_90 <- df_tt_lim %>%
  filter(!is.na(value)) %>%
  mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                            "PLHIV_ON_ART" = "On\nART"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         goal_rate = round((goal/100)^set*100),
         achv = value >= goal_rate) %>%
  count(country,iso, achv) %>%
  mutate(plhiv_base_90s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  distinct(country, iso, plhiv_base_90s)

#90'S PROGRESS - RELATIVE CASCADE BASE

#Relative Base
df_tt_rel_lim <- df_tt %>%
  filter(year == max(year),
         indicator %in% c("KNOWN_STATUS", "KNOWN_STATUS_ON_ART", "ON_ART_VLS"),
         age == "all",
         sex == "all",
         stat == "est") %>%
  select(year, country, iso, indicator, value)

rel_base_90 <- df_tt_rel_lim %>%
  filter(!is.na(value)) %>%
  mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                            "KNOWN_STATUS_ON_ART" = "On\nART",
                            "ON_ART_VLS" = "VLS"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         # goal_rate = round((goal/100)^set*100),
         achv = value >= goal) %>%
  group_by(country) %>%
  count(country, iso, achv) %>%
  mutate(rel_base_90s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  # select(-c(achv, n)) %>%
  distinct(country, iso, rel_base_90s)

#COMBINE 3 FLAGS INTO ONE DF AND LIST

flag_list <- full_join(plhiv_base_90, rel_base_90, by = c("country", "iso"))
flag_list <- full_join(flag_list, df_epi_cntry, by = c("country", "iso")) %>% list()

#save as data object
usethis::use_data(flag_list, overwrite = TRUE)

