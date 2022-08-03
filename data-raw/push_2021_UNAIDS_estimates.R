# Push clean UNAIDS 2021 estimates to gdrive and flag epi control

#date: 12/23/2021

#load packages
library(mindthegap)
library(tidyverse)
library(purrr)

#authorize googlesheets
googledrive::drive_auth()
googlesheets4::gs4_auth()
library(googledrive)
library(googlesheets4)

# EPI CONTROL FLAGS --------------------------------------

# Flagging epi control countries and 90's progress



#read clean data from munge_unaids
df_est <- munge_unaids("HIV Estimates", "Integer")
df_tt <- munge_unaids("HIV Test & Treat", "Percent")

# EPI CONTROL FLAG: INFECTIONS + DEATHS

df_est_lim <- df_est %>%
  filter(indicator %in% c("PLHIV", "AIDS Related Deaths", "New HIV Infections"),
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

#reshape wide to align with T&T
df_est_lim <- df_est_lim %>%
  pivot_wider(names_from = indicator,
              values_from = estimate,
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
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

plhiv_base_90 <- df_tt_lim %>%
  filter(!is.na(estimate)) %>%
  mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                            "PLHIV_ON_ART" = "On\nART"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         goal_rate = round((goal/100)^set*100),
         achv = estimate >= goal_rate) %>%
  count(country,iso, achv) %>%
  mutate(plhiv_base_90s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  distinct(country, iso, plhiv_base_90s)

#90'S PROGRESS - RELATIVE CASCADE BASE

#Relative Base
df_tt_rel_lim <- df_tt %>%
  filter(year == max(year),
         indicator %in% c("KNOWN_STATUS", "KNOWN_STATUS_ON_ART", "ON_ART_VLS"),
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

rel_base_90 <- df_tt_rel_lim %>%
  filter(!is.na(estimate)) %>%
  mutate(indicator = recode(indicator, "KNOWN_STATUS" = "Known\nStatus",
                            "KNOWN_STATUS_ON_ART" = "On\nART",
                            "ON_ART_VLS" = "VLS"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         # goal_rate = round((goal/100)^set*100),
         achv = estimate >= goal) %>%
  group_by(country) %>%
  count(country, iso, achv) %>%
  mutate(rel_base_90s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  # select(-c(achv, n)) %>%
  distinct(country, iso, rel_base_90s)

#COMBINE 3 FLAGS INTO ONE DF AND LIST

flag_list <- full_join(plhiv_base_90, rel_base_90, by = c("country", "iso"))
flag_list <- full_join(flag_list, df_epi_cntry, by = c("country", "iso")) %>% list()

#PUSH TO DRIVE -----------------------------------------------------------------------

data_type <- c("HIV Estimates", "HIV Estimates", "HIV Test & Treat", "HIV Test & Treat")
ind_type <- c("Integer", "Percent", "Integer", "Percent")

#make a list of the 4 dataframes
lst <- purrr::map2( .x = data_type,
                    .y = ind_type,
                    .f = ~munge_unaids(.x, .y)
)

unaids_list <- map(1:4, ~map2(lst[.x], flag_list, ~left_join(.x, .y, by = c("country", "iso"))))

unaids_list <- flatten(unaids_list)

#FULL DATASET -----------------------------

#add file to drive
gs_id_new <- drive_create(name = "UNAIDS 2021 Clean Estimates", path = "SI Folder/Analysis, Data & Tools/UNAIDS", type = "spreadsheet")

#specify sheet names
tab_names <- c("HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent")

#sheet names
purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_new), sheet = .x))

#read the data in!
purrr::walk2(.x = unaids_list, .y = tab_names, .f = ~sheet_write(data = .x, ss = gs_id_new, sheet = .y))

#JUST PEPFAR DATASET -----------------------------

#make a separate list filtering to just PEPFAR
list_pepfar <- lapply(unaids_list, function(x) filter(x, pepfar == TRUE))

#append 4 dfs to one df
df_pepfar <- do.call("rbind", list_pepfar)

#add file to drive
gs_id_pepfar <- drive_create(name = "PEPFAR Only - UNAIDS 2022 Clean Estimates", path = "SI Folder/Analysis, Data & Tools/UNAIDS", type = "spreadsheet")

#specify sheet names
tab_names <- c("2022 UNAIDS Estimates")

#sheet names
purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_pepfar), sheet = .x))

#read the data in!
sheet_write(data = df_pepfar, ss = gs_id_pepfar, sheet = tab_names)


    # #this is to read in the data with multiple tabs
    # tab_names <- c("HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent")
    # purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_pepfar), sheet = .x))
    # purrr::walk2(.x = df_pepfar, .y = tab_names, .f = ~sheet_write(data = .x, ss = gs_id_pepfar, sheet = .y))


