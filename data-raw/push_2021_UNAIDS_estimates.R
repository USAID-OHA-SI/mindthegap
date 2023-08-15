# Push clean UNAIDS estimates to gdrive and flag epi control

#date:  12/23/2021
#update: 07/24/2023 (2023 Estimates)

#load packages
library(mindthegap)
library(tidyverse)
library(purrr)
library(glamr)

#authorize googlesheets
googledrive::drive_auth()
googlesheets4::gs4_auth()
glamr::load_secrets()
library(googledrive)
library(googlesheets4)

# EPI CONTROL FLAGS --------------------------------------

#read clean data from munge_unaids
df_est <- munge_unaids(return_type = "HIV Estimates", indicator_type = "Integer")
df_tt <- munge_unaids(return_type = "HIV Test & Treat", indicator_type = "Percent")

#read national data from EDMS
df_nat <- googlesheets4::range_speedread("1Brg_v0rXtDcvtdrUkmyjztu4Vwzx_yzUcw4EzzKSA98")

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

#Compare
check <- pepfar_country_list

# setequal(df_nat$country, df_est$country)
# intersect(df_nat$country, df_est$country) #check overlap
# setdiff(df_nat$country, df_est$country)#check difference
# #"Sub-Saharan Africa", "Latin America and the Caribbean", "Africa","PEPFAR Country Programs", "PEPFAR Regional Programs" only in nat data
# setdiff(df_est$country, df_nat$country)
# setdiff(check$country, df_nat$country)
# #Of PEPFAR countries, nat data is missing "India", Nigeria", "Kazakhstan", "Ukraine"
# setequal(df_nat$region, df_est$region)
# intersect(df_nat$region, df_est$region)
# setdiff(df_nat$region, df_est$region)
# setdiff(df_est$region, df_nat$region)

#fix ISO code misalignment for Global and regions
iso_nat <- df_nat %>% distinct(iso) %>% pull()
iso_est <- df_est %>% distinct(iso) %>% pull()
change_isos <- setdiff(iso_nat, iso_est)

#pull out the Global and regional isos
reg_isos <- df_nat %>%
  filter(iso %in% change_isos) %>%
  count(country, iso) %>%
  select(-n) %>%
  rename(iso_new = iso)

#change them in df_est
df_est <- df_est %>%
  left_join(reg_isos, by = c("country")) %>%
  mutate(iso = ifelse(!is.na(iso_new), iso_new, iso)) %>%
  select(-iso_new)


#Bind together
df_est_join <- df_est %>%
bind_rows(df_nat)

# EPI CONTROL FLAG: INFECTIONS + DEATHS ----------------------------------------

df_est_lim <- df_est_join %>%
  filter(indicator %in% c("Number PLHIV",
                          "Total deaths to HIV Population", #substitute "Number AIDS Related Deaths" with "Total Deaths"
                          "Number New HIV Infections"),
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

#reshape wide to align with T&T
df_est_lim <- df_est_lim %>%
  pivot_wider(names_from = indicator,
              values_from = estimate,
              names_glue = "{indicator %>% str_extract_all('Total deaths to HIV Population|Infections|PLHIV') %>% tolower}")

  #plhiv for reference
df_plhiv <- df_est_lim %>%
  filter(year == max(year)) %>%
  select(year, country, iso, plhiv)

#identify if epi control or not
df_epi_cntry <- df_est_lim %>%
  arrange(country, year) %>%
  group_by(country) %>%
  rename(deaths = `total deaths to hiv population`) %>%
  mutate(declining_deaths = deaths - lag(deaths, order_by = year) <= 0) %>%
  ungroup() %>%
  mutate(infections_below_deaths = infections < deaths,
         ratio = infections / deaths,
         direction_streak = sequence(rle(declining_deaths)$lengths),
         epi_control = declining_deaths == TRUE & infections_below_deaths == TRUE) %>%
  filter(year == max(year),
         !is.na(ratio)) %>%
  select(country, iso, epi_control)

#95'S PROGRESS - PLHIV BASE ---------------------------------------------

#UNAID GOAL - 95
goal <- 95

df_tt_lim <- df_tt %>%
  filter(year == max(year),
         indicator %in% c("Percent Known Status of PLHIV", "Percent on ART of PLHIV", "Percent VLS of PLHIV"),
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

plhiv_base_95 <- df_tt_lim %>%
  filter(!is.na(estimate)) %>%
  mutate(indicator = recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                            "Percent on ART of PLHIV" = "On\nART",
                            "Percent VLS of PLHIV" = "VLS"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         goal_rate = round((goal/100)^set*100),
         achv = estimate >= goal_rate) %>%
  count(country,iso, achv) %>%
  mutate(plhiv_base_95s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  distinct(country, iso, plhiv_base_95s) %>%
  rename(`Achieved 95s with PLHIV base in 2022` = plhiv_base_95s)

#95'S PROGRESS - RELATIVE CASCADE BASE -----------------------------------------

#Relative Base
df_tt_rel_lim <- df_tt %>%
  filter(year == max(year),
         indicator %in% c("Percent Known Status of PLHIV", "Percent on ART with Known Status", "Percent VLS on ART"),
         age == "All",
         sex == "All") %>%
  select(year, country, iso, indicator, estimate)

rel_base_95 <- df_tt_rel_lim %>%
  filter(!is.na(estimate)) %>%
  mutate(indicator = recode(indicator, "Percent Known Status of PLHIV" = "Known\nStatus",
                            "Percent on ART with Known Status" = "On\nART",
                            "Percent VLS on ART" = "VLS"),
         set = recode(indicator, "Known\nStatus" = 1,
                      "On\nART" = 2,
                      "VLS" = 3),
         # goal_rate = round((goal/100)^set*100),
         achv = estimate >= goal) %>%
  group_by(country) %>%
  count(country, iso, achv) %>%
  mutate(rel_base_95s = ifelse(achv == TRUE & n == 3, TRUE, FALSE)) %>%
  # select(-c(achv, n)) %>%
  distinct(country, iso, rel_base_95s) %>%
rename(`Achieved 95s with relative base in 2022` = rel_base_95s)

#COMBINE 3 FLAGS INTO ONE DF AND LIST

flag_list <- full_join(plhiv_base_95, rel_base_95, by = c("country", "iso"))
flag_list <- full_join(flag_list, df_epi_cntry, by = c("country", "iso")) %>% list()

#ADD CUSTOM PULL INTO DF


#PUSH TO DRIVE -----------------------------------------------------------------------

data_type <- c("HIV Estimates", "HIV Estimates", "HIV Test & Treat", "HIV Test & Treat")
ind_type <- c("Integer", "Percent", "Integer", "Percent")

#make a list of the 4 dataframes
lst <- purrr::map2( .x = data_type,
                    .y = ind_type,
                    .f = ~munge_unaids(.x, .y)
)

#add df_nat to df_est, integers
lst[[1]] <- lst[[1]] %>%
  left_join(reg_isos, by = c("country")) %>%
  mutate(iso = ifelse(!is.na(iso_new), iso_new, iso)) %>%
  select(-iso_new) %>%
  bind_rows(df_nat)

unaids_list <- map(1:4, ~map2(lst[.x], flag_list, ~left_join(.x, .y, by = c("country", "iso"))))

unaids_list <- flatten(unaids_list)

#FULL DATASET -----------------------------

#add file to drive
gs_id_new <- drive_create(name = "UNAIDS 2023 Clean Estimates - FINAL", path = "SI Folder/Analysis, Data & Tools/UNAIDS", type = "spreadsheet")

#specify sheet names
#tab_names <- c("HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent")
tab_names <- c("2023 UNAIDS Estimates")

#append 4 dfs to one df
df_unaids <- do.call("rbind", unaids_list) #executes rbind on list of 4 dataframes

sheet_write(data = df_unaids, ss = gs_id_new, sheet = tab_names) #writes df into worksheet inside sheet


# #sheet names
# purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_new), sheet = .x))
#
# #read the data in!
# purrr::walk2(.x = unaids_list, .y = tab_names, .f = ~sheet_write(data = .x, ss = gs_id_new, sheet = .y))

#JUST PEPFAR DATASET -----------------------------

#make a separate list filtering to just PEPFAR
list_pepfar <- lapply(unaids_list, function(x) filter(x, pepfar == TRUE))

#append 4 dfs to one df
df_pepfar <- do.call("rbind", list_pepfar)

#add file to drive
gs_id_pepfar <- drive_create(name = "PEPFAR Only - UNAIDS 2023 Clean Estimates", path = "SI Folder/Analysis, Data & Tools/UNAIDS", type = "spreadsheet")

#specify sheet names
tab_names <- c("2023 UNAIDS Estimates")

#sheet names
purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_pepfar), sheet = .x))

#read the data in!
sheet_write(data = df_pepfar, ss = gs_id_pepfar, sheet = tab_names)


    # #this is to read in the data with multiple tabs
    # tab_names <- c("HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Treat - Percent")
    # purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_pepfar), sheet = .x))
    # purrr::walk2(.x = df_pepfar, .y = tab_names, .f = ~sheet_write(data = .x, ss = gs_id_pepfar, sheet = .y))


