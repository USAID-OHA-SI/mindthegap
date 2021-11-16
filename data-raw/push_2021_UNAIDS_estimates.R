# Push clean UNAIDS 2021 estimates to gdrive

#authorize googlesheets
googledrive::drive_auth()
googlesheets4::gs4_auth()

#load packages
library(mindthegap)
library(tidyverse)

data_type <- c("HIV Estimates", "HIV Estimates", "Test & Treat", "Test & Treat")
ind_type <- c("Integer", "Percent", "Integer", "Percent")

lst <- purrr::map2( .x = data_type,
                    .y = ind_type,
                    .f = ~munge_unaids(.x, .y)
)

#add file to drive
gs_id_new <- drive_create(name = "UNAIDS 2021 Clean Estimates", path = "SI Folder/Analysis, Data & Tools/UNAIDS", type = "spreadsheet")


#specify sheet names
tab_names <- c("HIV Estimates - Integer", "HIV Estimates - Percent", "Test & Treat - Integer", "Test & Test - Percent")


#sheet names
purrr::walk(.x = tab_names, .f = ~sheet_add(as_sheets_id(gs_id_new), sheet = .x))

#read the data in!
purrr::walk2(.x = lst, .y = tab_names, .f = ~sheet_write(data = .x, ss = gs_id_new, sheet = .y))




