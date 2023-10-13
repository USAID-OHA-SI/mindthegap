# AUTHOR:   K. Srikanth | USAID
# PURPOSE:  Store UNAIDS Data in GH Releases
# REF ID:   b35bcd5e
# LICENSE:  MIT
# DATE:     2022-10-11
# UPDATED:  2023-10-13

# DEPENDENCIES ------------------------------------------------------------

  library(glamr)
  library(tidyverse)
  library(googlesheets4)
  library(piggyback)
  load_all(.)

# FIRST UNAIDS RELEASE - 07-22-22 -----------------------------------------

#add version tag for date
new_tag <- "v2023.08.11"

#create a new release (default will go to the USAID-OHA-SI/mindthegap repo)
pb_new_release(tag = new_tag)

#set up a temp folder
glamr::temp_folder()

#import data
df_all <- googlesheets4::range_speedread(gs_clean_id_2022)
df_pepfar <- googlesheets4::range_speedread(pepfar_clean_id_2022)

#save data to temp folder
readr::write_csv(df_all, file.path(folderpath_tmp, "UNAIDS_2023_Clean_Estimates.csv.gz"))
saveRDS(df_all, file.path(folderpath_tmp, "UNAIDS_2023_Clean_Estimates.rds"))

readr::write_csv(df_pepfar, file.path(folderpath_tmp, "UNAIDS_2023_Clean_Estimates_PEPFAR-only.csv.gz"))
saveRDS(df_pepfar, file.path(folderpath_tmp, "UNAIDS_2023_Clean_Estimates_PEPFAR-only.rds"))

  #upload multiple files to release for archiving
  list.files(folderpath_tmp, full.names = TRUE) %>%
    pb_upload(tag = new_tag)

  #upload multiple files to latest release for ease of access
  list.files(folderpath_tmp, full.names = TRUE) %>%
    pb_upload(tag = "latest")

  #download a specific file - test
  pb_download(file = "UNAIDS.2022.Clean.Estimates.rds",
              repo = "USAID-OHA-SI/mindthegap",
              dest = glamr::si_path("path_downloads"))


  # ADDITIONAL UNAIDS INDICATORS - 08-10-2022 ------------------------------

  new_tag <- "v2022.08.10"
  pb_new_release(tag = new_tag)
  glamr::temp_folder()

  unaids_additional <- "1CSVOauu2gyq9Am0eCl7TgpAeB1Xd3dCtE_Oc_yk3cI4"

  #import all three tabs as individual dfs
  df_additional_epi <- googlesheets4::range_speedread(unaids_additional, sheet = 1)
  df_additional_cascade <- googlesheets4::range_speedread(unaids_additional, sheet = 2)
  df_additional_prev <- googlesheets4::range_speedread(unaids_additional, sheet = 3)

  #save to temp folder
  readr::write_csv(df_additional_epi, file.path(folderpath_tmp, "UNAIDS_epicontrol_2022.csv.gz"))
  saveRDS(df_additional_epi, file.path(folderpath_tmp, "UNAIDS_epicontrol_2022.rds"))

  readr::write_csv(df_additional_prev, file.path(folderpath_tmp, "UNAIDS_prev_2022.csv.gz"))
  saveRDS(df_additional_prev, file.path(folderpath_tmp, "UNAIDS_prev_2022.rds"))

  readr::write_csv(df_additional_cascade, file.path(folderpath_tmp, "UNAIDS_cascade_2022.csv.gz"))
  saveRDS(df_additional_cascade, file.path(folderpath_tmp, "UNAIDS_cascade_2022.rds"))

  #upload multiple files
  list.files(folderpath_tmp, full.names = TRUE) %>%
    pb_upload(tag = new_tag)

