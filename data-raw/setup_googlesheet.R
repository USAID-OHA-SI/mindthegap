#authorize googlesheets
  googledrive::drive_auth()
  googlesheets4::gs4_auth()

#create spreadsheet
  gs <- googlesheets4::gs4_create(glue::glue("MindTheGap_data"), sheets = c("ARTshare", "UNAIDS"))

#change permissions to allow USAID users to access
  googledrive::drive_share(googledrive::as_id(gs), role = "reader",
                           type = "domain", domain = "usaid.gov", verbose = FALSE)

#check file permissions
  googledrive::drive_reveal(googledrive::as_id(gs), "permissions")

#store file id for use in accessing
  txt <- glue::glue("#' IMPATT Google Sheet ID
                        #'
                        #' @export
                        #' @return google sheet id
                        #'
                        gs_id <- '{gs}'")
  writeLines(txt, "R/gs_id.R")
  usethis::ui_warn("Updating package documentation and rebuilding")
  devtools::document(".", quiet = TRUE)
  devtools::build(".", quiet = TRUE)
