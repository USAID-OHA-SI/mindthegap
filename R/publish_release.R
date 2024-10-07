#' Publish Tidy UNAIDS Data as a GitHub Release
#'
#' This is a developer's tool to take the output from EDMS that has been
#' tidied via `munge_unaids` and push it to GitHub as a
#' [Release](https://github.com/USAID-OHA-SI/mindthegap/releases). This is
#' performed once annually when the data are updated and then users will
#' access via `load_unaids`. This function posts versions for all countries
#' as well as PEPFAR only countries in csv and rds formats.
#'
#' @param df dataframe created by `munge_unaids`
#' @export
#'
#' @seealso [munge_unaids()] and [load_unaids()]
#'
#' @examples
#'  \dontrun{
#'   filepath <- "../DataList_10_1_2030-12_00_00-AM.csv"
#'   df <- munge_edms(filepath)
#'   publish_release(df)
#' }
publish_release <- function(df){

  #add version tag for date
  date_release <- format(Sys.Date(), "%Y.%m.%d")
  new_tag <- glue::glue("v{date_release}")

  #create a new release (default will go to the USAID-OHA-SI/mindthegap repo)
  piggyback::pb_new_release(tag = new_tag)

  #temp directory path
  fldr_temp <- tempdir()

  #current year for filename
  curr_year <- format(Sys.Date(), "%Y")

  ## Save data for ALL countries
  #save data as csv to temp folder
  readr::write_csv(df, file.path(fldr_temp, glue::glue("UNAIDS_{curr_year}_Clean_Estimates.csv.gz")))
  #save data as rds to temp folder
  saveRDS(df, file.path(fldr_temp, glue::glue("UNAIDS_{curr_year}_Clean_Estimates.rds")))

  #filter dataset to PEPFAR only
  df_pepfar <- dplyr::filter(df, pepfar == TRUE)

  ## Save data for PEPFAR-only countries
  #save data as csv to temp folder
  readr::write_csv(df_pepfar, file.path(fldr_temp, glue::glue("UNAIDS_{curr_year}_Clean_Estimates_PEPFAR-only.csv.gz")))
  #save data as rds to temp folder
  saveRDS(df_pepfar, file.path(fldr_temp, glue::glue("UNAIDS_{curr_year}_Clean_Estimates_PEPFAR-only.rds")))

  #local files
  local_outputs <- list.files(fldr_temp, "Clean_Estimates", full.names = TRUE)

  #upload to new tag folder
  piggyback::pb_upload(local_outputs, tag = new_tag)

  #upload multiple files to latest release for ease of access
  piggyback::pb_delete(
    file = NULL,
    repo = "USAID-OHA-SI/mindthegap",
    tag = "latest",
    .token = gh::gh_token()
  )

  piggyback::pb_upload(local_outputs, tag = "latest")

  cli::cli_alert_success("Data released uploaded ({new_tag}) to {.href [GH Release folder](https://github.com/USAID-OHA-SI/mindthegap/releases)}")


}
