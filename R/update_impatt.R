#' Update IMPATT Dataset
#'
#' @param filepath filepath to IMPATT data (from pepfar-panorama.org)
#' @param usaid_email USAID email to store file on GDrive
#'
#' @export
#' @importFrom googlesheets4 as_sheets_id
#'
#' @examples
#'  \dontrun{
#'    path <- "~/Data/MER_Structured_Datasets_NAT_SUBNAT_FY15-21_20200918_v2_1.rds"
#'    update_impatt(path, "twhitmore@usaid.gov") }

update_impatt <- function(filepath, usaid_email){

  if(!file.exists(filepath))
    stop("Filepath does not exist or bad path")

  if(!grepl("usaid.gov$", usaid_email))
    stop("USAID email address required for GDrive Access")

  package_check("googlesheets4")

  #import IMPATT data
    if(grepl("rds$", filepath)){
      df_impatt <- readr::read_rds(filepath)
    } else {
      df_impatt <- ICPIutilities::read_msd(filepath)
    }

  #filter to PLHIV and TX_CURR_SUBNAT for most recent year
    df_impatt <- df_impatt %>%
      dplyr::filter(indicator %in% c("PLHIV", "TX_CURR_SUBNAT"),
                    standardizeddisaggregate == "Age/Sex/HIVStatus",
                    fiscal_year == max(fiscal_year))

  #aggregate by country, age and sex
    df_impatt <- df_impatt %>%
      dplyr::group_by(operatingunit, countryname, fiscal_year, indicator, ageasentered, sex) %>%
      dplyr::summarise(value = sum(targets, na.rm = TRUE)) %>%
      dplyr::ungroup()

  #create ART coverage share
    df_impatt <- df_impatt %>%
      tidyr::pivot_wider(names_from = indicator) %>%
      dplyr::mutate(share_on_ART = TX_CURR_SUBNAT/PLHIV)

  #authenticate
    if(!googlesheets4::gs4_has_token())
      googlesheets4::gs4_auth(usaid_email)

  #update sheet
    googlesheets4::sheet_delete(as_sheets_id(gs), "ARTshare")
    googlesheets4::sheet_write(df_impatt, as_sheets_id(gs), "ARTshare")

  #return
    invisible(df_impatt)

}
