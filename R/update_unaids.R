#' Update UNAIDS 90-90-90 data
#'
#' @param filepaths filepaths to all 3 UNAIDS files - ALL, Female, Male
#' @param usaid_email USAID email to store file on GDrive
#'
#' @export
#' @importFrom googlesheets4 as_sheets_id
#'
#' @examples
#' \dontrun{
#'    files <- list.files("data-raw", full.names = TRUE)
#'    update_unaids(files, "twhitmore@usaid.gov") }

update_unaids <- function(filepaths, usaid_email){

  #import
    df <- vroom::vroom(filepaths, col_names = unaids_hdrs(), col_types = c(.default = "c"), skip = 2, id = "file")

  #munge
    df <- df %>%
      tidyr::gather(ind, value, -country, -file) %>%
      tidyr::separate(ind, c("year", "indicator", "type"), sep = "\\.") %>%
      dplyr::mutate(value = dplyr::na_if(value, "..."),
                    sex =  stringr::str_extract(file, "Female|Male|All"),
                    age = "15+") %>%
      dplyr::select(-file) %>%
      dplyr::select(-type, -value, dplyr::everything())


  #reshape wide and create a label for values
    df <- df %>%
      dplyr::mutate(value =  stringr::str_remove_all(value, "<|>| ") %>% as.numeric(.)/100) %>%
      tidyr::spread(type, value) %>%
      dplyr::relocate(value, lower, upper, .after = where(is.character)) %>%
      dplyr::mutate(label = scales::percent(value, 1))
                  # label = dplyr::na_if(label, "NA%"))

  #clean up indicator names
    df <- df %>%
      dplyr::mutate(indicator = dplyr::recode(indicator,
                                              "known_status" = "Known Status",
                                              "on_art" = "On ART",
                                              "vl_suppression" = "Virally Suppressed"))
  #adjust country names
    df <- df %>%
      dplyr::mutate(country = dplyr::case_when(stringr::str_detect(country, "d'Ivoire") ~ "Cote d'Ivoire",
                                               country == "United Republic of Tanzania" ~ "Tanzania",
                                               country == "Viet Nam" ~ "Vietnam",
                                               TRUE ~ country)
      )

  #authenticate
    if(!googlesheets4::gs4_has_token())
      googlesheets4::gs4_auth(usaid_email)

  #update sheet
    googlesheets4::sheet_delete(as_sheets_id(gs_id), "UNAIDS")
    googlesheets4::sheet_write(df, as_sheets_id(gs_id), "UNAIDS")

  #return
    invisible(df)
}


#' Generate UNAIDS headers
#'
#' @return list of headers

unaids_hdrs <- function(){

  #create headers (due to repeated headers over two lines)
  years <- c(2015:2019)
  indicator <- forcats::as_factor(c("known_status", "on_art", "vl_suppression"))
  type <- forcats::as_factor(c("value", "lower", "upper"))

  headers <- tidyr::crossing(years, indicator, type) %>%
    tidyr::unite(headrs, sep = ".") %>%
    dplyr::pull()

  headers <- c("country", headers)

  return(headers)
}
