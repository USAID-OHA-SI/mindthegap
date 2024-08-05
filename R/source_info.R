#' Year UNAIDS data are released through
#'
#' @keywords internal
#'
unaids_year <- 2023

#' UNAIDS Source Info
#'
#' Returns UNAIDS Source info for consistent sourcing notes
#'
#' @export
#'

source_note <- glue::glue("Source: UNAIDS AIDSinfo Global Data {unaids_year + 1} Release") %>% as.character()

