## code to prepare `req_cols` dataset goes here
req_cols <- c("region", "e_count", "iso3", "e_ind", "acronym", "acronymcombined","age", "sex",  "other", "time", "formatted")

usethis::use_data(req_cols, overwrite = TRUE)
