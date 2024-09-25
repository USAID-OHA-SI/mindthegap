# Test for age and sex standarization
test_that("standarize_agesex produces correct set of age/sex combinations", {


  df_agesex <- tibble::tibble(sex = c("M", "F", "M+F"),
                 age = c("allAges", "15+", "0-14"))

  output <- df_agesex %>%
    standarize_agesex()

  expect_equal(output$sex, c("Male", "Female", "All"))
  expect_equal(output$age, c("All", "15+", "0-14"))

}
)


test_that("parse_indicator correctly removes extraneous front and back matter from indicator", {

  df_ind <- tibble::tribble(
                                                                                             ~e_ind,                                                              ~expected,
                                       "O- Total deaths to HIV Population Male+Female; Lower bound",                                       "Total deaths to HIV Population",
                   "O-2nd 90- Among people living with HIV- the percent on ART; (15+) Male - Lower",                     "Among people living with HIV- the percent on ART",
                                                         "O-2- Number on ART; (15+) Female - Lower",                                                        "Number on ART",
                                                             "O- HIV population (0-14) Male+Female",                                                       "HIV population",
                                                           "O-2- Number on ART; (15+) Male - Upper",                                                        "Number on ART",
                   "O-3rd 86- Among people living with HIV- the percent with suppressed viral load", "Among people living with HIV- the percent with suppressed viral load",
                                                           "O-2- Number on ART; (15+) Male - Lower",                                                        "Number on ART",
            "O-3rd 86- Among people living with HIV- the percent with suppressed viral load; (15+)", "Among people living with HIV- the percent with suppressed viral load",
                                              "O- Total deaths to HIV Population Male; Upper bound",                                       "Total deaths to HIV Population",
                                                 "O- HIV population (15+) Male+Female; Upper bound",                                                       "HIV population",
                                                                   "O-2- Number on ART; (15+) Male",                                                        "Number on ART",
               "O-2nd 95- Among people who know their HIV status- the percent on ART; (15+) Female",           "Among people who know their HIV status- the percent on ART",
    "O-1st 95- Among people living with HIV- the percent who know their status; (15+) Male - Lower",      "Among people living with HIV- the percent who know their status",
                                                    "O-1- Number who know their HIV status; (0-14)",                                     "Number who know their HIV status",
                                                "O- HIV population (0-14) Male+Female; Upper bound",                                                       "HIV population"
    )



  expect_true(parse_indicator(df_ind) %>%
                dplyr::mutate(correct = indicator_edms == expected) %>%
                dplyr::pull() %>%
                all())

}
)

# test_that("munge_components handles missing indicators from map", {
#
#
#
# })

