# Test for age and sex standardization ------------------------------------

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



  df_result <- parse_indicator(df_ind)
  expect_equal(df_result$expected, df_result$indicator_edms)

}
)



# Check map_indicator -----------------------------------------------------


# Mock the indicator_map dataframe from the CSV
indicator_map <- data.frame(
  acronym = c("Prev", "AM", "INC%"),
  indicator = c("Percent Prevalence", "Number AIDS Related Deaths", "Incidence (per 1,000)"),
  stringsAsFactors = FALSE
)

# Sample dataframe to test the function
sample_df <- data.frame(
  acronym = c("Prev", "AM", "UnknownAcronym"),
  indicator_edms = c("Adult HIV prevalence", "AIDS deaths", "Unknown Indicator"),
  e_cat = c("Cat1", "Cat2", "Cat3"),
  e_ind = c("Ind1", "Ind2", "Ind3"),
  stringsAsFactors = FALSE
)

# Unit test for `map_indicator`
test_that("map_indicator correctly maps indicator names and handles missing acronyms", {
  # Apply the map_indicator function
  mapped_df <- map_indicator(sample_df)

  # Expected output dataframe
  expected_df <- data.frame(
    indicator = c("Prevalence", "Number AIDS Related Deaths", "Unknown Indicator"),
    stringsAsFactors = FALSE
  )

  # Ensure the correct indicator mapping and handling of missing acronyms
  expect_equal(mapped_df$indicator, expected_df$indicator)
})

# Testing for dropped columns
test_that("map_indicator drops unnecessary columns", {
  mapped_df <- map_indicator(sample_df)

  # Check that columns 'e_cat', 'e_ind', 'indicator_edms', 'acronym' are removed
  expect_false(any(c("e_cat", "e_ind", "indicator_edms", "acronym") %in% names(mapped_df)))
})

# Testing for correct ordering of columns
test_that("map_indicator places indicator column before indicator_edms", {
  mapped_df <- map_indicator(sample_df)

  # Check that the 'indicator' column comes before 'indicator_edms' in the output
  expect_equal(which(names(mapped_df) == "indicator"), 1)
})



# Test apply_indicator_type -----------------------------------------------

#dataset
df_expected_type <- tibble::tribble(
                               ~indicator, ~expected_type,
             "Number AIDS Related Deaths",      "Integer",
               "Number PMTCT Needing ART",      "Integer",
              "Number New HIV Infections",      "Integer",
  "Number Total Deaths to HIV Population",      "Integer",
             "Number PMTCT Receiving ART",      "Integer",
           "Number Deaths Averted by ART",      "Integer",
     "Number Infections Averted by PMTCT",      "Integer",
                           "Number PLHIV",      "Integer",
                 "Number on ART of PLHIV",      "Integer",
           "Number Known Status of PLHIV",      "Integer",
                    "Number VLS of PLHIV",      "Integer",
                "Percent on ART of PLHIV",      "Percent",
          "Percent Known Status of PLHIV",      "Percent",
       "Percent on ART with Known Status",      "Percent",
                   "Percent VLS of PLHIV",      "Percent",
                     "Percent VLS on ART",      "Percent",
                     "Percent Prevalence",         "Rate",
                  "Incidence (per 1,000)",         "Rate"
  )


# Test for correct mapping of indicator_type
test_that("apply_indicator_type correctly assigns indicator types", {
  # Apply the function
  df_result <- apply_indicator_type(df_expected_type)

  # Check that the resulting indicator_type matches the expected type from the CSV
  expect_equal(df_result$indicator_type, df_result$expected_type)
})

# Test for handling of unknown indicators
test_that("apply_indicator_type assigns 'Unknown' for unrecognized indicators", {
  # Create a test dataframe with both known and unknown indicators
  df_test <- data.frame(
    indicator = c("New Indicator", "Percent on ART", "Number on ART", "Another Unknown"),
    stringsAsFactors = FALSE
  )

  # Apply the function
  df_result <- apply_indicator_type(df_test)

  # Expected output for unknown indicators should be "Unknown"
  expect_equal(df_result$indicator_type, c("Unknown", "Percent", "Integer", "Unknown"))
})

