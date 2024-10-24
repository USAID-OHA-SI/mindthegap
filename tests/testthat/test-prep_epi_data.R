test_that("prep_epi_data returns the expected dataframe structure", {
  result <- prep_epi_data()

  # Check if the output is a dataframe
  expect_s3_class(result, "data.frame")

  # Check if the correct columns are present
  expected_columns <- c("region", "country", "year", "iso", "pepfar", "deaths", "infections")
  expect_setequal(names(result), expected_columns)

  # Check if columns have the expected types
  expect_type(result$region, "character")
  expect_type(result$year, "integer")
  expect_type(result$deaths, "double")
  expect_type(result$infections, "double")

  # Test filtering on the "indicator" column
  expect_true(all(result$deaths >= 0, na.rm = TRUE))
  expect_true(all(result$infections >= 0, na.rm = TRUE))
})
