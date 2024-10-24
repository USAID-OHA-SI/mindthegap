test_that("prep_epi_data processes data correctly", {
  # Mock dataset
  mock_data <- data.frame(
    region = c("Africa", "Asia"),
    country = c("CountryA", "CountryB"),
    year = c(2020L, 2020L),
    iso = c("A", "B"),
    indicator = c("Number Total Deaths to HIV Population", "Number New HIV Infections"),
    estimate = c(1000, 2000),
    age = c("All", "All"),
    sex = c("All", "All"),
    pepfar = c("Yes", "No")
  )

  # Run the function
  result <- prep_epi_data(mock_data)

  # Check if the result is a dataframe with expected columns
  expect_s3_class(result, "data.frame")
  expect_setequal(names(result), c("region", "country", "year", "iso", "pepfar", "deaths", "infections"))

  # Check values are correctly transformed
  expect_equal(result$deaths[1], 1000)
  expect_equal(result$infections[2], 2000)
})
