# Unit tests for prepare_plot_data function
test_that("prepare_plot_data returns filtered data and min-max year data", {

  test_data <- data.frame(
    country = rep(c("CountryA", "CountryB"), each = 2),
    year = c(2015, 2020, 2015, 2020),
    deaths = c(100, 150, 200, 250)
  )

  result <- prepare_plot_data(test_data)

  # Check that the output contains filtered_data and min_max_year
  expect_true("filtered_data" %in% names(result))
  expect_true("min_max_year" %in% names(result))

  # Check that min_max_year contains only min and max years
  expect_equal(sort(unique(result$min_max_year$year)), c(2015, 2020))
})
