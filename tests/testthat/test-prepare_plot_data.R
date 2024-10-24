test_that("prepare_plot_data filters data correctly", {
  # Creating a faux dataset for testing
  faux_data <- data.frame(
    country = rep(c("CountryA", "CountryB"), each = 5),
    year = rep(2010:2014, 2),
    deaths = runif(10, 0, 100),
    infections = runif(10, 0, 100)
  )

  country_filter <- "CountryA"
  year_range <- 2011:2013
  result <- prepare_plot_data(faux_data, country_filter, year_range)

  # Check filtered data contains only the specified country and year range
  filtered_data <- result$filtered_data
  expect_setequal(unique(filtered_data$country), country_filter)
  expect_true(all(filtered_data$year %in% year_range))

  # Ensure min_max_year contains only the min and max year data
  min_max_year <- result$min_max_year
  expect_equal(nrow(min_max_year), 2)
  expect_setequal(min_max_year$year, range(year_range))
})
