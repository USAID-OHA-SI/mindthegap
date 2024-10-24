test_that("construct_epi_plot creates a ggplot object without errors", {
  faux_data <- data.frame(
    country = rep("CountryA", 4),
    year = 2010L:2013L,
    deaths = runif(4, 0, 100),
    infections = runif(4, 0, 100)
  )
  min_max_year <- faux_data[c(1, 4), ]
  axis_limits <- calculate_axis_limits(faux_data)

  plot <- construct_epi_plot(faux_data, min_max_year, axis_limits)

  # Check if the returned object is a ggplot
  expect_s3_class(plot, "ggplot")
})
