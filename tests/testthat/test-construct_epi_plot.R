test_that("construct_epi_plot creates a ggplot object without errors", {
  faux_data <- data.frame(
    country = rep("CountryA", 4),
    year = 2010:2013,
    deaths = runif(4, 0, 100),
    infections = runif(4, 0, 100)
  )
  min_max_year <- faux_data[c(1, 4), ]
  axis_limits <- calculate_axis_limits(faux_data)

  plot <- construct_epi_plot(faux_data, min_max_year, axis_limits)

  # Check if the returned object is a ggplot
  expect_s3_class(plot, "ggplot")
})

test_that("plot_connected_scatter creates a ggplot object without errors", {
  faux_data <- data.frame(
    country = rep(c("CountryA", "CountryB"), each = 5),
    year = rep(2010:2014, 2),
    deaths = runif(10, 0, 100),
    infections = runif(10, 0, 100)
  )

  plot <- plot_connected_scatter(faux_data, "CountryA", 2010:2014)

  # Check if the returned object is a ggplot
  expect_s3_class(plot, "ggplot")
})
