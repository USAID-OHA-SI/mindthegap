test_that("calculate_axis_limits calculates global min and max", {
  faux_data <- data.frame(
    deaths = c(10, 20, 0, NA, 30),
    infections = c(15, 25, 3, 10, 50)
  )

  limits <- calculate_axis_limits(faux_data)
  expect_length(limits, 2)
  expect_equal(limits[1], 0)  # Minimum value
  expect_equal(limits[2], 50) # Maximum value
})
