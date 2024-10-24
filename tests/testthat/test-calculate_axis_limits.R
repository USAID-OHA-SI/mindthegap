test_that("calculate_axis_limits calculates global min and max", {
  faux_data <- data.frame(
    deaths = c(10, 20, 5, NA, 30),
    infections = c(15, 25, 5, 10, 50)
  )

  limits <- calculate_axis_limits(faux_data)
  expect_length(limits, 2)
  expect_equal(limits[1], 5)  # Minimum value
  expect_equal(limits[2], 50) # Maximum value
})
