# Sample data frame for testing
test_data <- data.frame(
  region = rep(c("Region1", "Region2"), each = 4),
  country = rep(c("CountryA", "CountryB"), each = 4),
  year = rep(c(2015, 2020), each = 2, times = 2),
  iso = rep(c("CYA", "CYB"), each = 4),
  indicator = rep(c("Number Total Deaths to HIV Population", "Number New HIV Infections"), times = 4),
  estimate = c(1000, 1500, 1200, 1800, 1100, 1400, 1300, 1600),
  age = rep("All", 8),
  sex = rep("All", 8),
  pepfar = rep(c(TRUE, FALSE), times = 4)
)

# Unit tests for prep_epi_data function
test_that("prep_epi_data filters by country and year", {
  result <- prep_epi_data(test_data, sel_cntry = "CountryA", year_range = 2015:2020)
  expect_equal(unique(result$country), "CountryA")
  expect_equal(sort(unique(result$year)), c(2015, 2020))
})

test_that("prep_epi_data returns error for missing country", {
  expect_error(prep_epi_data(test_data, sel_cntry = "NonExistentCountry", year_range = 2015:2020))
})

test_that("prep_epi_data renames columns correctly", {
  result <- prep_epi_data(test_data, sel_cntry = "CountryA", year_range = 2015:2020)
  expect_true(all(c("deaths", "infections") %in% colnames(result)))
  expect_false("indicator" %in% colnames(result))
})
