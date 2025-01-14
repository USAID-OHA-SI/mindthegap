# test-validate_countries.R

pepfar_countries <- pepfar$country_pepfar

test_that("validate_countries passes when all PEPFAR countries are present", {
  # Create a data frame with all PEPFAR countries
  df <- data.frame(
    country = pepfar_countries,
    pepfar = TRUE
  )

  # Expect no warnings when all PEPFAR countries are present
  expect_no_warning(validate_countries(df))
})

test_that("validate_countries throws warning for missing PEPFAR countries", {
  # Create a data frame missing two PEPFAR countries (e.g., "Angola" and "Burma")
  df <- data.frame(
    country = setdiff(pepfar_countries, c("Angola", "Burma")),
    pepfar = TRUE
  )

  # Expect a warning with the correct message about the missing countries
  expect_warning(validate_countries(df), "2 expected countries are missing")
})

test_that("validate_countries handles non-PEPFAR countries without warning", {
  # Create a data frame with both PEPFAR and non-PEPFAR countries
  df <- data.frame(
    country = c(pepfar_countries, "NonPEPFARCountry"),
    pepfar = c(rep(TRUE, length(pepfar_countries)), FALSE)
  )

  # Expect no warnings since all PEPFAR countries are present
  expect_no_warning(validate_countries(df))
})

test_that("validate_countries throws warning when no PEPFAR countries are present", {
  # Create a data frame with non-PEPFAR countries only
  df <- data.frame(
    country = c("CountryA", "CountryB"),
    pepfar = FALSE
  )

  # Expect a warning for all missing PEPFAR countries
  expect_warning(validate_countries(df))
})

