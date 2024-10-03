# test-validate_columns.R

test_that("validate_columns passes when all required columns are present", {
  # Create a data frame with all required columns
  df <- data.frame(
    region = "Region1", e_count = 1, iso3 = "USA", e_cat = "A",
    e_ind = "Indicator1", acronym = "XYZ", age = 25, sex = "M",
    other = "Other1", time = "2024", formatted = TRUE
  )

  # Expect no error when all required columns are present
  expect_no_error(validate_columns(df, req_cols))
})

test_that("validate_columns throws error when required columns are missing", {
  # Create a data frame missing a required column (e.g., "sex")
  df <- data.frame(
    region = "Region1", e_count = 1, iso3 = "USA", e_cat = "A",
    e_ind = "Indicator1", acronym = "XYZ", age = 25,
    other = "Other1", time = "2024", formatted = TRUE
  )

  # Expect an error specifying the missing column(s)
  expect_error(validate_columns(df, req_cols), "missing 1 key column")
})

test_that("validate_columns throws error when multiple required columns are missing", {
  # Create a data frame missing two required columns (e.g., "sex" and "age")
  df <- data.frame(
    region = "Region1", e_count = 1, iso3 = "USA", e_cat = "A",
    e_ind = "Indicator1", acronym = "XYZ",
    other = "Other1", time = "2024", formatted = TRUE
  )

  # Expect an error specifying multiple missing columns
  expect_error(validate_columns(df, req_cols), "missing 2 key columns")
})

test_that("validate_columns throws error for an empty dataframe", {
  # Create an empty data frame
  df <- data.frame()

  # Expect an error due to all columns being missing
  expect_error(validate_columns(df), "missing 11 key columns")
})

test_that("validate_columns passes with additional columns present", {
  # Create a data frame with required columns and some extra columns
  df <- data.frame(
    region = "Region1", e_count = 1, iso3 = "USA", e_cat = "A",
    e_ind = "Indicator1", acronym = "XYZ", age = 25, sex = "M",
    other = "Other1", time = "2024", formatted = TRUE,
    extra_col = "Extra"
  )

  # Expect no error since all required columns are present
  expect_no_error(validate_columns(df, req_cols))
})
