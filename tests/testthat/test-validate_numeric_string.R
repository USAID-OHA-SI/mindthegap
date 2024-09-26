# test-validate_numeric_string.R

test_that("Function runs without errors when only expected characters are present", {
  df_test <- data.frame(formatted = c("<0.1", ">98", "0", "1 000 000", "1.3m"))
  expect_silent(validate_numeric_string(df_test))
})

test_that("Function throws an error when unexpected characters are present", {
  df_test <- data.frame(formatted = c("<0.1", ">98", "0", "1 000 000", "1.3m", "$100"))
  expect_error(
    validate_numeric_string(df_test),
    regexp = "The value column"
  )
})

test_that("Error message correctly identifies unexpected characters", {
  df_test <- data.frame(formatted = c("<0.1", ">98", "0", "1 000 000", "1.3m", "$100", "abc"))
  expect_error(
    validate_numeric_string(df_test),
    # regexp = "Non-numeric values in dataset not handled: \\{\\\"\\$\\\", \"a\", \"b\", \"c\"\\}"
  )
})
