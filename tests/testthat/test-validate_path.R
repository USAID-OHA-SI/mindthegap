# test-validate_path.R

test_that("validate_path handles valid CSV file correctly", {
  # Create a temporary CSV file path
  temp_csv <- withr::local_tempfile(fileext = ".csv")

  # Write some data to the CSV
  write.csv(data.frame(a = 1:3, b = letters[1:3]), temp_csv, row.names = FALSE)

  # Expect no error when providing a valid CSV file path
  expect_no_error(validate_path(temp_csv))
})

test_that("validate_path throws an error if file does not exist", {
  # Create a temporary file path that does not exist
  temp_csv <- withr::local_tempfile(fileext = ".csv")

  # Expect an error since the file does not exist
  expect_error(validate_path(temp_csv), "File does not exist")
})

test_that("validate_path throws an error if path is not a CSV", {
  # Create a temporary file with a different extension
  temp_txt <- withr::local_tempfile(fileext = ".txt")
  writeLines("This is a test file", temp_txt)

  # Expect an error for non-CSV file
  expect_error(validate_path(temp_txt), "provide a csv file")
})

test_that("validate_path throws an error if path vector length is greater than 1", {
  # Create a vector of paths
  paths <- c("file1.csv", "file2.csv")

  # Expect an error due to multiple paths
  expect_error(validate_path(paths), "accept a vector of length 1")
})
