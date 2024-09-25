# test-validate_ind_disaggs.R

# Define the tests
test_that("validate_ind_disaggs passes when all expected indicators and disaggregates are present", {
  # Create a mock dataframe with all expected indicators and disaggregates
  df <- expected_ind %>%
    dplyr::rename(e_cat = source)  # Simulating the EDMS dataset format

  # Expect no warnings when all expected indicators/disaggregates are present
  expect_no_warning(validate_ind_disaggs(df))
})

test_that("validate_ind_disaggs throws warning for missing indicators or disaggregates", {
  # Create a mock dataframe missing one expected indicator/disaggregate
  df <- expected_ind %>%
    dplyr::slice_tail(n = -1) %>%  # Removing one indicator
    dplyr::rename(e_cat = source)

  # Expect a warning for the missing indicator
  expect_warning(validate_ind_disaggs(df), "1 expected item is missing")
})

test_that("validate_ind_disaggs throws warning for additional indicators or disaggregates", {
  # Create a mock dataframe with additional indicators not in expected_ind
  df <- expected_ind %>%
    dplyr::rename(e_cat = source)

  df_additional <- data.frame(
    indicator_edms = "New_Indicator",
    age = "15+",
    sex = "M",
    e_cat = "SourceA",
    expected = FALSE
  )

  df <- dplyr::bind_rows(df, df_additional)  # Adding an additional indicator

  # Expect a warning for the additional indicator
  expect_warning(validate_ind_disaggs(df), "1 additional item is included")
})

test_that("validate_ind_disaggs throws both missing and additional warnings", {
  # Create a mock dataframe missing one expected indicator and with an additional one
  df_missing <- expected_ind %>%
    dplyr::slice_tail(n = -1) %>% # Removing one indicator
    dplyr::rename(e_cat = source)

  df_additional <- data.frame(
    indicator_edms = "New_Indicator",
    age = "15+",
    sex = "M",
    e_cat = "SourceA",
    expected = FALSE
  )

  df <- dplyr::bind_rows(df_missing, df_additional)  # Adding an additional indicator

  # Expect warnings for both missing and additional indicators
  expect_warning(validate_ind_disaggs(df), "expected item is missing")
  expect_warning(validate_ind_disaggs(df), "additional item is included")
})


