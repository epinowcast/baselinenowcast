# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that(".validate_rep_tri_df() errors if missing required columns", { # nolint
  data <- data_as_of_df[-1] # remove a column
  expect_error(
    .validate_rep_tri_df(data, delays_unit = "days"),
    regexp = "Required columns missing from data"
  )
})

test_that(".validate_rep_tri_df() errors if there are duplicate pairs of reference and report dates", { # nolint
  df_dup <- bind_rows(data_as_of_df, data_as_of_df)
  expect_error(
    .validate_rep_tri_df(df_dup, delays_unit = "days"),
    regexp = "Data contains duplicate `reference_date` and `report_date` combinations" # nolint
  ) # nolint
})

test_that(".validate_rep_tri_df() can handle a ragged triangle with a single missing reference date", { # nolint
  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- expect_warning(
    .validate_rep_tri_df(test, delays_unit = "days"),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  )
})

test_that(".validate_rep_tri_df() warns when maximum report date is greater than the reference date", { # nolint
  expect_warning(.validate_rep_tri_df(syn_nssp_df, delays_unit = "days"),
    regexp = "The dataframe contains report dates beyond the final reference date." # nolint
  )
})
