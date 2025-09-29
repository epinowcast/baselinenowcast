# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that("as_reporting_triangle.data.frame() works as expected", { # nolint


  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_equal(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
  expected_structure <- 1
  expect_equal(rep_tri$structure, expected_structure)
})



test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- as_reporting_triangle(test,
    max_delay = 25
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_equal(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(test$reference_date))
  )

  # calculate the expected structure which will be all 1s except for 1 2 where
  # ref date is missing.
  expected_structure <- c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) # nolint
  expect_equal(rep_tri$structure, expected_structure)
})
