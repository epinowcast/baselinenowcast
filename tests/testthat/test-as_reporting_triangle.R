data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]


test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- as_reporting_triangle(test,
    max_delay = 25
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_equal(nrow(rep_tri$rep_tri_mat), length(unique(test$reference_date)))

  # calculate the expected structure which will be all 1s except for 1 2 where
  # ref date is missing.

  expect_equal(rep_tri$structure, expected_structure)
})
