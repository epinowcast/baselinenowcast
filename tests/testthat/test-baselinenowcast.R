data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df$age_group <- "00+"
rep_tri <- as_reporting_triangle(
  data = data_as_of_df,
  max_delay = 25,
  strata = "age_group"
)
expected_cols <- c("pred_count", "draw", "reference_date", "age_group")
test_that("baselinenowcast.reporting_triangle() works as expected", {
  nowcast_df <- baselinenowcast(rep_tri, draws = 100)
  expect_s3_class(nowcast_df, "data.frame")
  expect_true(all(expected_cols %in% colnames(nowcast_df)))
})

test_that("baselinenowcast.reporting_triangle() handles separate delay and uncertainty estimates appropriately", { # nolint
  expect_error(
    baselinenowcast(rep_tri,
      delay_pmf = rep(1 / 24, 24),
      draws = 100
    ),
    regexp = "`delay_pmf` is not the same length as the number"
  )

  test_df <- expect_no_error(baselinenowcast(rep_tri,
    delay_pmf = rep(1 / 26, 26),
    draws = 100
  ))
  expect_s3_class(test_df, "data.frame")
  expect_true(all(expected_cols %in% colnames(test_df)))

  expect_warning(
    baselinenowcast(rep_tri,
      delay_pmf = rep(0.2, 26),
      draws = 100
    ),
    regexp = "`delay_pmf` does not sum to approximately one."
  ) # nolint

  expect_error(
    baselinenowcast(rep_tri,
      uncertainty_params = rep(1, 10)
    ),
    regexp = "`uncertainty_params` are not the same length"
  )

  test_df2 <- baselinenowcast(rep_tri,
    uncertainty_params = rep(1, 25),
    draws = 100
  )

  expect_s3_class(test_df2, "data.frame")
  expect_true(all(expected_cols %in% colnames(test_df2)))
})
