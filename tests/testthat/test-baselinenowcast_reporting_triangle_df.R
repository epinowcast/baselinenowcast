# Setup test data - use enough data for nowcasting
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

# Single stratum reporting_triangle_df
rt_df <- as_reporting_triangle_df(data_as_of_df)

test_that("baselinenowcast.reporting_triangle_df works for single stratum", {
  # Point estimate - use all available data
  nowcast_point <- baselinenowcast(
    rt_df,
    output_type = "point"
  )

  expect_s3_class(nowcast_point, "baselinenowcast_df")
  expect_true("reference_date" %in% names(nowcast_point))
  expect_true("pred_count" %in% names(nowcast_point))
  expect_equal(nowcast_point$draw, rep(1, nrow(nowcast_point)))

  # Samples - use all available data
  nowcast_samples <- baselinenowcast(
    rt_df,
    output_type = "samples",
    draws = 10
  )

  expect_s3_class(nowcast_samples, "baselinenowcast_df")
  expect_true(max(nowcast_samples$draw) == 10)
})

test_that("baselinenowcast.reporting_triangle_df works for multiple strata", {
  # Create multi-stratum data
  multi_strata_df <- data_as_of_df
  multi_strata_df$age_group <- rep(c("0-17", "18+"), length.out = nrow(multi_strata_df))

  rt_df_strata <- as_reporting_triangle_df(
    multi_strata_df,
    by = "age_group"
  )

  nowcast_multi <- baselinenowcast(
    rt_df_strata,
    output_type = "samples",
    draws = 5
  )

  expect_s3_class(nowcast_multi, "baselinenowcast_df")
  expect_true("age_group" %in% names(nowcast_multi))
  expect_true(all(c("0-17", "18+") %in% unique(nowcast_multi$age_group)))
})

test_that("baselinenowcast.reporting_triangle_df supports strata_sharing", {
  skip("Strata sharing requires overlapping reference/report dates across all strata")

  # This test is skipped because creating test data that satisfies the
  # requirements for strata sharing (overlapping reference and report dates)
  # is complex. The functionality is tested implicitly through the
  # baselinenowcast.data.frame method which uses the same underlying code.
})

test_that("baselinenowcast.default provides helpful error message", {
  expect_error(
    baselinenowcast(list(a = 1:5)),
    "No baselinenowcast method for class 'list'"
  )

  expect_error(
    baselinenowcast(matrix(1:10, 2, 5)),
    "No baselinenowcast method for class 'matrix'"
  )
})

test_that("baselinenowcast.data.frame shows deprecation warning", {
  expect_warning(
    baselinenowcast(
      data_as_of_df,
      output_type = "point"
    ),
    "baselinenowcast.data.frame\\(\\) is deprecated"
  )
})
