# Setup test data - use enough data for nowcasting
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

# Single stratum reporting_triangle_df
rt_df <- as_reporting_triangle_df(data_as_of_df) |>
  truncate_to_delay(50)

# Create multi-stratum data
multi_strata_df <- rbind(data_as_of_df, data_as_of_df)
multi_strata_df$age_group <- c(
  rep("0-17", times = nrow(data_as_of_df)),
  rep("18+", times = nrow(data_as_of_df))
)

rt_df_strata <- as_reporting_triangle_df(
  multi_strata_df,
  by = "age_group"
) |> truncate_to_delay(50)

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
  # With strata sharing
  set.seed(123)
  nowcast_shared <- baselinenowcast(
    rt_df_strata,
    output_type = "samples",
    strata_sharing = c("delay", "uncertainty"),
    draws = 5
  )

  # Without strata sharing
  set.seed(123)
  nowcast_multi <- baselinenowcast(
    rt_df_strata,
    output_type = "samples",
    draws = 5
  )

  # Ensure they are not the same
  expect_false(all(nowcast_multi$pred_count == nowcast_shared$pred_count))

  expect_s3_class(nowcast_shared, "baselinenowcast_df")
  expect_true("age_group" %in% names(nowcast_shared))
  expect_true(all(c("0-17", "18+") %in% unique(nowcast_shared$age_group)))
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

# Comprehensive Functional Tests -------------------------------------------

test_that("baselinenowcast.reporting_triangle_df returns expected structure with strata", {
  expected_cols <- c("pred_count", "draw", "reference_date", "output_type")
  expected_cols_strata <- c(
    "pred_count", "draw", "reference_date", "output_type", "age_group"
  )

  # Single stratum
  nowcasts_single <- baselinenowcast_rt_df_test(
    data_as_of_df,
    by = NULL
  )
  expect_s3_class(nowcasts_single, "baselinenowcast_df")
  expect_setequal(names(nowcasts_single), expected_cols)
  expect_true(all(nowcasts_single$output_type == "samples"))

  # Multiple strata
  nowcasts_multi <- baselinenowcast_rt_df_test(
    multi_strata_df,
    by = "age_group"
  )
  expect_s3_class(nowcasts_multi, "baselinenowcast_df")
  expect_setequal(names(nowcasts_multi), expected_cols_strata)
  expect_true(all(c("0-17", "18+") %in% unique(nowcasts_multi$age_group)))
})

test_that("baselinenowcast.reporting_triangle_df supports different strata_sharing modes", {
  # No sharing
  set.seed(123)
  nowcast_no_share <- baselinenowcast_rt_df_test(
    multi_strata_df,
    by = "age_group",
    strata_sharing = "none"
  )

  # Delay sharing only
  set.seed(123)
  nowcast_delay_share <- baselinenowcast_rt_df_test(
    multi_strata_df,
    by = "age_group",
    strata_sharing = "delay"
  )

  # Uncertainty sharing only
  set.seed(123)
  nowcast_uncertainty_share <- baselinenowcast_rt_df_test(
    multi_strata_df,
    by = "age_group",
    strata_sharing = "uncertainty"
  )

  # Both delay and uncertainty sharing
  set.seed(123)
  nowcast_both_share <- baselinenowcast_rt_df_test(
    multi_strata_df,
    by = "age_group",
    strata_sharing = c("delay", "uncertainty")
  )

  # All should have valid structure
  expect_s3_class(nowcast_no_share, "baselinenowcast_df")
  expect_s3_class(nowcast_delay_share, "baselinenowcast_df")
  expect_s3_class(nowcast_uncertainty_share, "baselinenowcast_df")
  expect_s3_class(nowcast_both_share, "baselinenowcast_df")

  # Results should differ between sharing modes
  expect_false(identical(
    nowcast_no_share$pred_count,
    nowcast_both_share$pred_count
  ))
  expect_false(identical(
    nowcast_delay_share$pred_count,
    nowcast_uncertainty_share$pred_count
  ))
})

test_that("baselinenowcast.reporting_triangle_df accepts uncertainty parameters", {
  # Test with different uncertainty models
  nowcast1 <- baselinenowcast_rt_df_test(
    data_as_of_df,
    uncertainty_model = fit_by_horizon
  )
  expect_s3_class(nowcast1, "baselinenowcast_df")

  # Test with different uncertainty samplers
  nowcast2 <- baselinenowcast_rt_df_test(
    data_as_of_df,
    uncertainty_sampler = sample_nb
  )
  expect_s3_class(nowcast2, "baselinenowcast_df")
})

test_that("baselinenowcast.reporting_triangle_df handles preprocess parameter", {
  # With default preprocess
  nowcast1 <- baselinenowcast_rt_df_test(
    data_as_of_df,
    preprocess = preprocess_negative_values
  )
  expect_s3_class(nowcast1, "baselinenowcast_df")

  # With NULL preprocess
  nowcast2 <- baselinenowcast_rt_df_test(
    data_as_of_df,
    preprocess = NULL
  )
  expect_s3_class(nowcast2, "baselinenowcast_df")
})

test_that("baselinenowcast.reporting_triangle_df errors on invalid strata_sharing", {
  rt_df_test <- as_reporting_triangle_df(
    multi_strata_df,
    by = "age_group"
  ) |> truncate_to_delay(40)

  # Invalid strata_sharing value
  expect_error(
    baselinenowcast(
      rt_df_test,
      strata_sharing = "all"
    ),
    regexp = "Assertion on 'strata_sharing' failed:"
  )

  # Cannot combine "none" with others
  expect_error(
    baselinenowcast(
      rt_df_test,
      strata_sharing = c("none", "delay")
    ),
    regexp = "strata_sharing.*cannot be both 'none'"
  )
})

test_that("baselinenowcast.reporting_triangle_df produces consistent results with reporting_triangle", {
  skip_if_not_installed("dplyr")

  # Create via reporting_triangle
  set.seed(123)
  rep_tri <- as_reporting_triangle(data_as_of_df, max_delay = 40)
  nowcast_tri <- baselinenowcast(rep_tri, draws = 100)

  # Create via reporting_triangle_df
  set.seed(123)
  nowcast_df <- baselinenowcast_rt_df_test(data_as_of_df, by = NULL)

  # Results should be similar (allowing for small numerical differences)
  expect_equal(
    mean(nowcast_tri$pred_count),
    mean(nowcast_df$pred_count),
    tolerance = 0.1
  )
})

test_that("as_reporting_triangle_df validates required columns", {
  # Missing reference_date
  bad_data1 <- data_as_of_df
  names(bad_data1)[names(bad_data1) == "reference_date"] <- "ref_date"
  expect_error(
    as_reporting_triangle_df(bad_data1),
    "Required columns missing"
  )

  # Missing report_date
  bad_data2 <- data_as_of_df
  names(bad_data2)[names(bad_data2) == "report_date"] <- "rep_date"
  expect_error(
    as_reporting_triangle_df(bad_data2),
    "Required columns missing"
  )

  # Missing count
  bad_data3 <- data_as_of_df
  names(bad_data3)[names(bad_data3) == "count"] <- "cases"
  expect_error(
    as_reporting_triangle_df(bad_data3),
    "Required columns missing"
  )
})

test_that("as_reporting_triangle_df validates date types", {
  # reference_date not a Date
  bad_data1 <- data_as_of_df
  bad_data1$reference_date <- as.character(bad_data1$reference_date)
  expect_error(
    as_reporting_triangle_df(bad_data1),
    "Must be of type 'Date'"
  )

  # report_date not a Date
  bad_data2 <- data_as_of_df
  bad_data2$report_date <- as.character(bad_data2$report_date)
  expect_error(
    as_reporting_triangle_df(bad_data2),
    "Must be of type 'Date'"
  )
})

test_that("as_reporting_triangle_df validates strata columns", {
  # Strata column doesn't exist
  expect_error(
    as_reporting_triangle_df(
      data_as_of_df,
      by = "region"
    ),
    "Strata columns missing"
  )

  # Required column in strata (should error)
  expect_error(
    as_reporting_triangle_df(
      data_as_of_df,
      by = c("reference_date", "count")
    ),
    "Strata columns missing|Required columns"
  )
})

test_that("as_reporting_triangle_df handles duplicate combinations correctly", {
  # Create data with duplicates
  dup_data <- rbind(data_as_of_df[1:10, ], data_as_of_df[1:10, ])

  expect_error(
    as_reporting_triangle_df(dup_data),
    "Data contains duplicate combinations"
  )
})
