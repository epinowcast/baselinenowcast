# Setup test data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

# Single stratum reporting_triangle_df
rt_df <- as_reporting_triangle_df(data_as_of_df)

# Multi-stratum test data
multi_strata_df <- data_as_of_df
multi_strata_df$age_group <- rep(c("0-17", "18-49", "50+"), length.out = nrow(multi_strata_df))
multi_strata_df$location <- rep(c("urban", "rural"), length.out = nrow(multi_strata_df))

test_that("as_reporting_triangle_df creates valid object from data.frame", {
  expect_s3_class(rt_df, "reporting_triangle_df")
  expect_s3_class(rt_df, "data.frame")
  expect_true(is_reporting_triangle_df(rt_df))

  # Check required columns
  expect_true(all(c("reference_date", "report_date", "count") %in% names(rt_df)))

  # Check attributes
  expect_equal(attr(rt_df, "delays_unit"), "days")
  expect_null(attr(rt_df, "strata"))
})

test_that("as_reporting_triangle_df handles strata correctly", {
  rt_df_strata <- as_reporting_triangle_df(
    multi_strata_df,
    by = c("age_group", "location")
  )

  expect_s3_class(rt_df_strata, "reporting_triangle_df")
  expect_equal(attr(rt_df_strata, "strata"), c("age_group", "location"))

  # Check strata columns are preserved
  expect_true(all(c("age_group", "location") %in% names(rt_df_strata)))
})

test_that("is_reporting_triangle_df returns correct logical", {
  expect_true(is_reporting_triangle_df(rt_df))
  expect_false(is_reporting_triangle_df(data.frame(a = 1:5)))
  expect_false(is_reporting_triangle_df(matrix(1:10, 2, 5)))
})

test_that("get_strata returns correct strata attribute", {
  expect_null(get_strata(rt_df))

  rt_df_strata <- as_reporting_triangle_df(
    multi_strata_df,
    by = c("age_group", "location")
  )
  expect_equal(get_strata(rt_df_strata), c("age_group", "location"))

  # Test error with non-reporting_triangle_df
  expect_error(
    get_strata(data.frame(a = 1:5)),
    "must have class 'reporting_triangle_df'"
  )
})

test_that("as_reporting_triangle_df.reporting_triangle works", {
  # Create a reporting_triangle first
  rt <- as_reporting_triangle(data_as_of_df)

  # Convert to reporting_triangle_df
  rt_df_from_rt <- as_reporting_triangle_df(rt)

  expect_s3_class(rt_df_from_rt, "reporting_triangle_df")
  expect_true(all(c("reference_date", "report_date", "count") %in% names(rt_df_from_rt)))
  expect_null(attr(rt_df_from_rt, "strata"))
})

test_that("as_reporting_triangle.reporting_triangle_df works for single stratum", {
  rt_from_df <- as_reporting_triangle(rt_df)

  expect_s3_class(rt_from_df, "reporting_triangle")
  expect_s3_class(rt_from_df, "matrix")
})

test_that("as_reporting_triangle.reporting_triangle_df errors on multiple strata", {
  rt_df_strata <- as_reporting_triangle_df(
    multi_strata_df,
    by = c("age_group", "location")
  )

  expect_error(
    as_reporting_triangle(rt_df_strata),
    "Multiple strata detected"
  )
})

test_that("as_reporting_triangles works correctly", {
  # Single stratum
  rt_list <- as_reporting_triangles(rt_df)
  expect_type(rt_list, "list")
  expect_length(rt_list, 1)
  expect_s3_class(rt_list[[1]], "reporting_triangle")

  # Multiple strata
  rt_df_strata <- as_reporting_triangle_df(
    multi_strata_df,
    by = c("age_group", "location")
  )
  rt_list_multi <- as_reporting_triangles(rt_df_strata)
  expect_type(rt_list_multi, "list")
  expect_true(length(rt_list_multi) > 1)
  expect_true(all(sapply(rt_list_multi, is_reporting_triangle)))
})

test_that("subsetting preserves reporting_triangle_df class", {
  subset_df <- rt_df[1:10, ]
  expect_s3_class(subset_df, "reporting_triangle_df")
  expect_equal(attr(subset_df, "delays_unit"), "days")
})

test_that("head and tail work correctly", {
  head_df <- head(rt_df, n = 5)
  expect_s3_class(head_df, "reporting_triangle_df")
  expect_equal(nrow(head_df), 5)

  tail_df <- tail(rt_df, n = 3)
  expect_s3_class(tail_df, "reporting_triangle_df")
  expect_equal(nrow(tail_df), 3)
})

test_that("as.data.frame removes class and attributes", {
  plain_df <- as.data.frame(rt_df)
  expect_s3_class(plain_df, "data.frame")
  expect_false(inherits(plain_df, "reporting_triangle_df"))
  expect_null(attr(plain_df, "delays_unit"))
  expect_null(attr(plain_df, "strata"))
})

test_that("print.reporting_triangle_df runs without error", {
  expect_output(print(rt_df), "Reporting Triangle DataFrame")
  expect_output(print(rt_df), "Delays unit: days")
})

test_that("summary.reporting_triangle_df runs without error", {
  expect_output(summary(rt_df), "Reporting Triangle DataFrame Summary")
  expect_output(summary(rt_df), "Delays unit: days")
})

test_that("validate_reporting_triangle_df catches invalid data", {
  # Missing required columns
  invalid_df <- data.frame(x = 1:5, y = 1:5)
  class(invalid_df) <- c("reporting_triangle_df", "data.frame")
  attr(invalid_df, "delays_unit") <- "days"

  expect_error(
    validate_reporting_triangle_df(invalid_df),
    "Required columns missing"
  )

  # Invalid class
  expect_error(
    validate_reporting_triangle_df(data.frame(a = 1:5)),
    "must have class 'reporting_triangle_df'"
  )
})
