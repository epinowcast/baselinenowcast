# Setup test data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df$age_group <- "00+"
rep_tri <- as_reporting_triangle(
  data = data_as_of_df,
  max_delay = 10,
  strata = "00+"
)

test_that("is_reporting_triangle works correctly", {
  expect_true(is_reporting_triangle(rep_tri))
  expect_false(is_reporting_triangle(matrix(1:10, 2, 5)))
  expect_false(is_reporting_triangle(data.frame(a = 1:5)))
  expect_false(is_reporting_triangle(list(a = 1:5)))
})

test_that("get_reference_dates works correctly", {
  ref_dates <- get_reference_dates(rep_tri)
  expect_s3_class(ref_dates, "Date")
  expect_length(ref_dates, nrow(rep_tri))
  expect_identical(ref_dates[1], as.Date("2025-10-25"))
  expect_identical(ref_dates[length(ref_dates)], as.Date("2026-04-01"))

  # Test error with non-reporting_triangle
  expect_error(
    get_reference_dates(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )
})

test_that("get_max_delay works correctly", {
  expect_identical(get_max_delay(rep_tri), 10L)
  expect_identical(get_max_delay(rep_tri, non_zero = FALSE), 10L)

  # Test error with non-reporting_triangle
  expect_error(
    get_max_delay(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )
})

test_that("get_max_delay with non_zero option works correctly", {
  # Create test triangle with trailing zeros
  test_mat <- matrix(c(
    100, 50, 20, 0, 0,
    80, 40, 10, 0, 0,
    90, 45, 15, 0, 0,
    70, 35, NA, NA, NA
  ), nrow = 4, byrow = TRUE)

  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 4)
  test_tri <- as_reporting_triangle(
    data = test_mat,
    reference_dates = ref_dates,
    max_delay = 4
  )

  expect_identical(get_max_delay(test_tri), 4L)
  expect_identical(get_max_delay(test_tri, non_zero = TRUE), 2L)

  # Test with all zeros
  zero_mat <- matrix(0, nrow = 3, ncol = 5)
  ref_dates_zero <- seq(as.Date("2025-01-01"), by = "day", length.out = 3)
  zero_tri <- as_reporting_triangle(
    data = zero_mat,
    reference_dates = ref_dates_zero,
    max_delay = 4
  )
  expect_identical(get_max_delay(zero_tri, non_zero = TRUE), -1L)
})

test_that("get_mean_delay works correctly", {
  mean_delays <- get_mean_delay(rep_tri)
  expect_type(mean_delays, "double")
  expect_length(mean_delays, nrow(rep_tri))

  # Check that values are in reasonable range
  non_na_delays <- mean_delays[!is.na(mean_delays)]
  expect_true(all(non_na_delays >= 0))
  expect_true(all(non_na_delays <= get_max_delay(rep_tri)))

  # Test error with non-reporting_triangle
  expect_error(
    get_mean_delay(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )

  # Test with known simple case
  simple_mat <- matrix(c(
    10, 5, 2,
    8, 4, NA
  ), nrow = 2, byrow = TRUE)

  ref_dates_simple <- seq(as.Date("2025-01-01"), by = "day", length.out = 2)
  simple_tri <- as_reporting_triangle(
    data = simple_mat,
    reference_dates = ref_dates_simple,
    max_delay = 2
  )

  mean_delays_simple <- get_mean_delay(simple_tri)
  # Row 1: (10*0 + 5*1 + 2*2) / (10 + 5 + 2) = 9/17 ≈ 0.529
  expect_equal(mean_delays_simple[1], (10 * 0 + 5 * 1 + 2 * 2) / 17, tol = 1e-6)
  # Row 2: (8*0 + 4*1) / (8 + 4) = 4/12 = 0.333
  expect_equal(mean_delays_simple[2], (8 * 0 + 4 * 1) / 12, tol = 1e-6)
})

test_that("head.reporting_triangle preserves class", {
  h <- suppressWarnings(head(rep_tri, n = 5))
  expect_true(is_reporting_triangle(h))
  expect_identical(nrow(h), 5L)
  expect_identical(ncol(h), ncol(rep_tri))

  # Default n = 6
  h_default <- suppressWarnings(head(rep_tri))
  expect_identical(nrow(h_default), 6L)
})

test_that("tail.reporting_triangle preserves class", {
  t <- suppressWarnings(tail(rep_tri, n = 5))
  expect_true(is_reporting_triangle(t))
  expect_identical(nrow(t), 5L)
  expect_identical(ncol(t), ncol(rep_tri))

  # Default n = 6
  t_default <- suppressWarnings(tail(rep_tri))
  expect_identical(nrow(t_default), 6L)
})

test_that("[.reporting_triangle preserves class and validates", {
  # Row subsetting
  sub_rows <- suppressWarnings(rep_tri[1:10, ])
  expect_true(is_reporting_triangle(sub_rows))
  expect_identical(nrow(sub_rows), 10L)

  # Column subsetting changes structure, should still preserve class
  sub_cols <- suppressWarnings(rep_tri[, 1:5])
  expect_true(is_reporting_triangle(sub_cols))
})

test_that("print.reporting_triangle runs without error", {
  # Just check it doesn't error
  expect_no_error(print(rep_tri))
  expect_invisible(print(rep_tri))
})

test_that("summary.reporting_triangle runs without error", {
  # Just check it doesn't error
  expect_no_error(summary(rep_tri))
  expect_invisible(summary(rep_tri))
})

test_that("get_quantile_delay works correctly", {
  quantile_delays <- get_quantile_delay(rep_tri, p = 0.99)
  expect_type(quantile_delays, "integer")
  expect_length(quantile_delays, nrow(rep_tri))

  # Check that values are in reasonable range
  non_na_delays <- quantile_delays[!is.na(quantile_delays)]
  expect_true(all(non_na_delays >= 0))
  expect_true(all(non_na_delays <= get_max_delay(rep_tri)))

  # Test with known simple case
  simple_mat <- matrix(c(
    10, 5, 2, 1, 1,
    8, 4, 2, NA, NA
  ), nrow = 2, byrow = TRUE)

  ref_dates_simple <- seq(as.Date("2025-01-01"), by = "day", length.out = 2)
  simple_tri <- as_reporting_triangle(
    data = simple_mat,
    reference_dates = ref_dates_simple,
    max_delay = 4
  )

  q99_delays <- get_quantile_delay(simple_tri, p = 0.99)
  # Row 1: cumsum = [10, 15, 17, 18, 19], total = 19, 99% = 18.81, so delay = 4
  # (need all 19 cases to reach 99%)
  expect_identical(q99_delays[1], 4L)

  # Test with different quantile
  q50_delays <- get_quantile_delay(simple_tri, p = 0.50)
  expect_type(q50_delays, "integer")

  # Test error with invalid p
  expect_error(
    get_quantile_delay(rep_tri, p = 1.5),
    "Assertion on 'p' failed"
  )
  expect_error(
    get_quantile_delay(rep_tri, p = -0.1),
    "Assertion on 'p' failed"
  )

  # Test error with non-reporting_triangle
  expect_error(
    get_quantile_delay(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )
})

test_that("as.data.frame.reporting_triangle works correctly", {
  df <- as.data.frame(rep_tri)

  # Check structure
  expect_s3_class(df, "data.frame")
  expect_true(all(c("reference_date", "report_date", "delay", "count") %in%
    names(df)))
  expect_identical(ncol(df), 4L)

  # Check that all reference dates are present
  unique_ref_dates <- unique(df$reference_date)
  expected_ref_dates <- get_reference_dates(rep_tri)
  expect_true(all(expected_ref_dates %in% unique_ref_dates))

  # Check that delays are in reasonable range
  expect_true(all(df$delay >= 0))
  expect_true(all(df$delay <= get_max_delay(rep_tri)))

  # Check that report_date = reference_date + delay
  expect_true(all(
    df$report_date == df$reference_date + df$delay
  ))

  # Check that NAs from triangle are excluded
  expect_false(anyNA(df$count))

  # Check that count values match
  for (i in seq_len(nrow(df))) {
    row_in_tri <- which(expected_ref_dates == df$reference_date[i])
    col_in_tri <- df$delay[i] + 1
    expect_identical(
      df$count[i],
      rep_tri[row_in_tri, col_in_tri]
    )
  }

  # Test error with non-reporting_triangle
  expect_error(
    as.data.frame.reporting_triangle(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )
})

test_that("truncate_to_quantile works correctly", {
  # Create test triangle with known reporting pattern
  test_mat <- matrix(c(
    100, 50, 25, 10, 5, 2, 1, 0, 0, 0,
    80, 40, 20, 10, 5, 2, 1, 0, 0, NA,
    90, 45, 22, 11, 5, 2, 1, 0, NA, NA,
    70, 35, 17, 8, 4, 2, NA, NA, NA, NA
  ), nrow = 4, byrow = TRUE)

  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 4)
  test_tri <- as_reporting_triangle(
    data = test_mat,
    reference_dates = ref_dates,
    max_delay = 9
  )

  # Test default (p = 0.99)
  result_99 <- truncate_to_quantile(test_tri)
  expect_true(is_reporting_triangle(result_99))
  expect_lte(get_max_delay(result_99), get_max_delay(test_tri))

  # Test with p = 0.90
  result_90 <- truncate_to_quantile(test_tri, p = 0.90)
  expect_true(is_reporting_triangle(result_90))
  expect_lte(get_max_delay(result_90), get_max_delay(result_99))

  # Test with p = 0.50
  result_50 <- truncate_to_quantile(test_tri, p = 0.50)
  expect_true(is_reporting_triangle(result_50))
  expect_lte(get_max_delay(result_50), get_max_delay(result_90))

  # Check that reference dates are preserved
  expect_equal(get_reference_dates(result_99), get_reference_dates(test_tri))
  expect_equal(get_reference_dates(result_50), get_reference_dates(test_tri))

  # Check that other attributes are preserved
  expect_identical(attr(result_99, "delays_unit"), attr(test_tri, "delays_unit"))
  expect_identical(attr(result_99, "structure"), attr(test_tri, "structure"))

  # Test error with invalid p
  expect_error(
    truncate_to_quantile(test_tri, p = 1.5),
    "Assertion on 'p' failed"
  )
  expect_error(
    truncate_to_quantile(test_tri, p = -0.1),
    "Assertion on 'p' failed"
  )

  # Test error with non-reporting_triangle
  expect_error(
    truncate_to_quantile(matrix(1:10)),
    "must have class 'reporting_triangle'"
  )
})

test_that("truncate_to_quantile handles edge cases", {
  # Test with triangle where no truncation is needed
  small_tri_mat <- matrix(c(
    100, 50, 25,
    80, 40, NA,
    90, NA, NA
  ), nrow = 3, byrow = TRUE)

  ref_dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 3)
  small_tri <- as_reporting_triangle(
    data = small_tri_mat,
    reference_dates = ref_dates,
    max_delay = 2
  )

  result <- suppressMessages(truncate_to_quantile(small_tri, p = 0.99))
  expect_identical(get_max_delay(result), get_max_delay(small_tri))
  expect_identical(ncol(result), ncol(small_tri))

  # Test with all zeros
  zero_mat <- matrix(0, nrow = 3, ncol = 5)
  ref_dates_zero <- seq(as.Date("2025-01-01"), by = "day", length.out = 3)
  zero_tri <- as_reporting_triangle(
    data = zero_mat,
    reference_dates = ref_dates_zero,
    max_delay = 4
  )

  result_zero <- suppressMessages(truncate_to_quantile(zero_tri, p = 0.99))
  expect_true(is_reporting_triangle(result_zero))
})
