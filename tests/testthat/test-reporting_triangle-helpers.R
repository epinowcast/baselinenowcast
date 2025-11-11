# Setup test data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df$age_group <- "00+"
# Filter to max_delay of 10 by computing delay
data_as_of_df$delay <- as.numeric(
  data_as_of_df$report_date - data_as_of_df$reference_date
)
data_as_of_df <- data_as_of_df[data_as_of_df$delay <= 10, ]
rep_tri <- as_reporting_triangle(
  data = data_as_of_df
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
    reference_dates = ref_dates
  )

  expect_identical(get_max_delay(test_tri), 4L)
  expect_identical(get_max_delay(test_tri, non_zero = TRUE), 2L)

  # Test with all zeros
  zero_mat <- matrix(0, nrow = 3, ncol = 5)
  ref_dates_zero <- seq(as.Date("2025-01-01"), by = "day", length.out = 3)
  zero_tri <- as_reporting_triangle(
    data = zero_mat,
    reference_dates = ref_dates_zero
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
    reference_dates = ref_dates_simple
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

test_that("[.reporting_triangle row subsetting works correctly", {
  # Basic row subsetting
  sub <- rep_tri[1:5, ]
  expect_true(is_reporting_triangle(sub))
  expect_identical(nrow(sub), 5L)
  expect_identical(ncol(sub), ncol(rep_tri))
  expect_identical(attr(sub, "delays_unit"), attr(rep_tri, "delays_unit"))

  # Single row subsetting (should preserve as matrix with drop=FALSE)
  single_row <- rep_tri[1, , drop = FALSE]
  expect_true(is_reporting_triangle(single_row))
  expect_identical(nrow(single_row), 1L)

  # Extract single row as vector (drop=TRUE, default)
  single_row_vec <- rep_tri[1, ]
  expect_false(is_reporting_triangle(single_row_vec))
  expect_true(is.numeric(single_row_vec))
})

test_that("[.reporting_triangle column subsetting works correctly", {
  # Basic column subsetting
  sub <- rep_tri[, 1:3]
  expect_true(is_reporting_triangle(sub))
  expect_identical(ncol(sub), 3L)
  expect_identical(nrow(sub), nrow(rep_tri))
  expect_identical(attr(sub, "delays_unit"), attr(rep_tri, "delays_unit"))

  # Single column subsetting (should preserve as matrix with drop=FALSE)
  single_col <- rep_tri[, 1, drop = FALSE]
  expect_true(is_reporting_triangle(single_col))
  expect_identical(ncol(single_col), 1L)

  # Extract single column as vector (drop=TRUE, default)
  single_col_vec <- rep_tri[, 1]
  expect_false(is_reporting_triangle(single_col_vec))
  expect_true(is.numeric(single_col_vec))
})

test_that("[.reporting_triangle combined subsetting works correctly", {
  # Row and column subsetting
  sub <- rep_tri[1:10, 1:3]
  expect_true(is_reporting_triangle(sub))
  expect_identical(nrow(sub), 10L)
  expect_identical(ncol(sub), 3L)
  expect_identical(attr(sub, "delays_unit"), attr(rep_tri, "delays_unit"))

  # Extract single element
  element <- rep_tri[1, 1]
  expect_false(is_reporting_triangle(element))
  expect_true(is.numeric(element))
  expect_length(element, 1L)
})

test_that("[.reporting_triangle validates result structure", {
  # Create a triangle with valid structure
  mat <- matrix(c(
    10, 20, 30, 40,
    15, 25, 35, NA,
    20, 30, NA, NA,
    25, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  rt <- as_reporting_triangle(data = mat)

  # Subsetting that maintains valid structure should work
  expect_no_error(rt[1:3, ])
  expect_no_error(rt[, 1:3])
  expect_no_error(rt[1:2, 1:2])

  # Create invalid structure by directly modifying underlying matrix
  mat_invalid <- unclass(rt)
  mat_invalid[2, 2] <- NA
  class(mat_invalid) <- c("reporting_triangle", "matrix")
  attr(mat_invalid, "delays_unit") <- "days"

  # This should fail validation when we try to subset it
  expect_error(mat_invalid[1:3, ], "Invalid reporting triangle structure")
})

test_that("[.reporting_triangle preserves reference dates", {
  # Row subsetting should preserve correct reference dates
  sub <- rep_tri[1:5, ]
  ref_dates_original <- get_reference_dates(rep_tri)
  ref_dates_sub <- get_reference_dates(sub)

  expect_identical(ref_dates_sub, ref_dates_original[1:5])

  # Non-contiguous subsetting
  indices <- c(1, 3, 5, 7, 9)
  sub_noncontig <- rep_tri[indices, ]
  ref_dates_noncontig <- get_reference_dates(sub_noncontig)

  expect_identical(ref_dates_noncontig, ref_dates_original[indices])
})

test_that("[<-.reporting_triangle assignment works correctly", {
  # Create test triangle
  mat <- matrix(c(
    10, 20, 30, 40,
    15, 25, 35, NA,
    20, 30, NA, NA,
    25, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  rt <- as_reporting_triangle(data = mat)

  # Single element assignment
  rt_modified <- rt
  rt_modified[1, 1] <- 100
  expect_true(is_reporting_triangle(rt_modified))
  expect_identical(rt_modified[1, 1], 100)

  # Row assignment
  rt_modified <- rt
  rt_modified[1, ] <- c(100, 200, 300, 400)
  expect_true(is_reporting_triangle(rt_modified))
  expect_identical(rt_modified[1, 1], 100)
  expect_identical(rt_modified[1, 2], 200)

  # Column assignment
  rt_modified <- rt
  rt_modified[, 1] <- c(100, 200, 300, 400)
  expect_true(is_reporting_triangle(rt_modified))
  expect_identical(rt_modified[1, 1], 100)
  expect_identical(rt_modified[2, 1], 200)

  # Subset assignment
  rt_modified <- rt
  rt_modified[1:2, 1:2] <- matrix(c(100, 200, 300, 400), nrow = 2)
  expect_true(is_reporting_triangle(rt_modified))
  expect_identical(rt_modified[1, 1], 100)
  expect_identical(rt_modified[2, 2], 400)
})

test_that("[<-.reporting_triangle preserves attributes", {
  # Create test triangle
  mat <- matrix(c(
    10, 20, 30, 40,
    15, 25, 35, NA,
    20, 30, NA, NA,
    25, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  rt <- as_reporting_triangle(data = mat)

  original_delays_unit <- attr(rt, "delays_unit")
  original_ref_dates <- get_reference_dates(rt)

  # Modify values
  rt[1, 1] <- 100

  # Check attributes preserved
  expect_identical(attr(rt, "delays_unit"), original_delays_unit)
  expect_identical(get_reference_dates(rt), original_ref_dates)
})

test_that("[<-.reporting_triangle validates result structure", {
  # Create test triangle
  mat <- matrix(c(
    10, 20, 30, 40,
    15, 25, 35, NA,
    20, 30, NA, NA,
    25, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  rt <- as_reporting_triangle(data = mat)

  # Valid modifications should work
  rt_valid <- rt
  rt_valid[1, 1] <- 100
  expect_no_error(validate_reporting_triangle(rt_valid))

  # Invalid modification: creating out-of-pattern NA should fail
  expect_error({
    rt_invalid <- rt
    rt_invalid[2, 2] <- NA
  }, "Invalid reporting triangle structure")
})

test_that("[<-.reporting_triangle allows NA in valid positions", {
  # Create test triangle
  mat <- matrix(c(
    10, 20, 30, 40,
    15, 25, 35, 50,
    20, 30, 60, 70,
    25, 80, 90, 100
  ), nrow = 4, byrow = TRUE)
  rt <- as_reporting_triangle(data = mat)

  # Adding NA in bottom-right pattern should work
  rt[4, 4] <- NA
  expect_true(is_reporting_triangle(rt))
  expect_true(is.na(rt[4, 4]))

  rt[3, 4] <- NA
  expect_true(is_reporting_triangle(rt))
  expect_true(is.na(rt[3, 4]))

  rt[4, 3] <- NA
  expect_true(is_reporting_triangle(rt))
  expect_true(is.na(rt[4, 3]))
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
    reference_dates = ref_dates_simple
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
    reference_dates = ref_dates
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

  # Check that delays_unit attribute is preserved
  expect_identical(attr(result_99, "delays_unit"), attr(test_tri, "delays_unit"))

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
    reference_dates = ref_dates
  )

  result <- suppressMessages(truncate_to_quantile(small_tri, p = 0.99))
  expect_identical(get_max_delay(result), get_max_delay(small_tri))
  expect_identical(ncol(result), ncol(small_tri))

  # Test with all zeros
  zero_mat <- matrix(0, nrow = 3, ncol = 5)
  ref_dates_zero <- seq(as.Date("2025-01-01"), by = "day", length.out = 3)
  zero_tri <- as_reporting_triangle(
    data = zero_mat,
    reference_dates = ref_dates_zero
  )

  result_zero <- suppressMessages(truncate_to_quantile(zero_tri, p = 0.99))
  expect_true(is_reporting_triangle(result_zero))
})

test_that("check_na_pattern detects expected triangular pattern", {
  # Create a triangle with normal triangular pattern
  mat <- matrix(
    c(
      10, 20, 30, 40,
      15, 25, 35, NA,
      20, 30, NA, NA,
      25, NA, NA, NA
    ),
    nrow = 4, byrow = TRUE
  )
  rt <- as_reporting_triangle(data = mat)

  result <- baselinenowcast:::.check_na_pattern(rt)

  expect_true(result$valid)
  expect_identical(result$n_out_of_pattern, 0L)
  expect_identical(result$n_expected, 6L)
  expect_identical(length(result$rows_affected), 0L)
  expect_false(any(result$positions))
})

test_that("check_na_pattern detects out-of-pattern NAs", {
  # Create a valid triangle first, then modify underlying matrix
  mat <- matrix(
    c(
      10, 20, 30, 40,
      15, 25, 35, NA,
      20, 30, NA, NA,
      25, NA, NA, NA
    ),
    nrow = 4, byrow = TRUE
  )
  # Modify matrix before converting to reporting_triangle to create invalid pattern
  mat[2, 2] <- NA

  result <- baselinenowcast:::.check_na_pattern(mat)

  # One out-of-pattern NA at [2,2] (has data to the right and below)
  expect_false(result$valid)
  expect_identical(result$n_out_of_pattern, 1L)
  expect_identical(result$n_expected, 6L)
  expect_identical(length(result$rows_affected), 1L)
  expect_identical(result$rows_affected, 2L)
  expect_true(result$positions[2, 2])
})

test_that("check_na_pattern detects NA with data below", {
  # Create matrix with out-of-pattern NA
  mat <- matrix(
    c(
      10, 20, 30, 40,
      15, 25, 35, NA,
      20, 30, NA, NA,
      25, NA, NA, NA
    ),
    nrow = 4, byrow = TRUE
  )
  # Modify to create out-of-pattern NA
  mat[1, 3] <- NA

  result <- baselinenowcast:::.check_na_pattern(mat)

  # Out-of-pattern at [1,3] (has data at [2,3])
  expect_false(result$valid)
  expect_identical(result$n_out_of_pattern, 1L)
  expect_true(result$positions[1, 3])
  expect_identical(result$rows_affected, 1L)
})

test_that("check_na_pattern detects NA with data to the right", {
  # Create matrix with out-of-pattern NAs
  mat <- matrix(
    c(
      10, 20, 30, 40,
      15, 25, 35, 50,
      20, 30, NA, NA,
      25, NA, NA, NA
    ),
    nrow = 4, byrow = TRUE
  )
  # Modify to create out-of-pattern NAs
  mat[2, 2] <- NA
  mat[2, 3] <- NA

  result <- baselinenowcast:::.check_na_pattern(mat)

  # Out-of-pattern at [2,2] and [2,3] (have data at [2,4])
  expect_false(result$valid)
  expect_identical(result$n_out_of_pattern, 2L)
  expect_true(result$positions[2, 2])
  expect_true(result$positions[2, 3])
  expect_identical(result$rows_affected, 2L)
})

test_that("check_na_pattern handles complete triangle", {
  # Create a triangle with no NAs
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  rt <- as_reporting_triangle(data = mat)

  result <- baselinenowcast:::.check_na_pattern(rt)

  expect_true(result$valid)
  expect_identical(result$n_out_of_pattern, 0L)
  expect_identical(result$n_expected, 0L)
  expect_identical(length(result$rows_affected), 0L)
  expect_false(any(result$positions))
})

test_that("check_na_pattern handles multiple affected rows", {
  # Create matrix with out-of-pattern NAs in multiple rows
  mat <- matrix(
    c(
      10, 20, 30, 40,
      15, 25, 35, 50,
      20, 30, NA, NA,
      25, NA, NA, NA
    ),
    nrow = 4, byrow = TRUE
  )
  # Modify to create out-of-pattern NAs in multiple rows
  mat[1, 2] <- NA
  mat[2, 3] <- NA

  result <- baselinenowcast:::.check_na_pattern(mat)

  # Out-of-pattern at [1,2] (data to right) and [2,3] (data to right)
  expect_false(result$valid)
  expect_identical(result$n_out_of_pattern, 2L)
  expect_identical(length(result$rows_affected), 2L)
  expect_true(all(c(1, 2) %in% result$rows_affected))
})

test_that("check_na_pattern works with plain matrix", {
  # Internal function should work with plain matrices too
  mat <- matrix(
    c(
      10, 20, 30,
      15, 25, NA,
      20, NA, NA
    ),
    nrow = 3, byrow = TRUE
  )

  result <- baselinenowcast:::.check_na_pattern(mat)

  expect_true(result$valid)
  expect_identical(result$n_out_of_pattern, 0L)
  expect_identical(result$n_expected, 3L)
})
