# Common test matrices
triangle_with_neg_mat <- matrix(c(
  10, 5, -2, 3,
  8, -3, 4, 2,
  1, 6, 3, -1
), nrow = 3, byrow = TRUE)

triangle_with_neg <- as_reporting_triangle(triangle_with_neg_mat)

expected_fixed_mat <- matrix(c(
  10, 3, 0, 3,
  5, 0, 4, 2,
  1, 6, 2, 0
), nrow = 3, byrow = TRUE)

expected_fixed <- as_reporting_triangle(expected_fixed_mat)

test_that(
  "preprocess_negative_values correctly handles basic negative values",
  {
    result <- preprocess_negative_values(triangle_with_neg)
    result_mat <- as.matrix(result)
    result_mat <- as_plain_matrix(result)

    expected_mat <- as_plain_matrix(expected_fixed)
    expect_identical(result_mat, expected_mat)
  }
)

test_that(
  "preprocess_negative_values preserves matrices without negative values",
  {
    triangle_no_neg_mat <- matrix(c(
      5, 3, 2, 1,
      4, 3, 2, 1
    ), nrow = 2, byrow = TRUE)

    triangle_no_neg <- as_reporting_triangle(triangle_no_neg_mat)

    result <- preprocess_negative_values(triangle_no_neg)
    result_mat <- as_plain_matrix(result)
    expect_identical(result_mat, triangle_no_neg_mat)
  }
)

test_that(
  "preprocess_negative_values correctly handles negative values at end of rows", # nolint: line_length_linter
  {
    triangle_neg_at_end_mat <- matrix(c(
      10, 5, 3, -2,
      8, 4, -1, -3
    ), nrow = 2, byrow = TRUE)

    triangle_neg_at_end <- as_reporting_triangle(triangle_neg_at_end_mat)

    expected_neg_at_end_mat <- matrix(c(
      10, 5, 1, 0,
      8, 0, 0, 0
    ), nrow = 2, byrow = TRUE)

    expected_neg_at_end <- as_reporting_triangle(expected_neg_at_end_mat)

    result <- preprocess_negative_values(triangle_neg_at_end)
    result_mat <- as.matrix(result)
    result_mat <- as_plain_matrix(result)

    expected_mat <- as_plain_matrix(expected_neg_at_end)
    expect_identical(result_mat, expected_mat)
  }
)

test_that(
  "preprocess_negative_values correctly handles matrices with NA values",
  {
    triangle_with_na_mat <- matrix(c(
      10, 5, 1, -2,
      8, -3, 2, NA,
      -2, 3, NA, NA
    ), nrow = 3, byrow = TRUE)

    triangle_with_na <- as_reporting_triangle(triangle_with_na_mat)

    expected_with_na_mat <- matrix(c(
      10, 4, 0, 0,
      5, 0, 2, NA,
      0, 3, NA, NA
    ), nrow = 3, byrow = TRUE)

    expected_with_na <- as_reporting_triangle(expected_with_na_mat)

    result <- preprocess_negative_values(triangle_with_na)
    expect_true(is_reporting_triangle(result))
    result_mat <- as.matrix(result)
    result_mat <- as_plain_matrix(result)

    expected_mat <- as_plain_matrix(expected_with_na)
    expect_identical(result_mat, expected_mat)
  }
)

test_that("preprocess_negative_values returns a reporting_triangle with integer matrix", { # nolint: line_length_linter
  result <- preprocess_negative_values(triangle_with_neg)
  expect_s3_class(result, "reporting_triangle")
  result_mat <- as.matrix(result)
  expect_true(is.matrix(result_mat))
  expect_true(checkmate::check_integerish(result_mat))
})

test_that("preprocess_negative_values retains decimals when there are no negative values", { # nolint: line_length_linter
  triangle_dec_mat <- matrix(c(
    10.5, 5.25, 2.1, 1.7,
    8.3, 4.4, 2.9, NA,
    6.6, 3.2, NA, NA,
    7.9, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  triangle_dec <- as_reporting_triangle(triangle_dec_mat)

  result <- preprocess_negative_values(triangle_dec)
  result_mat <- as_plain_matrix(result)

  expect_identical(result_mat, triangle_dec_mat)
})

test_that("preprocess_negative_values retains decimals when redistributing negative values", { # nolint: line_length_linter
  triangle_dec_neg_mat <- matrix(c(
    10.5, 5.25, -2.5, 3.5,
    8.3, -3.1, 4.4, NA,
    6.6, 3.2, NA, NA,
    7.9, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  triangle_dec_neg <- as_reporting_triangle(triangle_dec_neg_mat)

  expected_mat <- matrix(c(
    10.5, 2.75, 0, 3.5,
    5.2, 0, 4.4, NA,
    6.6, 3.2, NA, NA,
    7.9, NA, NA, NA
  ), nrow = 4, byrow = TRUE)

  result <- suppressMessages(preprocess_negative_values(triangle_dec_neg))
  result_mat <- as_plain_matrix(result)

  expect_equal(result_mat, expected_mat, tolerance = 1e-10)
  expect_false(checkmate::test_integerish(result_mat))
})
