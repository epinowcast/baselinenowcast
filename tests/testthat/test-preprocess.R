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
    expected_mat <- as.matrix(expected_fixed)
    attributes(result_mat) <- list(dim = dim(result_mat))
    attributes(expected_mat) <- list(dim = dim(expected_mat))
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
    result_mat <- as.matrix(result)
    attributes(result_mat) <- list(dim = dim(result_mat))
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
    expected_mat <- as.matrix(expected_neg_at_end)
    attributes(result_mat) <- list(dim = dim(result_mat))
    attributes(expected_mat) <- list(dim = dim(expected_mat))
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
    result_mat <- as.matrix(result)
    expected_mat <- as.matrix(expected_with_na)
    attributes(result_mat) <- list(dim = dim(result_mat))
    attributes(expected_mat) <- list(dim = dim(expected_mat))
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
