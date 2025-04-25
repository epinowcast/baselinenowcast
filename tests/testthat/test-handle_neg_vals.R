# Common test matrices
triangle_with_neg <- matrix(c(
  10, 5, -2, 3,
  8, -3, 4, 2,
  1, 6, 3, -1
), nrow = 3, byrow = TRUE)

expected_fixed <- matrix(c(
  10, 3, 0, 3,
  5, 0, 4, 2,
  1, 6, 2, 0
), nrow = 3, byrow = TRUE)

test_that(".handle_neg_vals correctly handles basic negative values", {
  result <- .handle_neg_vals(triangle_with_neg)
  expect_identical(result, expected_fixed)
})

test_that(".handle_neg_vals preserves matrices without negative values", {
  triangle_no_neg <- matrix(c(
    5, 3, 2, 1,
    4, 3, 2, 1
  ), nrow = 2, byrow = TRUE)

  result <- .handle_neg_vals(triangle_no_neg)
  expect_identical(result, triangle_no_neg)
})

test_that(".handle_neg_vals correctly handles negative values at end of rows", {
  triangle_neg_at_end <- matrix(c(
    10, 5, 3, -2,
    8, 4, -1, -3
  ), nrow = 2, byrow = TRUE)

  expected_neg_at_end <- matrix(c(
    10, 5, 1, 0,
    8, 0, 0, 0
  ), nrow = 2, byrow = TRUE)

  result <- .handle_neg_vals(triangle_neg_at_end)
  expect_identical(result, expected_neg_at_end)
})

test_that(".handle_neg_vals correctly handles matrices with NA values", {
  triangle_with_na <- matrix(c(
    10, 5, NA, -2,
    8, -3, NA, 2
  ), nrow = 2, byrow = TRUE)

  expected_with_na <- matrix(c(
    10, 3, NA, 0,
    5, 0, NA, 2
  ), nrow = 2, byrow = TRUE)

  result <- .handle_neg_vals(triangle_with_na)
  expect_identical(result, expected_with_na)
})

test_that(".handle_neg_vals returns an integer matrix", {
  result <- .handle_neg_vals(triangle_with_neg)
  expect_true(is.matrix(result))
  expect_true(checkmate::check_integerish(result))
})
