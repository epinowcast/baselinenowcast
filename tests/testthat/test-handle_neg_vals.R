test_that(".handle_neg_vals function works correctly", {
  # Test case 1: Basic functionality
  triangle1 <- matrix(c(
    10, 5, -2, 3,
    8, -3, 4, 2,
    1, 6, 3, -1
  ), nrow = 3, byrow = TRUE)

  expected1 <- matrix(c(
    10, 3, 0, 3,
    5, 0, 4, 2,
    1, 6, 2, 0
  ), nrow = 3, byrow = TRUE)

  result1 <- .handle_neg_vals(triangle1)
  expect_identical(result1, expected1)

  # Test case 2: No negative values
  triangle2 <- matrix(c(
    5, 3, 2, 1,
    4, 3, 2, 1
  ), nrow = 2, byrow = TRUE)

  result2 <- .handle_neg_vals(triangle2)
  expect_identical(result2, triangle2)

  # Test case 3: Negative values at the end of rows
  triangle3 <- matrix(c(
    10, 5, 3, -2,
    8, 4, -1, -3
  ), nrow = 2, byrow = TRUE)

  expected3 <- matrix(c(
    10, 5, 1, 0,
    8, 0, 0, 0
  ), nrow = 2, byrow = TRUE)

  result3 <- .handle_neg_vals(triangle3)
  expect_identical(result3, expected3)

  # Test case 4: NA values
  triangle4 <- matrix(c(
    10, 5, NA, -2,
    8, -3, NA, 2
  ), nrow = 2, byrow = TRUE)

  expected4 <- matrix(c(
    10, 3, NA, 0,
    5, 0, NA, 2
  ), nrow = 2, byrow = TRUE)

  result4 <- .handle_neg_vals(triangle4)
  expect_identical(result4, expected4)

  # Test case 5: Output is integer matrix
  result5 <- .handle_neg_vals(triangle1)
  expect_true(is.matrix(result5))
  expect_true(checkmate::check_integerish(result5))
})
