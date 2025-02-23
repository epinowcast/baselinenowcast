test_that(".validate_delay_and_triangle works correctly", {
  # Test case 1: Valid inputs
  valid_triangle <- matrix(1:12, nrow = 3, ncol = 4)
  valid_delay_pmf <- c(0.4, 0.3, 0.2, 0.1)
  expect_no_error(.validate_delay_and_triangle(valid_triangle, valid_delay_pmf))

  # Test case 2: Non-integer values in triangle
  non_integer_triangle <- matrix(c(1, 2.5, 3, 4), nrow = 2, ncol = 2)
  expect_error(
    .validate_delay_and_triangle(non_integer_triangle, c(0.6, 0.4)),
    "Assertion on 'triangle' failed: Must be of type 'integerish'"
  )

  # Test case 3: Triangle is not a matrix
  not_matrix_triangle <- c(1, 2, 3, 4)
  expect_error(
    .validate_delay_and_triangle(not_matrix_triangle, c(0.6, 0.4)),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  )

  # Test case 4: Delay PMF is not numeric
  non_numeric_delay <- c("0.5", "0.5")
  expect_error(
    .validate_delay_and_triangle(valid_triangle, non_numeric_delay),
    "Assertion on 'delay_pmf' failed: Must inherit from class 'numeric'"
  )

  # Test case 5: Mismatch between triangle columns and delay PMF length
  mismatched_delay <- c(0.3, 0.3, 0.4)
  expect_error(
    .validate_delay_and_triangle(valid_triangle, mismatched_delay),
    "Length of the delay PMF is not the same as the number of delays"
  )

  # Test case 6: Empty triangle
  empty_triangle <- matrix(integer(0), nrow = 0, ncol = 0)
  expect_error(
    .validate_delay_and_triangle(empty_triangle, valid_delay_pmf),
    "Assertion on 'triangle' failed: Contains only missing values."
  )

  # Test case 7: Empty delay PMF
  empty_delay <- numeric(0)
  expect_error(
    .validate_delay_and_triangle(valid_triangle, empty_delay),
    "Length of the delay pmf is not the same as the number of delays"
  )
})
