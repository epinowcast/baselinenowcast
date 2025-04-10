test_that(".validate_triangle works correctly", {
  # Test case 1: Valid inputs
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_no_error(.validate_triangle(valid_triangle,
    max_delay = 3,
    n = 4
  ))

  # Test case 2: Non-integer values in triangle
  non_integer_triangle <- matrix(c(1, 2.5, 3, 4), nrow = 2, ncol = 2)
  expect_error(
    .validate_triangle(non_integer_triangle,
      max_delay = 1,
      n = 1
    ),
    "Assertion on 'triangle' failed: Must be of type 'integerish'"
  )

  # Test case 3: Triangle is not a matrix
  not_matrix_triangle <- c(1, 2, 3, 4)
  expect_error(
    .validate_triangle(not_matrix_triangle,
      max_delay = 1,
      n = 1
    ),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  ) # nolint

  # Test case 4: Non-integer max_delay
  expect_error(
    .validate_triangle(valid_triangle,
      max_delay = 1.5,
      n = 1
    ),
    "Assertion on 'max_delay' failed: Must be of type 'integerish'"
  )

  # Test case 5: Non-integer n_history
  expect_error(
    .validate_triangle(valid_triangle,
      max_delay = 1,
      n = 1.5
    ),
    "Assertion on 'n' failed: Must be of type 'integerish'"
  )

  # Test case 6: Number of observations not greater than maximum delay
  triangle <- matrix(1:9, nrow = 3, ncol = 3)
  expect_error(
    .validate_triangle(triangle,
      max_delay = 2,
      n = 2
    )
  )

  # Test case 7: Number of observations less than n_history
  expect_error(
    .validate_triangle(valid_triangle,
      max_delay = 3,
      n = 6
    )
  )

  # Test case 8: Number of delays less than max_delay + 1
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 4, n = 4),
    "Number of delays in input data not sufficient"
  )

  # Test case 9: max_delay less than 1
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 0, n = 4),
    "Insufficient `max_delay` or `n`"
  )

  # Test case 10: n_history less than 1
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 3, n = 0),
    "Insufficient `max_delay` or `n`"
  )

  # Test case 11: Edge case - triangle with exactly enough rows and columns
  edge_triangle <- matrix(1:12, nrow = 4, ncol = 3)
  expect_no_error(.validate_triangle(edge_triangle,
    max_delay = 2,
    n = 4
  ))

  # Test case 12: matrix of all NAs
  na_triangle <- matrix(NA, nrow = 6, ncol = 4)
  expect_error(
    .validate_triangle(na_triangle,
      max_delay = 3,
      n = 5
    ),
    regexp = "Assertion on 'triangle' failed: Contains only missing values."
  )

  # Test case 13: triangle is too short
  short_triangle <- matrix(
    c(
      0, 5, 5, 5,
      0, 10, 10, NA,
      0, 20, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_error(
    .validate_triangle(short_triangle)
  )
})
