test_that(".validate_triangle accepts valid inputs", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_no_error(.validate_triangle(valid_triangle,
    max_delay = 3,
    n = 4
  ))
})

test_that(".validate_triangle accepts non-integer values in triangle", {
  non_integer_triangle <- matrix(c(1, 2.5, 3, 4), nrow = 2, ncol = 2)
  expect_no_error(
    .validate_triangle(non_integer_triangle,
      max_delay = 1,
      n = 2
    )
  )
})

test_that(".validate_triangle errors if `n` is too low", {
  valid_triangle <- matrix(
    c(
      80, 50, 10, 10,
      100, 40, 31, 20,
      95, 45, 21, NA,
      82, 42, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  # Errors if user specified n is too low
  expect_error(.validate_triangle(valid_triangle,
    max_delay = 3,
    n = 3
  ))
  # Doesn't error on default behavior
  expect_no_error(.validate_triangle(valid_triangle,
    max_delay = 3
  ))
})

test_that(".validate_triangle requires input to be a matrix", {
  not_matrix_triangle <- c(1, 2, 3, 4)
  expect_error(
    .validate_triangle(not_matrix_triangle,
      max_delay = 1,
      n = 1
    ),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  )
})

test_that(".validate_triangle requires integer max_delay", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle,
      max_delay = 1.5,
      n = 1
    ),
    "Assertion on 'max_delay' failed: Must be of type 'integerish'"
  )
})

test_that(".validate_triangle requires integer n_history", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle,
      max_delay = 1,
      n = 1.5
    ),
    "Assertion on 'n' failed: Must be of type 'integerish'"
  )
})

test_that(
  ".validate_triangle checks observations are not less than n_history",
  {
    valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
    expect_error(
      .validate_triangle(valid_triangle,
        max_delay = 3,
        n = 6
      )
    )
  }
)

test_that(".validate_triangle checks number of delays against max_delay", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 4, n = 4),
    "Number of delays in input data not sufficient"
  )
})

test_that(".validate_triangle requires max_delay to be at least 1", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 0, n = 4),
    "Insufficient `max_delay` or `n`"
  )
})

test_that(".validate_triangle requires n_history to be at least 1", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle, max_delay = 3, n = 0),
    "Insufficient `max_delay` or `n`"
  )
})

test_that(".validate_triangle accepts edge case with exact dimensions", {
  edge_triangle <- matrix(1:12, nrow = 4, ncol = 3)
  expect_no_error(.validate_triangle(edge_triangle,
    max_delay = 2,
    n = 4
  ))
})

test_that(".validate_triangle rejects matrix of all NAs", {
  na_triangle <- matrix(NA, nrow = 6, ncol = 4)
  expect_error(
    .validate_triangle(na_triangle,
      max_delay = 3,
      n = 5
    ),
    regexp = "Assertion on 'triangle' failed: Contains only missing values."
  )
})

test_that(".validate_triangle rejects matrix with empty columns", {
  # Create a matrix with a column that has only NA values
  empty_col_triangle <- matrix(
    c(
      1, 2, NA, 4,
      5, 6, NA, 8,
      9, 10, NA, 12,
      13, 14, NA, 16
    ),
    nrow = 4,
    byrow = TRUE
  )

  expect_error(
    .validate_triangle(empty_col_triangle,
      max_delay = 3,
      n = 4
    ),
    "Invalid reporting triangle structure. Each column must have"
  )
})
