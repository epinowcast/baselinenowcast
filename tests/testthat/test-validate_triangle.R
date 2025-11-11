test_that(".validate_triangle accepts valid inputs", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_no_error(.validate_triangle(valid_triangle,
    n = 4
  ))
})

test_that(".validate_triangle accepts non-integer values in triangle", {
  non_integer_triangle <- matrix(c(1, 2.5, 3, 4), nrow = 2, ncol = 2)
  expect_no_error(
    .validate_triangle(non_integer_triangle,
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
    n = 3
  ))
  # Doesn't error on default behavior
  expect_no_error(.validate_triangle(valid_triangle))
})

test_that(".validate_triangle requires input to be a matrix", {
  not_matrix_triangle <- c(1, 2, 3, 4)
  expect_error(
    .validate_triangle(not_matrix_triangle,
      n = 1
    ),
    "Assertion on 'triangle' failed: Must inherit from class 'matrix'"
  )
})

test_that(".validate_triangle requires integer n_history", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle,
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
        n = 6
      )
    )
  }
)

test_that(".validate_triangle requires n_history to be at least 1", {
  valid_triangle <- matrix(1:20, nrow = 5, ncol = 4)
  expect_error(
    .validate_triangle(valid_triangle, n = 0),
    "Insufficient"
  )
})

test_that(".validate_triangle accepts edge case with exact dimensions", {
  edge_triangle <- matrix(1:12, nrow = 4, ncol = 3)
  expect_no_error(.validate_triangle(edge_triangle,
    n = 4
  ))
})

test_that(".validate_triangle rejects matrix of all NAs", {
  na_triangle <- matrix(NA, nrow = 6, ncol = 4)
  expect_error(
    .validate_triangle(na_triangle,
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
      n = 4
    ),
    "Invalid reporting triangle structure. Each column must have"
  )
})

test_that(".validate_triangle errors with appropriate name of arg", {
  data <- NULL
  expect_error(.validate_triangle(data),
    regexp = "`data` argument is missing."
  )
})

test_that(".validate_triangle errors when only 0s in LHS of NAs", {
  invalid_mat <- matrix(
    c(
      0, 0, 1, 2, 3,
      0, 0, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 2
  )
  expect_error(.validate_triangle(invalid_mat,
    n = 2
  ))
  invalid_mat2 <- matrix(
    c(
      0, 0, 4, 5, 6,
      0, 0, 1, 2, 3,
      0, 0, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_error(.validate_triangle(invalid_mat2,
    n = 3
  ))
  invalid_mat3 <- matrix(
    c(
      0, 1, 3,
      0, 4, NA,
      0, NA, NA
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_error(.validate_triangle(invalid_mat3,
    n = 3
  ))
})

test_that(".validate_triangle doesn't error when matrix is valid", {
  valid_mat <- matrix(
    c(
      0, 0, 1, 2, 3,
      1, 0, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 2
  )
  expect_no_error(.validate_triangle(valid_mat))
  valid_mat2 <- matrix(
    c(
      0, 0, 1, 2, 3,
      0, 1, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 2
  )
  expect_no_error(.validate_triangle(valid_mat2))
  valid_mat3 <- matrix(
    c(
      0, 0, 4, 5, 6,
      0, 0, 1, 2, 3,
      0, 0, 1, 2, 3
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_no_error(.validate_triangle(valid_mat3))
  valid_mat4 <- matrix(
    c(
      4, 1, 4, 5, 6,
      0, 0, 1, 2, 3,
      0, 0, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_no_error(.validate_triangle(valid_mat4))
})

test_that(".validate_triangle errors appropriately based on rows used in matrix", { # nolint
  could_be_valid_mat <- matrix(
    c(
      1, 4, 5, 7, 1,
      0, 0, 1, 2, 3,
      0, 0, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_no_error(.validate_triangle(could_be_valid_mat,
    n = 3
  ))
  expect_error(.validate_triangle(could_be_valid_mat,
    n = 2
  ))
  could_be_valid_mat2 <- matrix(
    c(
      1, 4, 5, 6,
      0, 2, 5, 4,
      0, 6, 7, NA,
      0, 10, NA, NA,
      0, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_error(.validate_triangle(could_be_valid_mat2,
    n = 4
  ))
  expect_no_error(.validate_triangle(could_be_valid_mat2,
    n = 5
  ))
})

test_that("valide_triangle errors if first NA is in the first column", {
  invalid_mat5 <- matrix(
    c(
      4, 2, 4, 5, 6,
      1, 3, 1, 2, 3,
      NA, NA, NA, NA, NA
    ),
    byrow = TRUE,
    nrow = 3
  )
  expect_error(.validate_triangle(invalid_mat5))
})
