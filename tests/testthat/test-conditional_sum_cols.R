test_that(".conditional_sum_cols works correctly", {
  # Test case 1: Basic functionality
  matrix_bool1 <- matrix(
    c(
      TRUE, FALSE,
      TRUE, TRUE
    ),
    nrow = 2,
    byrow = TRUE
  )
  matrix_bool2 <- matrix(
    c(
      TRUE, TRUE,
      FALSE, TRUE
    ),
    nrow = 2,
    byrow = TRUE
  )
  matrix_to_sum <- matrix(
    c(
      1, 2,
      3, 4
    ),
    nrow = 2,
    byrow = TRUE
  )


  expect_identical(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    1
  )
  expect_identical(
    .conditional_sum_cols(
      2,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    4
  )

  # Test case 2: Handling NAs (sums should return 0)
  matrix_to_sum <- matrix(
    c(
      1, 2,
      3, NA
    ),
    nrow = 2,
    byrow = TRUE
  )
  matrix_all_true <- matrix(data = TRUE, nrow = 2, ncol = 2)
  expect_identical(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    1
  )
  expect_identical(
    .conditional_sum_cols(
      2,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    0
  )
  expect_identical(
    .conditional_sum_cols(
      2,
      matrix_all_true,
      matrix_bool2,
      matrix_to_sum
    ),
    2
  )

  # Test case 3: All FALSE in boolean matrices
  matrix_bool1 <- matrix(FALSE, nrow = 2, ncol = 2)
  matrix_bool2 <- matrix(FALSE, nrow = 2, ncol = 2)
  matrix_to_sum <- matrix(c(1, 2, 3, 4), nrow = 2)

  expect_identical(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    0
  )
  expect_identical(
    .conditional_sum_cols(
      2,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    0
  )

  # Test case 4: Larger matrices
  matrix_bool1 <- matrix(
    c(
      TRUE, FALSE,
      TRUE, TRUE,
      TRUE, TRUE
    ),
    nrow = 3,
    byrow = TRUE
  )
  matrix_bool2 <- matrix(
    c(
      TRUE, TRUE,
      FALSE, TRUE,
      FALSE, TRUE
    ),
    nrow = 3,
    byrow = TRUE
  )
  matrix_to_sum <- matrix(
    c(
      1, 2,
      3, 4,
      5, 6
    ),
    nrow = 3,
    byrow = TRUE
  )

  expect_identical(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    1
  )
  expect_identical(
    .conditional_sum_cols(
      2,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    10
  )

  # Test case 5: Error handling for mismatched dimensions
  matrix_bool1 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2)
  matrix_bool2 <- matrix(c(TRUE, TRUE, FALSE), nrow = 3)
  matrix_to_sum <- matrix(c(1, 2, 3, 4), nrow = 2)

  expect_error(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    regexp = "Dimensions of boolean matrices are not the same"
  )
  matrix_bool1 <- matrix_bool2
  expect_error(
    .conditional_sum_cols(
      1,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    regexp = "Dimensions of boolean matrices and matrix to sum are not the same" # nolint
  ) # nolint


  # Test case 6: Column out of bounds
  matrix_bool1 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2)
  matrix_bool2 <- matrix(c(TRUE, TRUE, FALSE, TRUE), nrow = 2)
  matrix_to_sum <- matrix(c(1, 2, 3, 4), nrow = 2)

  expect_error(
    .conditional_sum_cols(
      3,
      matrix_bool1,
      matrix_bool2,
      matrix_to_sum
    ),
    regexp = "Column to sum is out of bounds of input matrices"
  )
})
