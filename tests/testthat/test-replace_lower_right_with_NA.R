test_that(".replace_lower_right_with_NA works correctly", {
  # Test case 1: Square matrix
  square_matrix <- matrix(1:16, nrow = 4, ncol = 4, byrow = TRUE)
  expected_square <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, NA,
      9, 10, NA, NA,
      13, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_equal(.replace_lower_right_with_NA(square_matrix),
    expected_square,
    tol = 1e-6
  )

  # Test case 2: Rectangular matrix (more rows than columns)
  rect_matrix1 <- matrix(1:20, nrow = 5, ncol = 4, byrow = TRUE)
  expected_rect1 <- matrix(c(
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, NA,
    13, 14, NA, NA,
    17, NA, NA, NA
  ), nrow = 5, byrow = TRUE)
  expect_equal(.replace_lower_right_with_NA(rect_matrix1),
    expected_rect1,
    tol = 1e-6
  )

  # Test case 3: Rectangular matrix (more columns than rows)
  rect_matrix2 <- matrix(1:20, nrow = 4, ncol = 5, byrow = TRUE)
  expected_rect2 <- matrix(c(
    1, 2, 3, 4, NA,
    6, 7, 8, NA, NA,
    11, 12, NA, NA, NA,
    16, NA, NA, NA, NA
  ), nrow = 4, byrow = TRUE)
  expect_equal(.replace_lower_right_with_NA(rect_matrix2),
    expected_rect2,
    tol = 1e-6
  )

  # Test case 4: 1x1 matrix
  single_cell <- matrix(1, nrow = 1, ncol = 1)
  expect_equal(.replace_lower_right_with_NA(single_cell),
    single_cell,
    tol = 1e-6
  )

  # Test case 5: 2x2 matrix
  two_by_two <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
  expected_two_by_two <- matrix(c(1, 2, 3, NA), nrow = 2, byrow = TRUE)
  expect_equal(.replace_lower_right_with_NA(two_by_two),
    expected_two_by_two,
    tol = 1e-6
  )

  # Test case 6: Matrix with NAs
  na_matrix <- matrix(c(
    1, 2, NA,
    4, 5, 6,
    7, 8, 9
  ), nrow = 3, ncol = 3, byrow = TRUE)
  expected_na <- matrix(c(
    1, 2, NA,
    4, 5, NA,
    7, NA, NA
  ), nrow = 3, byrow = TRUE)
  expect_equal(.replace_lower_right_with_NA(na_matrix),
    expected_na,
    tol = 1e-6
  )


  # Test case 7: Matrix with one row
  one_row <- matrix(1:5, nrow = 1)
  expected_one_row <- matrix(c(1, NA, NA, NA, NA), nrow = 1)
  expect_equal(.replace_lower_right_with_NA(one_row),
    expected_one_row,
    tol = 1e-6
  )

  # Test case 8: Matrix with one column
  one_col <- matrix(1:5, ncol = 1)
  expected_one_col <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
  expect_equal(.replace_lower_right_with_NA(one_col),
    expected_one_col,
    tol = 1e-6
  )

  # Test case 10: Ensure original matrix is not modified
  original <- matrix(1:9, nrow = 3)
  original_copy <- original
  result <- .replace_lower_right_with_NA(original)
  expect_identical(original, original_copy)
  expect_false(identical(result, original))
})
