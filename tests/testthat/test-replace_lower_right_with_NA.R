test_that(
  "replace_lower_right_with_NA handles square matrix", {
  square_matrix <- matrix(
    1:16,
    nrow = 4,
    ncol = 4,
    byrow = TRUE
  )
  expected <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, NA,
      9, 10, NA, NA,
      13, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  result <- replace_lower_right_with_NA(square_matrix)
  expect_identical(result, expected)
})

test_that(
  "replace_lower_right_with_NA handles rectangular matrix with more rows", {
  rect_matrix <- matrix(
    1:20,
    nrow = 5,
    ncol = 4,
    byrow = TRUE
  )
  expected <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, NA,
      13, 14, NA, NA,
      17, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  result <- replace_lower_right_with_NA(rect_matrix)
  expect_identical(result, expected)
})

test_that(
  "replace_lower_right_with_NA handles rectangular matrix with more columns", {
  rect_matrix <- matrix(
    1:20,
    nrow = 4,
    ncol = 5,
    byrow = TRUE
  )
  expected <- matrix(
    c(
      1, 2, 3, 4, NA,
      6, 7, 8, NA, NA,
      11, 12, NA, NA, NA,
      16, NA, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  result <- replace_lower_right_with_NA(rect_matrix)
  expect_identical(result, expected)
})

test_that("replace_lower_right_with_NA leaves 1x1 matrix unchanged", {
  single_cell <- matrix(1, nrow = 1, ncol = 1)
  result <- replace_lower_right_with_NA(single_cell)
  expect_identical(result, single_cell)
})

test_that("replace_lower_right_with_NA handles 2x2 matrix", {
  two_by_two <- matrix(
    1:4,
    nrow = 2,
    ncol = 2,
    byrow = TRUE
  )
  expected <- matrix(
    c(1, 2, 3, NA),
    nrow = 2,
    byrow = TRUE
  )
  result <- replace_lower_right_with_NA(two_by_two)
  expect_identical(result, expected)
})

test_that("replace_lower_right_with_NA handles matrix with existing NAs", {
  na_matrix <- matrix(
    c(
      1, 2, NA,
      4, 5, 6,
      7, 8, 9
    ),
    nrow = 3,
    ncol = 3,
    byrow = TRUE
  )
  expected <- matrix(
    c(
      1, 2, NA,
      4, 5, NA,
      7, NA, NA
    ),
    nrow = 3,
    byrow = TRUE
  )
  result <- replace_lower_right_with_NA(na_matrix)
  expect_identical(result, expected)
})

test_that("replace_lower_right_with_NA handles one-row matrix", {
  one_row <- matrix(1:5, nrow = 1)
  expected <- matrix(
    c(1, NA, NA, NA, NA),
    nrow = 1
  )
  result <- replace_lower_right_with_NA(one_row)
  expect_identical(result, expected)
})

test_that("replace_lower_right_with_NA handles one-column matrix", {
  one_col <- matrix(1:5, ncol = 1)
  result <- replace_lower_right_with_NA(one_col)
  expect_identical(result, one_col)
})

test_that("replace_lower_right_with_NA does not modify the original matrix", {
  original <- matrix(1:9, nrow = 3)
  original_copy <- original
  result <- replace_lower_right_with_NA(original)
  expect_identical(original, original_copy)
  expect_false(identical(result, original))
})

test_that("replace_lower_right_with_NA handles ragged structure with integer", {
  test_matrix <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

  # Test with structure = 2
  expected_ragged <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, NA,
      6, 2, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  result_ragged <- replace_lower_right_with_NA(test_matrix, 2)
  expect_identical(result_ragged, expected_ragged)
})

test_that("replace_lower_right_with_NA handles custom structure with vector", {
  test_matrix <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, 15,
      3, 0, 0, 0, 0,
      6, 2, 0, 0, 0
    ),
    nrow = 5,
    byrow = TRUE
  )

  # Test with structure = c(1, 2, 1)
  expected_custom <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, 12,
      9, 10, 0, 0, NA,
      3, 0, 0, NA, NA,
      6, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  result_custom <- replace_lower_right_with_NA(test_matrix, c(1, 2, 1))
  expect_identical(result_custom, expected_custom)
})

test_that("replace_lower_right_with_NA validates structure parameter", {
  test_matrix <- matrix(1:9, nrow = 3)

  # Test with negative structure
  expect_error(replace_lower_right_with_NA(test_matrix, -1),
               "Structure must be positive")

  # Test with structure larger than columns
  expect_error(replace_lower_right_with_NA(test_matrix, 4),
               "Structure cannot be larger than number of columns")

  # Test with invalid vector structure
  expect_error(
    replace_lower_right_with_NA(test_matrix, c(1, 1, 1)),
    "Sum of structure vector must equal the number of columns in matrix"
  )
})
