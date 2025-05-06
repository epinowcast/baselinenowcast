# Tests for true triangular NA patterns
test_that(".check_na_bottom_right accepts standard triangular pattern", {
  valid_matrix <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, NA,
      8, 9, NA, NA,
      10, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_true(.check_na_bottom_right(valid_matrix))
})

test_that(".check_na_bottom_right accepts matrix with no NAs", {
  no_na_matrix <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_true(.check_na_bottom_right(no_na_matrix))
})

# Tests for ragged patterns
test_that(".check_na_bottom_right accepts ragged NA pattern", {
  ragged_valid <- matrix(
    c(
      1, 2, 3, 4, 5,
      6, 7, 8, 9, NA,
      10, 11, 12, NA, NA,
      13, 14, NA, NA, NA,
      15, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_true(.check_na_bottom_right(ragged_valid))
})

# Tests for edge cases
test_that(".check_na_bottom_right accepts single element matrix", {
  single_element <- matrix(1, nrow = 1, ncol = 1)
  expect_true(.check_na_bottom_right(single_element))
})

test_that(".check_na_bottom_right accepts single element NA matrix", {
  single_na <- matrix(NA_real_, nrow = 1, ncol = 1)
  expect_true(.check_na_bottom_right(single_na))
})

test_that(".check_na_bottom_right accepts valid single row matrix", {
  row_matrix <- matrix(c(1, 2, 3, NA, NA), nrow = 1)
  expect_true(.check_na_bottom_right(row_matrix))
})

test_that(".check_na_bottom_right accepts valid single column matrix", {
  col_matrix <- matrix(c(1, 2, 3, NA, NA), ncol = 1)
  expect_true(.check_na_bottom_right(col_matrix))
})

# Tests for invalid patterns
test_that(".check_na_bottom_right rejects NA in top-left", {
  invalid_matrix1 <- matrix(
    c(
      NA, 2, 3, 4,
      5, 6, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_false(.check_na_bottom_right(invalid_matrix1))
})

test_that(".check_na_bottom_right rejects NA in middle", {
  invalid_matrix2 <- matrix(
    c(
      1, 2, 3, 4,
      5, NA, 7, 8,
      9, 10, 11, 12,
      13, 14, 15, 16
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_false(.check_na_bottom_right(invalid_matrix2))
})

test_that(".check_na_bottom_right rejects inconsistent NA pattern in rows", {
  invalid_matrix3 <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, NA, 8,
      9, 10, 11, NA,
      13, NA, 15, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_false(.check_na_bottom_right(invalid_matrix3))
})

test_that(".check_na_bottom_right rejects inconsistent NA pattern in columns", {
  invalid_matrix4 <- matrix(
    c(
      1, 2, 3, 4,
      5, 6, 7, NA,
      8, 9, NA, 12,
      10, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )
  expect_false(.check_na_bottom_right(invalid_matrix4))
})

test_that(".check_na_bottom_right rejects non-continuous NAs in single row", {
  invalid_row_matrix <- matrix(c(1, NA, 3, NA, 5), nrow = 1)
  expect_false(.check_na_bottom_right(invalid_row_matrix))
})

test_that(".check_na_bottom_right rejects non-continuous NAs in single column", {
  invalid_col_matrix <- matrix(c(1, NA, 3, NA, 5), ncol = 1)
  expect_false(.check_na_bottom_right(invalid_col_matrix))
})

test_that(".check_na_bottom_right rejects value where NA should be in ragged pattern", {
  ragged_invalid <- matrix(
    c(
      1, 2, 3, 4, 5,
      6, 7, 8, 9, NA,
      10, 11, 12, NA, NA,
      13, 14, NA, 20, NA,
      15, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )
  expect_false(.check_na_bottom_right(ragged_invalid))
})
