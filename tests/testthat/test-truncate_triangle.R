# Example matrix for testing
rep_tri <- matrix(
  c(
    10, 20, 30,
    40, 50, NA,
    70, NA, NA
  ),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)

# Test 1: Basic functionality
test_that("truncate_to_row works with positive t", {
  t <- 1
  result <- truncate_to_row(t, rep_tri)
  expected <- matrix(
    c(
      10, 20, 30,
      40, 50, NA
    ),
    nrow = 2,
    ncol = 3,
    byrow = TRUE
  )
  expect_identical(result, expected)
})

# Test 2: Edge case with t equal to nrow(rep_tri) fails
test_that("truncate_to_row throws an error when t is too large", {
  t <- nrow(rep_tri)
  expect_error(
    truncate_to_row(t, rep_tri),
    "The as of time point is greater than or equal to the number of"
  )
})

# Test 3: Negative t
test_that("truncate_to_row throws an error for a negative t", {
  t <- -1
  expect_error(
    truncate_to_row(t, rep_tri)
  )
})

# Test 4: Non-integer t
test_that("truncate_to_row throws an error for a non-integer t", {
  t <- 1.5
  expect_error(truncate_to_row(t, rep_tri))
})

# Test 5: Zero t
test_that("truncate_to_row handles zero t", {
  t <- 0L
  result <- truncate_to_row(t, rep_tri)
  expect_identical(result, rep_tri)
})

# Test 6: Empty matrix input throws an error
test_that("truncate_to_row handles empty matrix input", {
  matr_empty <- matrix(nrow = 0, ncol = 0)
  t <- 1
  expect_error(
    truncate_to_row(t, matr_empty),
    "The as of time point is greater than or equal to the number of"
  )
})
