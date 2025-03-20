# Example matrix for testing
matr_observed <- matrix(
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
test_that("truncate_triangle works with positive t", {
  t <- 1
  result <- truncate_triangle(t, matr_observed)
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

# Test 2: Edge case with t equal to nrow(matr_observed) fails
test_that("truncate_triangle throws an error when t is too large", {
  t <- nrow(matr_observed)
  expect_error(
    truncate_triangle(t, matr_observed),
    "The as of time point is greater than or equal to the number of"
  )
})

# Test 3: Negative t
test_that("truncate_triangle throws an error for a negative t", {
  t <- -1
  expect_error(
    truncate_triangle(t, matr_observed)
  )
})

# Test 4: Non-integer t
test_that("truncate_triangle throws an error for a non-integer t", {
  t <- 1.5
  expect_error(truncate_triangle(t, matr_observed))
})

# Test 5: Zero t
test_that("truncate_triangle handles zero t", {
  t <- 0L
  result <- truncate_triangle(t, matr_observed)
  expect_identical(result, matr_observed)
})

# Test 6: Empty matrix input throws an error
test_that("truncate_triangle handles empty matrix input", {
  matr_empty <- matrix(nrow = 0, ncol = 0)
  t <- 1
  expect_error(
    truncate_triangle(t, matr_empty),
    "The as of time point is greater than or equal to the number of"
  )
})
