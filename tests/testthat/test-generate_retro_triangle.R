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
test_that("generate_retro_triangle works with positive t", {
  t <- 1
  result <- generate_retro_triangle(t, matr_observed)
  expected <- matrix(
    c(
      10, 20, 30,
      40, 50, 60
    ),
    nrow = 2,
    ncol = 3,
    byrow = TRUE
  )
  expected <- .replace_lower_right_with_NA(expected)
  expect_identical(result, expected)
})

# Test 2: Edge case with t equal to nrow(matr_observed) fails
test_that("generate_retro_triangle throws an error when t is too large", {
  t <- nrow(matr_observed)
  expect_error(
    generate_retro_triangle(t, matr_observed),
    "The as of time point is greater than or equal to the number of"
  )
})

# Test 3: Negative t
test_that("generate_retro_triangle throws an error for a negative t", {
  t <- -1
  expect_error(
    generate_retro_triangle(t, matr_observed),
    "t must be a non-negative integer"
  )
})

# Test 4: Non-integer t
test_that("generate_retro_triangle throws an error for a non-integer t", {
  t <- 1.5
  expect_error(generate_retro_triangle(t, matr_observed))
})

# Test 5: Zero t
test_that("generate_retro_triangle handles zero t", {
  t <- 0L
  result <- generate_retro_triangle(t, matr_observed)
  expect_identical(result, .replace_lower_right_with_NA(matr_observed))
})

# Test 6: Empty matrix input throws an error
test_that("generate_retro_triangle handles empty matrix input", {
  matr_empty <- matrix(nrow = 0, ncol = 0)
  t <- 1
  expect_error(
    generate_retro_triangle(t, matr_empty),
    "The as of time point is greater than or equal to the number of"
  )
})
