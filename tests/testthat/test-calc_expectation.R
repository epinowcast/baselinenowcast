test_that("calc_expectation function works correctly", {
  # Setup
  n_dates <- 10
  n_delays <- 5
  expectation <- matrix(1:50, nrow = n_dates, ncol = n_delays)
  delay_pmf <- c(0.5, 0.3, 0.1, 0.07, 0.03)

  # Test 1: Basic functionality
  result <- .calc_expectation(3, expectation, delay_pmf)
  expect_is(result, "matrix")
  expect_identical(dim(result), as.integer(c(n_dates, n_delays)))

  # Test 2: Check if the function modifies the correct cells
  original <- expectation
  modified <- .calc_expectation(3, expectation, delay_pmf)
  expect_equal(modified[1:8, ], original[1:8, ], tolerance = 1e-6)
  expect_false(all(modified[9:10, 3] == original[9:10, 3]))

  # Test 3: Check if the calculation is correct
  block_bottom_left <- expectation[9:10, 1:2]
  exp_total <- (rowSums(block_bottom_left) + 1 - sum(delay_pmf[1:2])) / sum(delay_pmf[1:2]) # nolint
  expected_values <- exp_total * delay_pmf[3]
  expect_identical(modified[9:10, 3], expected_values)

  # Test 4: Edge case - when co is 2
  result_edge <- .calc_expectation(2, expectation, delay_pmf)
  expect_identical(dim(result_edge), as.integer(c(n_dates, n_delays)))

  # Test 5: Edge case - when nrows < ncols
  short_triangle <- matrix(
    c(
      80, 40, NA, NA, NA,
      70, NA, NA, NA, NA
    ),
    nrow = 2,
    byrow = TRUE
  )
  # Test out iterative updating of rows
  first_result <- .calc_expectation(2, short_triangle, delay_pmf)
  second_result <- .calc_expectation(3, first_result, delay_pmf)
  third_result <- .calc_expectation(4, second_result, delay_pmf)
  last_result <- .calc_expectation(5, third_result, delay_pmf)
  # Test no more NAs in final result
  expect_false(anyNA(last_result))


  # Test 6: Error handling - invalid co
  expect_error(.calc_expectation(1, expectation, delay_pmf))
  expect_error(.calc_expectation(n_delays + 1, expectation, n_dates, delay_pmf))
})
