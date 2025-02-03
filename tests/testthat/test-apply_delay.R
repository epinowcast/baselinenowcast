test_that("apply_delay function works correctly on simple triangle", {
  set.seed(123)
  # Make a simple triangle of ones
  triangle <- matrix(nrow = 5, ncol = 4, data = 1)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.1)
  result <- apply_delay(
    triangle_to_nowcast = triangle,
    delay_pmf = delay_pmf
  )

  expect_is(result, "matrix")

  # Test that the dimensions of the output match the input
  expect_identical(dim(result), dim(triangle))

  # Test that the known values remain unchanged
  expect_identical(result[1:3, 1:2], triangle[1:3, 1:2])
})

test_that("apply_delay function works correctly with larger triangle", {
  # Create a sample triangle to nowcast
  triangle_to_nowcast <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, NA,
      80, 40, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Create a sample delay PMF
  delay_pmf <- c(0.5, 0.3, 0.15, 0.05)

  # Run the function
  result <- apply_delay(triangle_to_nowcast, delay_pmf)

  # Test that the output is a matrix
  expect_is(result, "matrix")

  # Test that the dimensions of the output match the input
  expect_identical(dim(result), dim(triangle_to_nowcast))

  # Test that the known values remain unchanged
  expect_identical(result[1:3, 1:2], triangle_to_nowcast[1:3, 1:2])

  # Test that NA values are replaced
  expect_false(anyNA(result))

  # Test specific calculations
  # For the last row, second column (delay = 1)
  expect_equal(result[4, 2], 70 * delay_pmf[2] / delay_pmf[1], tolerance = 1e-6)

  # For the third row, third column (delay = 2)
  expected_total <- sum(triangle_to_nowcast[3, 1:2]) / sum(delay_pmf[1:2])
  expect_equal(result[3, 3], expected_total * delay_pmf[3], tolerance = 1e-6)

  # Test error handling, specific error message expected
  expect_error(
    apply_delay(
      triangle_to_nowcast,
      c(0.5, 0.5)
    ),
    regexp = "Length of the delay pmf is not the same"
  )
})

test_that("apply_delay function works the same as the more verbose for loop", {
  triangle <- matrix(nrow = 5, ncol = 4, data = 1)
  delay_pmf <- c(0.4, 0.3, 0.2, 0.1)
  result <- apply_delay(
    triangle_to_nowcast = triangle,
    delay_pmf = delay_pmf
  )

  n_delays <- length(delay_pmf)
  n_dates <- nrow(triangle)
  expectation2 <- triangle
  for (co in 2:n_delays) {
    block_bottom_left <- expectation2[(n_dates - co + 2):n_dates, 1:(co - 1),
      drop = FALSE
    ]
    # Uses the observed data to find the expected total on that reference date
    exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])
    # * Note, we will have to do some correction if this is 0, ignore for now*
    # Finds the expected value for the particular delay by scaling by the
    # delay pmf for delay d
    expectation2[(n_dates - co + 2):n_dates, co] <- exp_total * delay_pmf[co]
  }

  expect_identical(result, expectation2)
})

test_that("calc_expectation function works correctly", {
  # Setup
  n_dates <- 10
  n_delays <- 5
  expectation <- matrix(1:50, nrow = n_dates, ncol = n_delays)
  delay_pmf <- c(0.5, 0.3, 0.1, 0.07, 0.03)

  # Test 1: Basic functionality
  result <- calc_expectation(3, expectation, n_dates, delay_pmf)
  expect_is(result, "matrix")
  expect_identical(dim(result), as.integer(c(n_dates, n_delays)))

  # Test 2: Check if the function modifies the correct cells
  original <- expectation
  modified <- calc_expectation(3, expectation, n_dates, delay_pmf)
  expect_equal(modified[1:8, ], original[1:8, ], tolerance = 1e-6)
  expect_false(all(modified[9:10, 3] == original[9:10, 3]))

  # Test 3: Check if the calculation is correct
  block_bottom_left <- expectation[9:10, 1:2]
  exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:2])
  expected_values <- exp_total * delay_pmf[3]
  expect_identical(modified[9:10, 3], expected_values)

  # Test 4: Edge case - when co is 2
  result_edge <- calc_expectation(2, expectation, n_dates, delay_pmf)
  expect_identical(dim(result_edge), as.integer(c(n_dates, n_delays)))

  # Test 5: Error handling - invalid co
  expect_error(calc_expectation(1, expectation, n_dates, delay_pmf))
  expect_error(calc_expectation(n_delays + 1, expectation, n_dates, delay_pmf))
})
