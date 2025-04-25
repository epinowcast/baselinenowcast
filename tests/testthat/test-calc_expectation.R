# Setup shared test utilities
n_dates <- 10
n_delays <- 5
full_expectation <- matrix(1:50, nrow = n_dates, ncol = n_delays)
expectation <- generate_triangle(full_expectation)
delay_pmf <- c(0.5, 0.3, 0.1, 0.07, 0.03)
delay_cdf_prev <- sum(delay_pmf[1])

test_that(".calc_expectation has correct basic functionality", {
  result <- .calc_expectation(
    delay_index = 3,
    expectation = expectation,
    delay_prob = delay_pmf[3],
    delay_cdf_prev = delay_cdf_prev,
    n_rows = n_dates
  )

  expect_identical(dim(result), as.integer(c(n_dates, n_delays)))
})

test_that(".calc_expectation modifies only the correct cells", {
  original <- expectation
  modified <- .calc_expectation(
    delay_index = 2,
    expectation = expectation,
    delay_prob = delay_pmf[2],
    delay_cdf_prev = delay_cdf_prev,
    n_rows = n_dates
  )

  expect_equal(modified[1:9, ], original[1:9, ], tolerance = 1e-6)
  expect_identical(modified[10, 2], 6.3)
})

test_that(".calc_expectation calculates correct values", {

  modified_step1 <- .calc_expectation(
    delay_index = 2,
    expectation = expectation,
    delay_prob = delay_pmf[2],
    delay_cdf_prev = delay_cdf_prev,
    n_rows = n_dates
  )

  # Then apply for delay_index 3
  modified_step2 <- .calc_expectation(
    delay_index = 3,
    expectation = modified_step1,
    delay_prob = delay_pmf[3],
    delay_cdf_prev = sum(delay_pmf[1:2]),
    n_rows = n_dates
  )

  # Test the output of delay_index 3
  block_bottom_left <- modified_step1[9:10, 1:2]
  exp_total <- .calc_modified_expectation(
    x = rowSums(block_bottom_left),
    delay_cdf_prev = sum(delay_pmf[1:2])
  )
  expected_values <- exp_total * delay_pmf[3]

  expect_identical(modified_step2[9:10, 3], expected_values)
})
