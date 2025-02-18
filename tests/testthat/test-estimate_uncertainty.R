test_that("estimate_uncertainty function generates dispersion parameters", {
  set.seed(123)
  # Make a simple triangle
  triangle <- matrix(
    c(
      70, 40, 20, 5,
      80, 50, 10, 10,
      100, 40, 31, 20,
      95, 45, 21, NA,
      82, 42, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 6,
    byrow = TRUE
  )
  delay_pmf <- c(0.4, 0.3, 0.2, 0.1)


  # Test case 1: works as expected
  result2 <- estimate_uncertainty(
    triangle_for_uncertainty = triangle,
    delay_pmf,
    n_history_dispersion = 4
  )
  expect_length(result2, 3L)

  # Test case 3: n_history_dispersion too large
  expect_error(
    estimate_uncertainty(
      triangle_for_uncertainty = triangle,
      delay_pmf = delay_pmf,
      n_history_dispersion = 10
    ),
    regexp = "Triangle to nowcast does not contain sufficient rows"
  )
  # Test case 4: Mismatched delay_pmf length
  wrong_delay_pmf <- c(0.5, 0.5)
  expect_error(
    estimate_uncertainty(
      triangle_for_uncertainty = triangle,
      delay_pmf = wrong_delay_pmf,
      n_history_dispersion = 4
    ),
    regexp = "Length of the delay pmf is not the same as the number of delays"
  ) # nolint

  # Test case 5: Empty triangle
  empty_triangle <- matrix(nrow = 0, ncol = 0)
  expect_error(
    estimate_uncertainty(
      triangle_for_uncertainty = empty_triangle,
      delay_pmf = delay_pmf,
      n_history_dispersion = 1
    ),
    regexp = "Assertion on 'triangle' failed: Contains only missing values."
  ) # nolint

  # Test case 6: Triangle with all NAs, should error
  na_triangle <- matrix(NA, nrow = 6, ncol = 4)
  expect_error(
    estimate_uncertainty(
      triangle_for_uncertainty = na_triangle,
      delay_pmf = delay_pmf,
      n_history_dispersion = 4
    ),
    regexp = "Assertion on 'triangle' failed: Contains only missing values."
  ) # nolint

  # Test case 7: Triangle with some zero values
  zero_triangle <- triangle
  zero_triangle[1, 1] <- 0
  result7 <- estimate_uncertainty(
    triangle_for_uncertainty = zero_triangle,
    delay_pmf = delay_pmf,
    n_history_dispersion = 4
  )
  expect_type(result7, "double")
  expect_length(result7, length(delay_pmf) - 1)

  # Test case 8: Ensure function handles rounding errors
  small_triangle <- matrix(c(
    0.1, 0.05, 0.02,
    0.15, 0.08, NA,
    0.2, NA, NA
  ), nrow = 3, byrow = TRUE)
  small_delay_pmf <- c(0.6, 0.3, 0.1)
  expect_error(
    estimate_uncertainty(
      triangle_for_uncertainty = small_triangle,
      delay_pmf = small_delay_pmf,
      n_history_dispersion = 2
    ),
    regexp = "Assertion on 'triangle' failed: Must be of type 'integerish'"
  ) # nolint
})
