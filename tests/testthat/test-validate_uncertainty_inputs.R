test_that(".validate_uncertainty_inputs handles invalid inputs correctly", {
  # Test case 1: n_history_dispersion < 1
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 0, 2),
    "Number of observations to use to estimate dispersion"
  )

  # Test case 2: Insufficient observations in triangle
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 2, 2),
    "Reporting triangle to estimate uncertainty does not "
  )

  # Test case 3: n_history greater than number of rows in triangle
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 1, 4),
    "Reporting triangle to estimate uncertainty does not contain"
  )
})

test_that(".validate_uncertainty_inputs provides correct warnings", {
  # Test case 4: Warning when delay_pmf is provided
  expect_warning(
    .validate_uncertainty_inputs(matrix(1:16, 4, 4), c(0.5, 0.3, 0.2), 2, 2),
    regexp = "The delay distribution specified will be used to compute"
  )

  # Test case 5: Warning when delay_pmf is not provided
  expect_warning(
    .validate_uncertainty_inputs(matrix(1:16, 4, 4), NULL, 2, 2),
    "No delay distribution was specified, therefore the delay"
  )
})

test_that(".validate_uncertainty_inputs runs without errors for valid inputs", {
  # Test case 6: Valid inputs without delay_pmf
  expect_warning(
    .validate_uncertainty_inputs(matrix(1:25, 5, 5), NULL, 2, 3),
    "No delay distribution was specified"
  )

  # Test case 7: Valid inputs with delay_pmf
  expect_warning(
    .validate_uncertainty_inputs(matrix(1:25, 5, 5), c(0.5, 0.3, 0.2), 2, 3),
    "The delay distribution specified will be used"
  )
})
