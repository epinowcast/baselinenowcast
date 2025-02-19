test_that(".validate_uncertainty_inputs handles invalid inputs correctly", {
  # Test case 1: n_history_dispersion < 1
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 0, 2),
    "Number of observations to use to estimate dispersion"
  )

  # Test case 2: Insufficient observations in triangle
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 2, 2),
    "Insufficient rows in reporting triangle for specified number"
  )

  # Test case 3: n_history greater than number of rows in triangle
  expect_error(
    .validate_uncertainty_inputs(matrix(1:9, 3, 3), NULL, 1, 4),
    "Insufficient rows in reporting triangle for specified number"
  )
})

test_that(".validate_uncertainty_inputs handles valid inputs correctly", {
  expect_no_error(.validate_uncertainty_inputs(matrix(1:25, 5, 5), NULL, 2, 3))
  expect_no_error(.validate_uncertainty_inputs(
    matrix(1:25, 5, 5),
    c(0.5, 0.3, 0.2),
    2,
    3
  ))
})
test_that(".validate_uncertainty_inputs produces correct messages", {
  expect_message(
    .validate_uncertainty_inputs(matrix(1:25, 5, 5), NULL, 2, 3),
    "No delay distribution was specified, therefore the delay "
  )
  expect_message(
    .validate_uncertainty_inputs(
      matrix(1:25, 5, 5),
      c(0.5, 0.3, 0.2), 2, 3
    ),
    "The delay distribution specified will be used to compute "
  )
})
