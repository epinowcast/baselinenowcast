test_that(".validate_retro_inputs works correctly", {
  # Setup
  triangle <- matrix(1:40, nrow = 10, ncol = 4) |>
    .replace_lower_right_with_NA()

  # Test 1: Valid inputs
  expect_silent(.validate_retro_inputs(triangle, 2, 4))

  # Test 2: n_history_uncertainty is not a positive integer
  expect_error(
    .validate_retro_inputs(triangle, -1, 3),
    "Assertion on 'n_history_uncertainty' failed"
  )
  expect_error(
    .validate_retro_inputs(triangle, 1.5, 3),
    "Assertion on 'n_history_uncertainty' failed"
  )

  # Test 3: n_history_delay is not a positive integer
  expect_error(
    .validate_retro_inputs(triangle, 2, -1),
    "Assertion on 'n_history_delay' failed"
  )
  expect_error(
    .validate_retro_inputs(triangle, 2, 1.5),
    "Assertion on 'n_history_delay' failed"
  )

  # Test 4: n_history_uncertainty + n_history_delay > nrow(triangle)
  expect_error(
    .validate_retro_inputs(triangle, 7, 4),
    "Triangle to nowcast does not contain sufficient rows"
  )

  # Test 5: n_history_delay < ncol(triangle)
  expect_error(
    .validate_retro_inputs(triangle, 2, 2),
    "Specified number of observations to use to estimate the delay"
  )

  # Test 6: Edge case - n_history_uncertainty + n_history_delay == nrow(triangle)
  expect_silent(.validate_retro_inputs(triangle, 6, 4))

  # Test 7: Edge case - n_history_delay == ncol(triangle)
  expect_silent(.validate_retro_inputs(triangle, 1, 4))
})
