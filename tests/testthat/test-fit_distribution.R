# Sample data setup
test_obs <- matrix(
  c(
    4, 15, 10,
    8, 10, 6
  ),
  nrow = 2,
  byrow = TRUE
)

test_pred <- matrix(
  c(
    11, 21, 8,
    10, 15.3, 3.5
  ),
  nrow = 2,
  byrow = TRUE
)

test_that("fit_distribution: works with all three options for error functions", {
  result <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
    observation_model_name = "dnbinom"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0)) # Parameters should be positive
  expect_named(result, NULL) # Should be unnamed vector
  # works with default identically
  result2 <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
  )
  expect_identical(result, result2)

  result <- fit_distribution(
    obs = test_obs + rnorm(length(test_obs), 0, 0.01),
    pred = test_pred,
    observation_model_name = "dnorm"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))

  result <- fit_distribution(
    obs = test_obs + rnorm(length(test_obs), 0, 0.05),
    pred = test_pred,
    observation_model_name = "dgamma"
  )
  expect_type(result, "double")
  expect_length(result, ncol(test_obs))
  expect_true(all(is.finite(result)))
  expect_true(all(result < 1000) & all(result > 0.1))
})

test_that("fit_distribution: errors appropriately if observation model not supported", { # nolint
  expect_error(
    fit_distribution(
      obs = test_obs,
      pred = test_pred,
      observation_model_name = "bernoulli"
    ),
    regexp = "not supported by `fit_distribution` error model."
  )

  expect_error(
    fit_distribution(
      obs = test_obs,
      pred = test_pred,
      observation_model_name = 123
    ),
    regexp = "not supported by `fit_distribution` error model."
  )
})

test_that("fit_distribution validates input matrices", {
  # Test non-matrix inputs
  expect_error(
    fit_distribution(
      obs = c(1, 2, 3, 4),
      pred = test_pred,
      observation_model_name = "negative binomial"
    ),
    regexp = "Dimensions of observations and predictions do not match."
  )

  expect_error(
    fit_distribution(
      obs = test_obs,
      pred = c(1, 2, 3, 4),
      observation_model_name = "negative binomial"
    ),
    regexp = "Dimensions of observations and predictions do not match."
  )

  # Test mismatched dimensions
  wrong_pred <- matrix(1:6, nrow = 3, ncol = 2)
  expect_error(
    fit_distribution(
      obs = test_obs,
      pred = wrong_pred,
      observation_model_name = "negative binomial"
    ),
    regexp = "Dimensions of observations and predictions do not match."
  )

  # Test different number of columns
  wrong_obs <- matrix(1:15, nrow = 5, ncol = 3)
  expect_error(
    fit_distribution(
      obs = wrong_obs,
      pred = test_pred,
      observation_model_name = "negative binomial"
    ),
    regexp = "Dimensions of observations and predictions do not match."
  )
})

test_that("fit_distribution handles edge cases", {
  # Single column matrices
  single_obs <- matrix(c(4, 6, 3, 5, 8), ncol = 1)
  single_pred <- matrix(c(3.7, 5.2, 2.8, 4.9, 7.5), ncol = 1)

  result <- fit_distribution(
    obs = single_obs,
    pred = single_pred,
    observation_model_name = "negative binomial"
  )

  expect_length(result, 1)
  expect_true(result > 0)
  expect_true(is.finite(result))

  # Single row matrices
  single_row_obs <- matrix(c(4, 5, 6), nrow = 1)
  single_row_pred <- matrix(c(3.7, 6.1, 5.8), nrow = 1)

  result_single_row <- fit_distribution(
    obs = single_row_obs,
    pred = single_row_pred,
    observation_model_name = "negative binomial"
  )

  expect_length(result_single_row, 3)
  expect_true(all(result_single_row > 0))

  # Large matrices
  large_obs <- matrix(rpois(200, 10), nrow = 40, ncol = 5)
  large_pred <- matrix(rnorm(200, 10, 2), nrow = 40, ncol = 5)

  result_large <- fit_distribution(
    obs = large_obs,
    pred = large_pred,
    observation_model_name = "negative binomial"
  )

  expect_length(result_large, 5)
  expect_true(all(result_large > 0))
  expect_true(all(is.finite(result_large)))
})
test_that("fit_distribution handles missing values", {
  # Add some NAs to test data
  obs_with_na <- test_obs
  obs_with_na[1, 1] <- NA
  obs_with_na[2, 2] <- NA

  pred_with_na <- test_pred
  pred_with_na[2, 1] <- NA

  result <- fit_distribution(
    obs = obs_with_na,
    pred = pred_with_na,
    observation_model_name = "negative binomial"
  )

  expect_is(result, "numeric")
  expect_length(result, ncol(obs_with_na))
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
})

test_that("fit_distribution optimization produces reasonable parameters", {
  # Create data with known dispersion for negative binomial
  set.seed(42)
  true_sizes <- c(15, 25)
  n_obs <- 50

  obs_known <- matrix(0, nrow = n_obs, ncol = 2)
  pred_known <- matrix(0, nrow = n_obs, ncol = 2)

  for (i in 1:2) {
    pred_known[, i] <- rnorm(n_obs, mean = 10 + i * 5, sd = 1)
    obs_known[, i] <- rnbinom(n_obs, size = true_sizes[i], mu = pred_known[, i])
  }

  estimated_params <- fit_distribution(
    obs = obs_known,
    pred = pred_known,
    observation_model_name = "negative binomial"
  )

  # Estimated parameters should be reasonably close to true values
  expect_true(all(estimated_params > 0))
  expect_equal(estimated_params[1], true_sizes[1], tolerance = 8)
  expect_equal(estimated_params[2], true_sizes[2], tolerance = 12)

  set.seed(42)
  stdevs <- c(1, 3)
  n_obs <- 50

  obs_known <- matrix(0, nrow = n_obs, ncol = 2)
  pred_known <- matrix(0, nrow = n_obs, ncol = 2)

  for (i in 1:2) {
    pred_known[, i] <- rnorm(n_obs, mean = 10 + i * 5, sd = 1)
    obs_known[, i] <- rnorm(n_obs, sd = stdevs[i], mean = pred_known[, i])
  }

  estimated_params_normal <- fit_distribution(
    obs = obs_known,
    pred = pred_known,
    observation_model_name = "normal"
  )

  # Estimated parameters should be reasonably close to true values
  expect_true(all(estimated_params_normal > 0))
  expect_equal(estimated_params_normal[1], stdevs[1], tolerance = 8)
  expect_equal(estimated_params_normal[2], stdevs[2], tolerance = 12)
})

test_that("fit_distribution handles different data types appropriately", {
  # Integer observations for negative binomial
  int_obs <- matrix(as.integer(test_obs), nrow = nrow(test_obs))

  result_int <- fit_distribution(
    obs = int_obs,
    pred = test_pred,
    observation_model_name = "negative binomial"
  )

  expect_true(all(result_int > 0))
  expect_true(all(is.finite(result_int)))

  # Continuous observations for normal
  cont_obs <- test_obs + rnorm(length(test_obs), 0, 0.1)

  result_cont <- fit_distribution(
    obs = cont_obs,
    pred = test_pred,
    observation_model_name = "normal"
  )

  expect_true(all(result_cont > 0))
  expect_true(all(is.finite(result_cont)))
})

test_that("fit_distributio errors passing in an empty matrix", {
  # Empty matrices
  expect_error(
    fit_distribution(
      obs = matrix(nrow = 0, ncol = 0),
      pred = matrix(nrow = 0, ncol = 0),
      observation_model_name = "negative binomial"
    ),
    regexp = "Empty observations or predictions passed in."
  )
})

test_that("fit_distribution optimization bounds work correctly", {
  # Test that parameters are within expected bounds [0.1, 1000]
  result <- fit_distribution(
    obs = test_obs,
    pred = test_pred,
    observation_model_name = "negative binomial"
  )

  expect_true(all(result >= 0.1))
  expect_true(all(result <= 1000))
})

test_that("fit_distribution produces consistent results", {
  # Same input should produce same output
  set.seed(123)
  obs_consistent <- matrix(rpois(20, 5), nrow = 10, ncol = 2)
  pred_consistent <- matrix(rnorm(20, 5, 0.5), nrow = 10, ncol = 2)

  result1 <- fit_distribution(
    obs = obs_consistent,
    pred = pred_consistent,
    observation_model_name = "negative binomial"
  )

  result2 <- fit_distribution(
    obs = obs_consistent,
    pred = pred_consistent,
    observation_model_name = "negative binomial"
  )

  expect_identical(result1, result2)
})

test_that("fit_distribution handles extreme values", {
  # Very small values
  small_obs <- matrix(c(0, 1, 0, 1, 1, 0), nrow = 3, ncol = 2)
  small_pred <- matrix(c(0.1, 0.5, 0.2, 1.1, 0.9, 0.3), nrow = 3, ncol = 2)

  result_small <- fit_distribution(
    obs = small_obs,
    pred = small_pred,
    observation_model_name = "negative binomial"
  )

  expect_true(all(is.finite(result_small)))
  expect_true(all(result_small > 0))

  # Very large values
  large_obs <- matrix(c(1000, 1500, 2000, 2500), nrow = 2, ncol = 2)
  large_pred <- matrix(c(950, 1450, 1950, 2450), nrow = 2, ncol = 2)

  result_large <- fit_distribution(
    obs = large_obs,
    pred = large_pred,
    observation_model_name = "negative binomial"
  )

  expect_true(all(is.finite(result_large)))
  expect_true(all(result_large > 0))
})

test_that("fit_distribution gamma distribution handles parameter constraints", {
  # Gamma requires positive observations and predictions
  positive_obs <- abs(test_obs) + 0.1
  positive_pred <- abs(test_pred) + 0.1

  result <- fit_distribution(
    obs = positive_obs,
    pred = positive_pred,
    observation_model_name = "gamma"
  )

  expect_true(all(result > 0))
  expect_true(all(is.finite(result)))
  expect_length(result, ncol(positive_obs))

  expect_error(
    fit_distribution(
      obs = -positive_obs,
      pred = -positive_pred,
      observation_model_name = "gamma"
    ),
    regexp = "Observations and/or predictions must be"
  )
})

test_that("fit_distribution normal distribution handles negative values", {
  # Normal can handle negative values
  mixed_obs <- test_obs - 5 # Some negative values
  mixed_pred <- test_pred - 5

  result <- fit_distribution(
    obs = mixed_obs,
    pred = mixed_pred,
    observation_model_name = "normal"
  )

  expect_true(all(result > 0)) # SD should still be positive
  expect_true(all(is.finite(result)))
  expect_length(result, ncol(mixed_obs))
})

test_that("fit_distribution negative binomial errors with negative values", {
  mixed_obs <- test_obs - 5 # Some negative values
  mixed_pred <- test_pred - 5

  expect_error(
    fit_distribution(
      obs = mixed_obs,
      pred = mixed_pred,
      observation_model_name = "negative binomial"
    ),
    regexp = "Observations and/or predictions must be"
  )
})

test_that("fit_distribution handles perfect predictions", {
  # When predictions exactly match observations
  perfect_obs <- test_obs
  perfect_pred <- test_obs # Identical

  result <- fit_distribution(
    obs = perfect_obs,
    pred = perfect_pred,
    observation_model_name = "negative binomial"
  )

  # Should still return valid parameters (might be large for NB indicating low dispersion)
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
  expect_length(result, ncol(perfect_obs))
})

test_that("fit_distribution handles horizon-specific fitting correctly", {
  # Create data where different horizons have different dispersion
  set.seed(123)
  n_obs <- 30
  obs_multi <- matrix(0, nrow = n_obs, ncol = 3)
  pred_multi <- matrix(0, nrow = n_obs, ncol = 3)

  # Different true dispersions for different horizons
  true_dispersions <- c(5, 15, 30)

  for (i in 1:3) {
    pred_multi[, i] <- rnorm(n_obs, mean = 10, sd = 1)
    obs_multi[, i] <- rnbinom(n_obs,
      size = true_dispersions[i],
      mu = pred_multi[, i]
    )
  }

  result <- fit_distribution(
    obs = obs_multi,
    pred = pred_multi,
    observation_model_name = "negative binomial"
  )

  expect_length(result, 3)
  expect_true(all(result > 0))

  # Results should be in increasing order (roughly matching true dispersions)
  # Allow for estimation uncertainty
  expect_true(result[1] < result[2] * 2) # First should be smaller than second
  expect_true(result[2] < result[3] * 2) # Second should be smaller than third
})
