# Sample data setup
test_matrix <- matrix(c(3.7, 6.1, 5.2, 10.4), byrow = TRUE, nrow = 2)
test_vector <- c(3.7, 6.1, 5.2, 10.4)
uncertainty_params <- c(50, 100)

test_that("sample_distribution: works with all three options for error functions", {
  result <- sample_distribution(
    pred = test_matrix,
    uncertainty_params = uncertainty_params,
    observation_model_name = "negative binomial"
  )
  expect_true(checkmate::test_integerish(result))
  expect_length(result, length(test_matrix))
  expect_true(all(is.finite(result)))


  result <- sample_distribution(
    pred = test_matrix,
    uncertainty_params = uncertainty_params,
    observation_model_name = "rnorm"
  )
  expect_type(result, "double")
  expect_length(result, length(test_matrix))
  expect_true(all(is.finite(result)))

  result <- sample_distribution(
    pred = test_matrix,
    uncertainty_params = uncertainty_params,
    observation_model_name = "rgamma"
  )
  expect_type(result, "double")
  expect_length(result, length(test_matrix))
  expect_true(all(is.finite(result)))

  # Test with vector input
  result_vector <- sample_distribution(
    test_vector,
    uncertainty_params = c(50, 100, 75, 125),
    observation_model_name = "negative binomial"
  )

  expect_true(checkmate::test_integerish(result_vector))
  expect_length(result_vector, length(test_vector))
})

test_that("sample_distribution: errors appropriately if observation model not supported", { # nolint
  expect_error(
    sample_distribution(
      pred = test_matrix,
      uncertainty_params = uncertainty_params,
      observation_model_name = "bernoulli"
    ),
    regexp = "not supported by `sample_distribution` error model."
  )
})

test_that("sample_distribution produces different results with different seeds", {
  set.seed(123)
  result1 <- sample_distribution(
    test_vector,
    uncertainty_params = c(50, 100, 75, 125),
    observation_model_name = "negative binomial"
  )

  set.seed(456)
  result2 <- sample_distribution(
    test_vector,
    uncertainty_params = c(50, 100, 75, 125),
    observation_model_name = "negative binomial"
  )

  expect_false(identical(result1, result2))
})

test_that("sample_distribution produces consistent results with same seed", {
  set.seed(123)
  result1 <- sample_distribution(
    test_vector,
    uncertainty_params = c(50, 100, 75, 125),
    observation_model_name = "negative binomial"
  )

  set.seed(123)
  result2 <- sample_distribution(
    test_vector,
    uncertainty_params = c(50, 100, 75, 125),
    observation_model_name = "negative binomial"
  )

  expect_identical(result1, result2)
})

test_that("sample_distribution negative binomial has reasonable statistical properties", {
  set.seed(123)
  mu_vals <- rep(100, 1000)
  size_vals <- rep(10, 1000)

  samples <- sample_distribution(
    mu_vals,
    uncertainty_params = size_vals,
    observation_model_name = "negative binomial"
  )

  # Mean should be approximately mu
  expect_equal(mean(samples), 100, tolerance = 10)

  # Should be non-negative integers
  expect_true(all(samples >= 0))
  expect_true(all(samples == floor(samples)))
  expect_true(checkmate::test_integerish(samples))
})

test_that("sample_distribution normal has reasonable statistical properties", {
  set.seed(123)
  mean_vals <- rep(50, 1000)
  sd_vals <- rep(5, 1000)

  samples <- sample_distribution(
    mean_vals,
    uncertainty_params = sd_vals,
    observation_model_name = "normal"
  )

  # Mean should be approximately the input mean
  expect_equal(mean(samples), 50, tolerance = 1)

  # Standard deviation should be approximately the input sd
  expect_equal(sd(samples), 5, tolerance = 1)
})

test_that("sample_distribution gamma has reasonable statistical properties", {
  set.seed(123)
  pred_vals <- rep(10, 1000)
  uncertainty_vals <- rep(2, 1000)

  samples <- sample_distribution(
    pred_vals,
    uncertainty_params = uncertainty_vals,
    observation_model_name = "gamma"
  )

  # Should be positive
  expect_true(all(samples > 0))

  # Mean should be approximately pred_vals
  expect_equal(mean(samples), 10, tolerance = 1)
})

test_that("sample_distribution handles edge cases", {
  # Single value input
  single_result <- sample_distribution(
    5.0,
    uncertainty_params = 10,
    observation_model_name = "negative binomial"
  )
  expect_length(single_result, 1)
  expect_true(checkmate::test_integerish(single_result))

  # Zero values
  zero_result <- sample_distribution(
    c(0, 1, 2),
    uncertainty_params = c(1, 1, 1),
    observation_model_name = "negative binomial"
  )
  expect_length(zero_result, 3)
  expect_true(zero_result[1] >= 0) # Can sample 0 from negative binomial with mu=0
})

test_that("sample_distribution handles large values", {
  large_vals <- c(1000, 5000, 10000)
  large_uncertainty <- c(100, 500, 1000)

  result <- sample_distribution(
    large_vals,
    uncertainty_params = large_uncertainty,
    observation_model_name = "negative binomial"
  )

  expect_length(result, 3)
  expect_true(all(is.finite(result)))
  expect_true(checkmate::test_integerish(result))
})

test_that("sample_distribution works with different parameter lengths", {
  # Uncertainty params should match length of pred
  expect_no_error(
    sample_distribution(
      c(1, 2, 3, 4),
      uncertainty_params = c(10, 20, 30, 40),
      observation_model_name = "negative binomial"
    )
  )

  # Single uncertainty param (should be recycled by R)
  result_single <- sample_distribution(
    c(1, 2, 3, 4),
    uncertainty_params = 10,
    observation_model_name = "negative binomial"
  )
  expect_length(result_single, 4)
  expect_true(checkmate::test_integerish(result_single))
})
test_that("sample_distribution handles boundary values", {
  # Very small values
  small_result <- sample_distribution(
    c(0.001, 0.01, 0.1),
    uncertainty_params = c(1, 1, 1),
    observation_model_name = "normal"
  )
  expect_length(small_result, 3)
  expect_true(all(is.finite(small_result)))

  # For gamma, test with small uncertainty
  gamma_small <- sample_distribution(
    c(1, 2, 3),
    uncertainty_params = c(0.1, 0.1, 0.1),
    observation_model_name = "gamma"
  )
  expect_length(gamma_small, 3)
  expect_true(all(gamma_small > 0))
})

test_that("sample_distribution errors if value less than 0 passed in for uncertainty parmeters for negative binomial", { # nolint
  expect_error(
    sample_distribution(
      test_matrix,
      uncertainty_params = c(0, 0.1),
      observation_model_name = "negative binomial"
    ),
    regexp = "'uncertainty_params' must be greater than 0"
  )
  expect_no_error(sample_distribution(
    test_matrix,
    uncertainty_params = c(0, 0.1),
    observation_model_name = "normal"
  ))
  expect_no_error(sample_distribution(
    test_matrix,
    uncertainty_params = c(0, 0.1),
    observation_model_name = "gamma"
  ))
})

test_that("sample_distribution works in realistic scenarios", {
  # Realistic nowcasting scenario
  nowcast_preds <- c(45.2, 67.8, 123.4, 89.1, 156.7)
  nb_sizes <- c(15, 25, 40, 30, 50)

  set.seed(42)
  nowcast_samples <- sample_distribution(
    nowcast_preds,
    uncertainty_params = nb_sizes,
    observation_model_name = "negative binomial"
  )

  expect_length(nowcast_samples, 5)
  expect_true(all(nowcast_samples >= 0))
  expect_true(checkmate::test_integerish(nowcast_samples))

  # Should be reasonably close to input means
  expect_true(all(abs(nowcast_samples - nowcast_preds) < 100))
})
