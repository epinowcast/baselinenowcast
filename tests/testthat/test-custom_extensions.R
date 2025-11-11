# Tests demonstrating custom extensions and user-defined components

# Custom Distribution Tests ----------------------------------------------------

test_that("users can create custom Gaussian distribution", {
  strategy <- uncertainty_by_horizon()

  gaussian_model <- uncertainty_model(
    fit = function(obs, pred) {
      sqrt(mean((obs - pred)^2))
    },
    sample = function(pred, params) {
      rnorm(length(pred), mean = pred, sd = params)
    },
    family = "gaussian",
    strategy = strategy
  )

  expect_s3_class(gaussian_model, "uncertainty_gaussian")
  expect_s3_class(gaussian_model, "uncertainty_model")
  expect_identical(gaussian_model$family, "gaussian")
})

test_that("custom Gaussian distribution works end-to-end", {
  set.seed(123)
  strategy <- uncertainty_by_horizon()

  gaussian_model <- uncertainty_model(
    fit = function(obs, pred) {
      sqrt(mean((obs - pred)^2, na.rm = TRUE))
    },
    sample = function(pred, params) {
      rnorm(length(pred), mean = pred, sd = params)
    },
    family = "gaussian",
    strategy = strategy
  )

  # Fit on some data
  obs <- matrix(c(10, 15, 20, 12, 18, 22), nrow = 2, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21), nrow = 2, byrow = TRUE)

  fitted_params <- gaussian_model$fit(obs, pred)

  # Sample using fitted parameters
  new_pred <- matrix(c(15, 20, 25), nrow = 1)
  samples <- gaussian_model$sample(new_pred, fitted_params[1])

  expect_type(samples, "double")
  expect_length(samples, length(new_pred))
})

test_that("custom distribution can be used in uncertainty_opts", {
  strategy <- uncertainty_by_horizon()

  custom_model <- uncertainty_model(
    fit = function(obs, pred) abs(mean(obs - pred)),
    sample = function(pred, params) pred + runif(length(pred), -params, params),
    family = "uniform",
    strategy = strategy
  )

  opts <- uncertainty_opts(model = custom_model)

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$model$family, "uniform")
})

# Custom Strategy Tests --------------------------------------------------------

test_that("users can create custom pooled strategy", {
  pooled_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        base_fit(as.vector(obs), as.vector(pred))
      }
    },
    name = "pooled"
  )

  expect_s3_class(pooled_strategy, "uncertainty_pooled")
  expect_s3_class(pooled_strategy, "uncertainty_strategy")
  expect_identical(attr(pooled_strategy, "name"), "pooled")
})

test_that("custom pooled strategy works with uncertainty_nb", {
  pooled_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        base_fit(as.vector(obs), as.vector(pred))
      }
    },
    name = "pooled"
  )

  model <- uncertainty_nb(strategy = pooled_strategy)

  expect_s3_class(model, "uncertainty_nb")
  expect_identical(attr(model$strategy, "name"), "pooled")
})

test_that("custom strategy applies fit function correctly", {
  pooled_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        base_fit(as.vector(obs), as.vector(pred))
      }
    },
    name = "pooled"
  )

  base_fit <- function(obs, pred) mean(obs - pred)
  wrapped_fit <- pooled_strategy$apply_fit(base_fit)

  obs <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  pred <- matrix(c(12, 18, 32, 13, 23, 33), nrow = 2, byrow = TRUE)

  result <- wrapped_fit(obs, pred)

  # Pooled should return a single value
  expected <- mean(as.vector(obs) - as.vector(pred))
  expect_identical(result, expected)
})

test_that("custom smoothed strategy can be created", {
  skip_if_not_installed("zoo")

  smoothed_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        raw_params <- sapply(seq_len(ncol(obs)), function(i) {
          base_fit(obs[, i], pred[, i])
        })
        # Apply smoothing to parameters
        zoo::rollmean(raw_params, k = 2, fill = NA, align = "right")
      }
    },
    name = "smoothed"
  )

  expect_s3_class(smoothed_strategy, "uncertainty_smoothed")
  expect_identical(attr(smoothed_strategy, "name"), "smoothed")
})

# Custom Aggregation Tests -----------------------------------------------------

test_that("users can create rolling sum aggregation", {
  skip_if_not_installed("zoo")

  rolling_agg <- aggregation_opts(
    ref_time = function(x) {
      zoo::rollsum(x, k = 3, align = "right", fill = NA)
    },
    delay = function(x) rowSums(x, na.rm = TRUE)
  )

  expect_s3_class(rolling_agg, "aggregation_custom")
  expect_s3_class(rolling_agg, "aggregation_opts")

  # Test it works
  test_matrix <- matrix(1:12, nrow = 4, ncol = 3)
  result <- rolling_agg$ref_time(test_matrix)

  expect_true(is.matrix(result))
  expect_identical(dim(result), dim(test_matrix))
})

test_that("users can create column aggregation", {
  col_agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) colSums(x, na.rm = TRUE)
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  result <- col_agg$delay(test_matrix)

  expected <- colSums(test_matrix, na.rm = TRUE)
  expect_identical(result, expected)
})

test_that("custom aggregation helper function pattern works", {
  skip_if_not_installed("zoo")

  # Helper function that returns aggregation_opts
  aggregation_rolling <- function(k = 3) {
    aggregation_opts(
      ref_time = function(x) {
        zoo::rollsum(x, k = k, align = "right", fill = NA)
      },
      delay = function(x) rowSums(x, na.rm = TRUE)
    )
  }

  agg3 <- aggregation_rolling(k = 3)
  agg5 <- aggregation_rolling(k = 5)

  expect_s3_class(agg3, "aggregation_opts")
  expect_s3_class(agg5, "aggregation_opts")

  # Test they produce different results
  test_matrix <- matrix(1:20, nrow = 5, ncol = 4)
  result3 <- agg3$ref_time(test_matrix)
  result5 <- agg5$ref_time(test_matrix)

  expect_false(identical(result3, result5))
})

# End-to-End Custom Configuration Tests ----------------------------------------

test_that("custom model, strategy, and aggregation work together", {
  # Custom strategy
  pooled_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) base_fit(as.vector(obs), as.vector(pred))
    },
    name = "pooled"
  )

  # Custom model with custom strategy
  custom_model <- uncertainty_model(
    fit = function(obs, pred) mean(abs(obs - pred)),
    sample = function(pred, params) pred + rnorm(length(pred), 0, params),
    family = "custom",
    strategy = pooled_strategy
  )

  # Custom aggregation
  custom_agg <- aggregation_opts(
    ref_time = function(x) x,
    delay = function(x) colSums(x, na.rm = TRUE)
  )

  # Combine in uncertainty_opts
  opts <- uncertainty_opts(
    model = custom_model,
    aggregation = custom_agg
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$model$family, "custom")
  expect_identical(attr(opts$model$strategy, "name"), "pooled")
  expect_s3_class(opts$aggregation, "aggregation_custom")
})

test_that("custom configuration can fit and sample", {
  set.seed(456)

  # Create custom configuration
  strategy <- uncertainty_by_horizon()
  custom_model <- uncertainty_model(
    fit = function(obs, pred) {
      sd_vals <- apply(obs - pred, 2, sd, na.rm = TRUE)
      pmax(sd_vals, 0.1)
    },
    sample = function(pred, params) {
      if (is.matrix(pred)) {
        result <- matrix(NA, nrow = nrow(pred), ncol = ncol(pred))
        for (i in seq_len(ncol(pred))) {
          result[, i] <- rnorm(nrow(pred), mean = pred[, i], sd = params[i])
        }
        result
      } else {
        rnorm(length(pred), mean = pred, sd = params[1])
      }
    },
    family = "normal",
    strategy = strategy
  )

  opts <- uncertainty_opts(model = custom_model)

  # Fit
  obs <- matrix(c(10, 15, 20, 12, 18, 22, 14, 16, 24), nrow = 3, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21, 15, 15, 23), nrow = 3, byrow = TRUE)

  params <- opts$model$fit(obs, pred)

  expect_type(params, "double")
  expect_length(params, ncol(obs))
  expect_true(all(params > 0))

  # Sample
  new_pred <- matrix(c(15, 20, 25), nrow = 1)
  samples <- opts$model$sample(new_pred, params)

  expect_type(samples, "double")
  expect_identical(dim(samples), dim(new_pred))
})

# Validation of Custom Components ----------------------------------------------

test_that("custom model validates correctly", {
  strategy <- uncertainty_by_horizon()

  expect_no_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      family = "test",
      strategy = strategy
    )
  )
})

test_that("custom strategy validates correctly", {
  expect_no_error(
    uncertainty_strategy(
      apply_fit = function(base_fit) base_fit,
      name = "test"
    )
  )
})

test_that("custom aggregation validates correctly", {
  expect_no_error(
    aggregation_opts(
      ref_time = identity,
      delay = function(x) rowSums(x, na.rm = TRUE)
    )
  )
})

# Real-world Extension Examples ------------------------------------------------

test_that("Gamma distribution extension example works", {
  strategy <- uncertainty_by_horizon()

  gamma_model <- uncertainty_model(
    fit = function(obs, pred) {
      # Fit gamma using method of moments
      residuals <- obs - pred
      residual_var <- max(var(residuals), 1e-6)
      shape <- mean(residuals)^2 / residual_var
      list(shape = shape, rate = shape / mean(residuals))
    },
    sample = function(pred, params) {
      # Sample from gamma and add to predictions
      # Note: simplified for testing
      pred + 1
    },
    family = "gamma",
    strategy = strategy
  )

  expect_s3_class(gamma_model, "uncertainty_gamma")
  expect_identical(gamma_model$family, "gamma")
})

test_that("Zero-inflated distribution concept works", {
  strategy <- uncertainty_by_horizon()

  zi_model <- uncertainty_model(
    fit = function(obs, pred) {
      # Estimate zero-inflation proportion
      prop_zero <- mean(obs == 0)
      list(prop_zero = prop_zero, size = 1)
    },
    sample = function(pred, params) {
      # Simplified zero-inflated sampler
      n <- length(pred)
      zeros <- rbinom(n, 1, params$prop_zero)
      ifelse(zeros == 1, 0, pred)
    },
    family = "zero_inflated",
    strategy = strategy
  )

  expect_s3_class(zi_model, "uncertainty_zero_inflated")
  expect_identical(zi_model$family, "zero_inflated")
})

test_that("Hierarchical strategy concept works", {
  hierarchical_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        # First fit per horizon
        horizon_params <- sapply(seq_len(ncol(obs)), function(i) {
          base_fit(obs[, i], pred[, i])
        })
        # Then take mean as hierarchical shrinkage
        mean(horizon_params)
      }
    },
    name = "hierarchical"
  )

  expect_s3_class(hierarchical_strategy, "uncertainty_hierarchical")
  expect_identical(attr(hierarchical_strategy, "name"), "hierarchical")
})

# Documentation Examples -------------------------------------------------------

test_that("README example for custom distribution works", {
  # Example from documentation
  my_dist <- uncertainty_model(
    fit = function(obs, pred) {
      mean((obs - pred)^2)
    },
    sample = function(pred, params) {
      rnorm(length(pred), pred, sqrt(params))
    },
    family = "gaussian",
    strategy = uncertainty_by_horizon()
  )

  opts <- uncertainty_opts(model = my_dist)

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$model$family, "gaussian")
})

test_that("README example for custom aggregation works", {
  skip_if_not_installed("zoo")

  # Example from documentation
  opts <- uncertainty_opts(
    model = uncertainty_nb(),
    aggregation = aggregation_opts(
      ref_time = function(x) {
        zoo::rollsum(x, k = 3, align = "right", fill = NA)
      },
      delay = function(x) rowSums(x, na.rm = TRUE)
    )
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_s3_class(opts$aggregation, "aggregation_custom")
})
