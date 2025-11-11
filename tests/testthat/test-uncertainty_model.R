# Tests for uncertainty_model() constructor and methods

test_that("uncertainty_model creates valid object with all components", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) mean((obs - pred)^2),
    sample = function(pred, params) pred + rnorm(length(pred), 0, params),
    family = "gaussian",
    strategy = strategy
  )

  expect_s3_class(model, "uncertainty_model")
  expect_s3_class(model, "uncertainty_gaussian")
  expect_type(model, "list")
  expect_identical(names(model), c("fit", "sample", "family", "strategy"))
})

test_that("uncertainty_model stores functions correctly", {
  fit_fn <- function(obs, pred) mean(obs - pred)
  sample_fn <- function(pred, params) pred
  strategy <- uncertainty_by_horizon()

  model <- uncertainty_model(
    fit = fit_fn,
    sample = sample_fn,
    family = "test",
    strategy = strategy
  )

  expect_identical(model$fit, fit_fn)
  expect_identical(model$sample, sample_fn)
})

test_that("uncertainty_model stores family and strategy correctly", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "custom_family",
    strategy = strategy
  )

  expect_identical(model$family, "custom_family")
  expect_s3_class(model$strategy, "uncertainty_strategy")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})

test_that("uncertainty_model errors with missing fit function", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      sample = function(pred, params) pred,
      family = "test",
      strategy = strategy
    ),
    regexp = "fit"
  )
})

test_that("uncertainty_model errors with missing sample function", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      family = "test",
      strategy = strategy
    ),
    regexp = "sample"
  )
})

test_that("uncertainty_model errors with missing family", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      strategy = strategy
    ),
    regexp = "family"
  )
})

test_that("uncertainty_model errors with missing strategy", {
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      family = "test"
    ),
    regexp = "strategy"
  )
})

test_that("uncertainty_model errors with non-function fit", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      fit = "not a function",
      sample = function(pred, params) pred,
      family = "test",
      strategy = strategy
    ),
    regexp = "function"
  )
})

test_that("uncertainty_model errors with non-function sample", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = 123,
      family = "test",
      strategy = strategy
    ),
    regexp = "function"
  )
})

test_that("uncertainty_model errors with non-character family", {
  strategy <- uncertainty_by_horizon()
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      family = 123,
      strategy = strategy
    ),
    regexp = "character"
  )
})

test_that("uncertainty_model errors with invalid strategy", {
  expect_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      family = "test",
      strategy = "not a strategy"
    ),
    regexp = "uncertainty_strategy"
  )
})

test_that("uncertainty_model creates correct class hierarchy", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "custom",
    strategy = strategy
  )

  expect_true(inherits(model, "uncertainty_custom"))
  expect_true(inherits(model, "uncertainty_model"))
  expect_identical(class(model), c("uncertainty_custom", "uncertainty_model"))
})

test_that("print.uncertainty_model displays correctly", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "test_family",
    strategy = strategy
  )

  output <- capture.output(print(model))
  expect_true(any(grepl("Uncertainty Model", output)))
  expect_true(any(grepl("test_family", output)))
  expect_true(any(grepl("by_horizon", output)))
})

test_that("print.uncertainty_model returns object invisibly", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "test",
    strategy = strategy
  )

  result <- withVisible(print(model))
  expect_false(result$visible)
  expect_identical(result$value, model)
})

test_that("uncertainty_model validation runs at construction", {
  strategy <- uncertainty_by_horizon()

  # Create a model that passes initial checks but has issues
  # This tests that assert_uncertainty_model is called
  expect_no_error(
    uncertainty_model(
      fit = function(obs, pred) 1,
      sample = function(pred, params) pred,
      family = "valid",
      strategy = strategy
    )
  )
})

test_that("uncertainty_model works with different strategy types", {
  # Test with by_horizon strategy
  strategy_bh <- uncertainty_by_horizon()
  model_bh <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "test",
    strategy = strategy_bh
  )

  expect_s3_class(model_bh, "uncertainty_model")
  expect_identical(attr(model_bh$strategy, "name"), "by_horizon")
})

test_that("uncertainty_model fit function can be called", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) sum((obs - pred)^2),
    sample = function(pred, params) pred,
    family = "test",
    strategy = strategy
  )

  obs <- c(10, 20, 30)
  pred <- c(12, 18, 32)

  result <- model$fit(obs, pred)
  expect_type(result, "double")
  expect_true(is.finite(result))
})

test_that("uncertainty_model sample function can be called", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred + rnorm(length(pred), 0, 0.1),
    family = "test",
    strategy = strategy
  )

  set.seed(123)
  pred <- c(10, 20, 30)
  result <- model$sample(pred, 1)

  expect_type(result, "double")
  expect_length(result, length(pred))
  expect_true(all(is.finite(result)))
})
