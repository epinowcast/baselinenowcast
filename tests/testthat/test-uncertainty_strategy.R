# Tests for uncertainty_strategy() and uncertainty_by_horizon()

# uncertainty_strategy() Tests -------------------------------------------------

test_that("uncertainty_strategy creates valid strategy object", {
  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) base_fit(obs, pred)
    },
    name = "test_strategy"
  )

  expect_s3_class(strategy, "uncertainty_strategy")
  expect_s3_class(strategy, "uncertainty_test_strategy")
  expect_type(strategy, "list")
})

test_that("uncertainty_strategy stores apply_fit function", {
  apply_fn <- function(base_fit) {
    function(obs, pred) base_fit(obs, pred)
  }

  strategy <- uncertainty_strategy(
    apply_fit = apply_fn,
    name = "test"
  )

  expect_identical(strategy$apply_fit, apply_fn)
})

test_that("uncertainty_strategy stores name attribute", {
  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) base_fit,
    name = "custom_name"
  )

  expect_identical(attr(strategy, "name"), "custom_name")
})

test_that("uncertainty_strategy creates correct class hierarchy", {
  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) base_fit,
    name = "pooled"
  )

  expect_true(inherits(strategy, "uncertainty_pooled"))
  expect_true(inherits(strategy, "uncertainty_strategy"))
  expect_identical(class(strategy), c("uncertainty_pooled",
                                       "uncertainty_strategy"))
})

test_that("uncertainty_strategy errors with missing apply_fit", {
  expect_error(
    uncertainty_strategy(name = "test"),
    regexp = "apply_fit"
  )
})

test_that("uncertainty_strategy errors with missing name", {
  expect_error(
    uncertainty_strategy(apply_fit = function(base_fit) base_fit),
    regexp = "name"
  )
})

test_that("uncertainty_strategy errors with non-function apply_fit", {
  expect_error(
    uncertainty_strategy(apply_fit = "not a function", name = "test"),
    regexp = "function"
  )
})

test_that("uncertainty_strategy errors with non-character name", {
  expect_error(
    uncertainty_strategy(
      apply_fit = function(base_fit) base_fit,
      name = 123
    ),
    regexp = "character"
  )
})

test_that("uncertainty_strategy apply_fit can wrap a function", {
  base_fit <- function(obs, pred) mean(obs - pred)

  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        base_fit(obs, pred) * 2
      }
    },
    name = "doubler"
  )

  wrapped_fit <- strategy$apply_fit(base_fit)
  result <- wrapped_fit(c(10, 20, 30), c(8, 18, 28))

  expect_identical(result, 4)
})

# uncertainty_by_horizon() Tests -----------------------------------------------

test_that("uncertainty_by_horizon creates valid strategy", {
  strategy <- uncertainty_by_horizon()

  expect_s3_class(strategy, "uncertainty_by_horizon")
  expect_s3_class(strategy, "uncertainty_strategy")
})

test_that("uncertainty_by_horizon has correct name", {
  strategy <- uncertainty_by_horizon()

  expect_identical(attr(strategy, "name"), "by_horizon")
})

test_that("uncertainty_by_horizon has apply_fit function", {
  strategy <- uncertainty_by_horizon()

  expect_type(strategy$apply_fit, "closure")
})

test_that("uncertainty_by_horizon wraps fit function correctly", {
  strategy <- uncertainty_by_horizon()

  # Simple base fit that returns sum
  base_fit <- function(obs, pred) sum(obs - pred)

  # Wrap it
  wrapped_fit <- strategy$apply_fit(base_fit)

  # Test with matrix data
  obs <- matrix(c(10, 15, 20, 12, 18, 22), nrow = 2, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21), nrow = 2, byrow = TRUE)

  result <- wrapped_fit(obs, pred)

  # Should return one value per column (horizon)
  expect_type(result, "double")
  expect_length(result, ncol(obs))
})

test_that("uncertainty_by_horizon applies fit independently to columns", {
  strategy <- uncertainty_by_horizon()

  # Base fit that returns mean difference
  base_fit <- function(obs, pred) mean(obs - pred)

  wrapped_fit <- strategy$apply_fit(base_fit)

  obs <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  pred <- matrix(c(12, 18, 32, 13, 23, 33), nrow = 2, byrow = TRUE)

  result <- wrapped_fit(obs, pred)

  # Calculate expected values for each column
  expected <- c(
    mean(obs[, 1] - pred[, 1]),
    mean(obs[, 2] - pred[, 2]),
    mean(obs[, 3] - pred[, 3])
  )

  expect_identical(result, expected)
})

test_that("uncertainty_by_horizon works with fit_nb", {
  strategy <- uncertainty_by_horizon()
  wrapped_fit <- strategy$apply_fit(fit_nb)

  obs <- matrix(c(10, 15, 20, 12, 18, 22, 14, 16, 24), nrow = 3, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21, 15, 15, 23), nrow = 3,
                 byrow = TRUE)

  result <- wrapped_fit(obs, pred)

  expect_type(result, "double")
  expect_length(result, ncol(obs))
  expect_true(all(result > 0))
  expect_true(all(is.finite(result)))
})

# print.uncertainty_strategy() Tests -------------------------------------------

test_that("print.uncertainty_strategy displays correctly", {
  strategy <- uncertainty_by_horizon()
  output <- capture.output(print(strategy))

  expect_true(any(grepl("Uncertainty Strategy", output)))
  expect_true(any(grepl("by_horizon", output)))
})

test_that("print.uncertainty_strategy works with custom strategy", {
  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) base_fit,
    name = "custom_test"
  )

  output <- capture.output(print(strategy))

  expect_true(any(grepl("Uncertainty Strategy", output)))
  expect_true(any(grepl("custom_test", output)))
})

test_that("print.uncertainty_strategy returns object invisibly", {
  strategy <- uncertainty_by_horizon()

  result <- withVisible(print(strategy))
  expect_false(result$visible)
  expect_identical(result$value, strategy)
})

# assert_uncertainty_strategy() Tests ------------------------------------------

test_that("assert_uncertainty_strategy accepts valid strategy", {
  strategy <- uncertainty_by_horizon()

  expect_no_error(assert_uncertainty_strategy(strategy))
  expect_invisible(assert_uncertainty_strategy(strategy))
})

test_that("assert_uncertainty_strategy errors with non-strategy object", {
  expect_error(
    assert_uncertainty_strategy("not a strategy"),
    regexp = "uncertainty_strategy"
  )
})

test_that("assert_uncertainty_strategy errors with missing class", {
  bad_strategy <- list(apply_fit = function(x) x)

  expect_error(
    assert_uncertainty_strategy(bad_strategy),
    regexp = "uncertainty_strategy"
  )
})

test_that("assert_uncertainty_strategy errors with missing apply_fit", {
  bad_strategy <- structure(
    list(),
    name = "test",
    class = c("uncertainty_test", "uncertainty_strategy")
  )

  expect_error(
    assert_uncertainty_strategy(bad_strategy),
    regexp = "apply_fit"
  )
})

test_that("assert_uncertainty_strategy errors with non-function apply_fit", {
  bad_strategy <- structure(
    list(apply_fit = "not a function"),
    name = "test",
    class = c("uncertainty_test", "uncertainty_strategy")
  )

  expect_error(
    assert_uncertainty_strategy(bad_strategy),
    regexp = "function"
  )
})

test_that("assert_uncertainty_strategy errors with missing name attribute", {
  # Create a strategy with explicit NULL name attribute
  bad_strategy <- structure(
    list(apply_fit = function(x) x),
    class = c("uncertainty_test", "uncertainty_strategy"),
    name = NULL
  )

  expect_error(
    assert_uncertainty_strategy(bad_strategy),
    regexp = "name.*attribute"
  )
})

test_that("assert_uncertainty_strategy accepts custom strategies", {
  custom_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) {
        base_fit(as.vector(obs), as.vector(pred))
      }
    },
    name = "pooled"
  )

  expect_no_error(assert_uncertainty_strategy(custom_strategy))
})

# Integration Tests ------------------------------------------------------------

test_that("strategy can be used with uncertainty_model", {
  strategy <- uncertainty_by_horizon()

  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "test",
    strategy = strategy
  )

  expect_s3_class(model, "uncertainty_model")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})

test_that("custom strategy can be used with uncertainty_model", {
  custom_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      function(obs, pred) base_fit(obs, pred)
    },
    name = "custom"
  )

  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "test",
    strategy = custom_strategy
  )

  expect_s3_class(model, "uncertainty_model")
  expect_identical(attr(model$strategy, "name"), "custom")
})

test_that("strategy works in uncertainty_nb", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_nb(strategy = strategy)

  expect_s3_class(model, "uncertainty_nb")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})

test_that("strategy works in uncertainty_poisson", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_poisson(strategy = strategy)

  expect_s3_class(model, "uncertainty_poisson")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})
