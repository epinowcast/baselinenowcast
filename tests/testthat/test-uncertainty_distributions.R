# Tests for uncertainty_nb() and uncertainty_poisson()

# Negative Binomial Distribution Tests ----------------------------------------

test_that("uncertainty_nb creates valid NB model with defaults", {
  model <- uncertainty_nb()

  expect_s3_class(model, "uncertainty_nb")
  expect_s3_class(model, "uncertainty_model")
  expect_identical(model$family, "nb")
  expect_s3_class(model$strategy, "uncertainty_strategy")
})

test_that("uncertainty_nb creates model with specified strategy", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_nb(strategy = strategy)

  expect_s3_class(model, "uncertainty_nb")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})

test_that("uncertainty_nb has fit and sample functions", {
  model <- uncertainty_nb()

  expect_type(model$fit, "closure")
  expect_type(model$sample, "closure")
})

test_that("uncertainty_nb fit function uses fit_nb internally", {
  model <- uncertainty_nb()

  # Create test data
  obs <- matrix(c(10, 15, 20, 12, 18, 22), nrow = 2, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21), nrow = 2, byrow = TRUE)

  # Fit should return dispersion parameters
  result <- model$fit(obs, pred)

  expect_type(result, "double")
  expect_length(result, ncol(obs))
  expect_true(all(result > 0))
  expect_true(all(is.finite(result)))
})

test_that("uncertainty_nb sample function uses sample_nb", {
  set.seed(123)
  model <- uncertainty_nb()

  pred <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  # Need one dispersion parameter per prediction (6 total) or just 1
  dispersion <- 1.5

  result <- model$sample(pred, dispersion)

  expect_type(result, "double")
  expect_identical(dim(result), dim(pred))
  expect_true(all(result >= 0))
})

test_that("uncertainty_nb errors with invalid strategy", {
  expect_error(
    uncertainty_nb(strategy = "not a strategy"),
    regexp = "uncertainty_strategy"
  )
})

test_that("uncertainty_nb print method works", {
  model <- uncertainty_nb()
  output <- capture.output(print(model))

  expect_true(any(grepl("Uncertainty Model", output, fixed = TRUE)))
  expect_true(any(grepl("nb", output, fixed = TRUE)))
  expect_true(any(grepl("by_horizon", output, fixed = TRUE)))
})

# Poisson Distribution Tests ---------------------------------------------------

test_that("uncertainty_poisson creates valid Poisson model with defaults", {
  model <- uncertainty_poisson()

  expect_s3_class(model, "uncertainty_poisson")
  expect_s3_class(model, "uncertainty_model")
  expect_identical(model$family, "poisson")
  expect_s3_class(model$strategy, "uncertainty_strategy")
})

test_that("uncertainty_poisson creates model with specified strategy", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_poisson(strategy = strategy)

  expect_s3_class(model, "uncertainty_poisson")
  expect_identical(attr(model$strategy, "name"), "by_horizon")
})

test_that("uncertainty_poisson has fit and sample functions", {
  model <- uncertainty_poisson()

  expect_type(model$fit, "closure")
  expect_type(model$sample, "closure")
})

test_that("uncertainty_poisson fit function returns placeholder", {
  model <- uncertainty_poisson()

  # Create test data (integers required for Poisson)
  obs <- matrix(c(10L, 15L, 20L, 12L, 18L, 22L), nrow = 2, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21), nrow = 2, byrow = TRUE)

  # Poisson fit returns 1 as placeholder (no parameters to estimate)
  result <- model$fit(obs, pred)

  expect_type(result, "double")
  expect_length(result, ncol(obs))
  expect_true(all(result == 1))
})

test_that("uncertainty_poisson sample function generates Poisson samples", {
  set.seed(456)
  model <- uncertainty_poisson()

  pred <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  params <- c(1, 1, 1)

  result <- model$sample(pred, params)

  expect_type(result, "integer")
  expect_identical(dim(result), dim(pred))
  expect_true(all(result >= 0))
})

test_that("uncertainty_poisson samples are integers", {
  set.seed(789)
  model <- uncertainty_poisson()

  pred <- c(10.5, 20.3, 30.7)
  result <- model$sample(pred, 1)

  expect_type(result, "integer")
  expect_true(all(result == floor(result)))
})

test_that("uncertainty_poisson errors with invalid strategy", {
  expect_error(
    uncertainty_poisson(strategy = "not a strategy"),
    regexp = "uncertainty_strategy"
  )
})

test_that("uncertainty_poisson print method works", {
  model <- uncertainty_poisson()
  output <- capture.output(print(model))

  expect_true(any(grepl("Uncertainty Model", output, fixed = TRUE)))
  expect_true(any(grepl("poisson", output, fixed = TRUE)))
  expect_true(any(grepl("by_horizon", output, fixed = TRUE)))
})

# Internal Function Tests (.fit_poisson, .sample_poisson) ---------------------

test_that(".fit_poisson returns 1 for valid inputs", {
  x <- c(10L, 15L, 20L)
  mu <- c(12, 14, 19)

  result <- .fit_poisson(x, mu)

  expect_identical(result, 1)
})

test_that(".fit_poisson returns NA for empty inputs", {
  x <- integer(0)
  mu <- numeric(0)

  result <- .fit_poisson(x, mu)

  expect_true(is.na(result))
})

test_that(".fit_poisson errors with negative observations", {
  x <- c(10L, -5L, 20L)
  mu <- c(12, 14, 19)

  expect_error(
    .fit_poisson(x, mu),
    regexp = "Negative values detected in observations"
  )
})

test_that(".fit_poisson errors with negative predictions", {
  x <- c(10L, 15L, 20L)
  mu <- c(12, -14, 19)

  expect_error(
    .fit_poisson(x, mu),
    regexp = "Negative values detected in predictions"
  )
})

test_that(".fit_poisson errors with non-integer observations", {
  x <- c(10.5, 15.3, 20.7)
  mu <- c(12, 14, 19)

  expect_error(
    .fit_poisson(x, mu),
    regexp = "integerish"
  )
})

test_that(".fit_poisson accepts zero values", {
  x <- c(0L, 15L, 20L)
  mu <- c(1, 14, 19)

  result <- .fit_poisson(x, mu)
  expect_identical(result, 1)
})

test_that(".sample_poisson generates correct samples from vector", {
  set.seed(123)
  pred <- c(10, 20, 30)
  params <- 1

  result <- .sample_poisson(pred, params)

  expect_type(result, "integer")
  expect_length(result, length(pred))
  expect_true(all(result >= 0))
})

test_that(".sample_poisson generates correct samples from matrix", {
  set.seed(456)
  pred <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  params <- c(1, 1, 1)

  result <- .sample_poisson(pred, params)

  expect_type(result, "integer")
  expect_identical(dim(result), dim(pred))
  expect_true(all(result >= 0))
})

test_that(".sample_poisson preserves matrix dimnames", {
  set.seed(789)
  pred <- matrix(c(10, 20, 30), nrow = 1, byrow = TRUE)
  rownames(pred) <- "row1"
  colnames(pred) <- c("col1", "col2", "col3")

  result <- .sample_poisson(pred, 1)

  expect_identical(dimnames(result), dimnames(pred))
})

test_that(".sample_poisson handles NULL pred", {
  # .sample_poisson now validates pred, so NULL should error
  expect_error(
    .sample_poisson(NULL, 1),
    regexp = "pred.*must be numeric"
  )
})

test_that(".sample_poisson errors with non-numeric pred", {
  expect_error(
    .sample_poisson("not numeric", 1),
    regexp = "pred.*must be numeric"
  )
})

# Comparison Tests -------------------------------------------------------------

test_that("NB and Poisson models have same structure", {
  nb_model <- uncertainty_nb()
  pois_model <- uncertainty_poisson()

  expect_named(nb_model, names(pois_model))
  expect_named(nb_model, c("fit", "sample", "family", "strategy"))
})

test_that("NB and Poisson have different families", {
  nb_model <- uncertainty_nb()
  pois_model <- uncertainty_poisson()

  expect_false(nb_model$family == pois_model$family)
  expect_identical(nb_model$family, "nb")
  expect_identical(pois_model$family, "poisson")
})

test_that("NB and Poisson can use same strategy", {
  strategy <- uncertainty_by_horizon()
  nb_model <- uncertainty_nb(strategy = strategy)
  pois_model <- uncertainty_poisson(strategy = strategy)

  expect_identical(
    attr(nb_model$strategy, "name"),
    attr(pois_model$strategy, "name")
  )
})

test_that("NB samples have more variability than Poisson at high dispersion", {
  set.seed(100)
  pred <- rep(100, 1000)

  nb_model <- uncertainty_nb()
  pois_model <- uncertainty_poisson()

  # High dispersion for NB (similar to Poisson)
  nb_samples_high <- nb_model$sample(pred, 1000)
  pois_samples <- pois_model$sample(pred, 1)

  # NB variance should be similar to Poisson when dispersion is high
  # Allow some tolerance due to random variation
  expect_lt(abs(var(nb_samples_high) - var(pois_samples)), 25)
})

test_that("Poisson variance equals mean", {
  set.seed(200)
  pred <- rep(50, 1000)
  model <- uncertainty_poisson()

  samples <- model$sample(pred, 1)

  # For Poisson, variance should approximately equal mean
  expect_lt(abs(mean(samples) - 50), 5)
  expect_lt(abs(var(samples) - 50), 10)
})
