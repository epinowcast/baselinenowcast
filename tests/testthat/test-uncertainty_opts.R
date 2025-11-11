# Tests for uncertainty_opts() main configuration container

# Basic Construction Tests -----------------------------------------------------

test_that("uncertainty_opts creates valid object with all defaults", {
  opts <- uncertainty_opts()

  expect_s3_class(opts, "uncertainty_opts")
  expect_type(opts, "list")
  expect_named(opts, c("model", "aggregation"))
})

test_that("uncertainty_opts uses NB model by default", {
  opts <- uncertainty_opts()

  expect_s3_class(opts$model, "uncertainty_nb")
  expect_s3_class(opts$model, "uncertainty_model")
  expect_identical(opts$model$family, "nb")
})

test_that("uncertainty_opts uses observed aggregation by default", {
  opts <- uncertainty_opts()

  expect_s3_class(opts$aggregation, "aggregation_observed")
  expect_s3_class(opts$aggregation, "aggregation_opts")
})

test_that("uncertainty_opts accepts custom model", {
  custom_model <- uncertainty_poisson()
  opts <- uncertainty_opts(model = custom_model)

  expect_identical(opts$model, custom_model)
  expect_identical(opts$model$family, "poisson")
})

test_that("uncertainty_opts accepts custom aggregation", {
  custom_agg <- aggregation_opts(
    ref_time = function(x) x[1:2, ],
    delay = function(x) colSums(x)
  )
  opts <- uncertainty_opts(aggregation = custom_agg)

  expect_identical(opts$aggregation, custom_agg)
})

test_that("uncertainty_opts accepts both custom model and aggregation", {
  custom_model <- uncertainty_poisson()
  custom_agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) colSums(x, na.rm = TRUE)
  )

  opts <- uncertainty_opts(model = custom_model, aggregation = custom_agg)

  expect_identical(opts$model, custom_model)
  expect_identical(opts$aggregation, custom_agg)
})

test_that("uncertainty_opts has single class (no subclasses)", {
  opts <- uncertainty_opts()

  expect_identical(class(opts), "uncertainty_opts")
  expect_false(inherits(opts, "uncertainty_nb"))
  expect_false(inherits(opts, "aggregation_observed"))
})

# Validation Tests -------------------------------------------------------------

test_that("uncertainty_opts validates model", {
  expect_error(
    uncertainty_opts(model = "not a model"),
    regexp = "uncertainty_model"
  )
})

test_that("uncertainty_opts validates aggregation", {
  expect_error(
    uncertainty_opts(aggregation = "not aggregation"),
    regexp = "aggregation_opts"
  )
})

test_that("uncertainty_opts accepts valid uncertainty_nb model", {
  model <- uncertainty_nb()
  expect_no_error(uncertainty_opts(model = model))
})

test_that("uncertainty_opts accepts valid uncertainty_poisson model", {
  model <- uncertainty_poisson()
  expect_no_error(uncertainty_opts(model = model))
})

test_that("uncertainty_opts accepts valid custom model", {
  strategy <- uncertainty_by_horizon()
  model <- uncertainty_model(
    fit = function(obs, pred) 1,
    sample = function(pred, params) pred,
    family = "custom",
    strategy = strategy
  )

  expect_no_error(uncertainty_opts(model = model))
})

test_that("uncertainty_opts accepts aggregation_observed", {
  agg <- aggregation_observed()
  expect_no_error(uncertainty_opts(aggregation = agg))
})

test_that("uncertainty_opts accepts custom aggregation_opts", {
  agg <- aggregation_opts(
    ref_time = function(x) x,
    delay = function(x) rowSums(x, na.rm = TRUE)
  )
  expect_no_error(uncertainty_opts(aggregation = agg))
})

# Component Access Tests -------------------------------------------------------

test_that("uncertainty_opts components can be accessed", {
  opts <- uncertainty_opts()

  expect_type(opts$model$fit, "closure")
  expect_type(opts$model$sample, "closure")
  expect_identical(opts$model$family, "nb")
  expect_s3_class(opts$model$strategy, "uncertainty_strategy")
  expect_type(opts$aggregation$ref_time, "closure")
  expect_type(opts$aggregation$delay, "closure")
})

test_that("uncertainty_opts model fit function can be called", {
  opts <- uncertainty_opts()

  obs <- matrix(c(10, 15, 20, 12, 18, 22), nrow = 2, byrow = TRUE)
  pred <- matrix(c(11, 14, 19, 13, 17, 21), nrow = 2, byrow = TRUE)

  result <- opts$model$fit(obs, pred)

  expect_type(result, "double")
  expect_length(result, ncol(obs))
})

test_that("uncertainty_opts model sample function can be called", {
  set.seed(123)
  opts <- uncertainty_opts()

  pred <- matrix(c(10, 20, 30, 15, 25, 35), nrow = 2, byrow = TRUE)
  # Use single parameter for all predictions
  params <- 1.5

  result <- opts$model$sample(pred, params)

  expect_type(result, "double")
  expect_identical(dim(result), dim(pred))
})

test_that("uncertainty_opts aggregation functions can be called", {
  opts <- uncertainty_opts()

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  ref_result <- opts$aggregation$ref_time(test_matrix)
  delay_result <- opts$aggregation$delay(test_matrix)

  expect_identical(ref_result, test_matrix)
  expect_length(delay_result, nrow(test_matrix))
})

# print.uncertainty_opts() Tests -----------------------------------------------

test_that("print.uncertainty_opts displays complete configuration", {
  opts <- uncertainty_opts()
  output <- capture.output(print(opts))

  expect_true(any(grepl("Uncertainty Configuration", output, fixed = TRUE)))
  expect_true(any(grepl("Model", output, fixed = TRUE)))
  expect_true(any(grepl("Aggregation", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts shows model family", {
  opts <- uncertainty_opts()
  output <- capture.output(print(opts))

  expect_true(any(grepl("nb", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts shows model strategy", {
  opts <- uncertainty_opts()
  output <- capture.output(print(opts))

  expect_true(any(grepl("by_horizon", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts shows aggregation details", {
  opts <- uncertainty_opts()
  output <- capture.output(print(opts))

  expect_true(any(grepl("identity", output, fixed = TRUE)))
  expect_true(any(grepl("rowSums", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts shows Poisson model correctly", {
  opts <- uncertainty_opts(model = uncertainty_poisson())
  output <- capture.output(print(opts))

  expect_true(any(grepl("poisson", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts shows custom aggregation", {
  opts <- uncertainty_opts(
    aggregation = aggregation_opts(
      ref_time = function(x) x[1:2, ],
      delay = function(x) colSums(x)
    )
  )
  output <- capture.output(print(opts))

  expect_true(any(grepl("custom function", output, fixed = TRUE)))
})

test_that("print.uncertainty_opts returns object invisibly", {
  opts <- uncertainty_opts()

  result <- withVisible(print(opts))
  expect_false(result$visible)
  expect_identical(result$value, opts)
})

# Common Configuration Patterns ------------------------------------------------

test_that("uncertainty_opts creates NB with by_horizon configuration", {
  opts <- uncertainty_opts(
    model = uncertainty_nb(strategy = uncertainty_by_horizon()),
    aggregation = aggregation_observed()
  )

  expect_s3_class(opts$model, "uncertainty_nb")
  expect_identical(attr(opts$model$strategy, "name"), "by_horizon")
  expect_s3_class(opts$aggregation, "aggregation_observed")
})

test_that("uncertainty_opts creates Poisson configuration", {
  opts <- uncertainty_opts(
    model = uncertainty_poisson(strategy = uncertainty_by_horizon()),
    aggregation = aggregation_observed()
  )

  expect_s3_class(opts$model, "uncertainty_poisson")
  expect_identical(opts$model$family, "poisson")
})

test_that("uncertainty_opts works with custom aggregation pattern", {
  skip_if_not_installed("zoo")

  opts <- uncertainty_opts(
    model = uncertainty_nb(),
    aggregation = aggregation_opts(
      ref_time = function(x) {
        return(zoo::rollsum(x, k = 3, align = "right", fill = NA))
      },
      delay = function(x) rowSums(x, na.rm = TRUE)
    )
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_s3_class(opts$aggregation, "aggregation_custom")
})

# Equivalence Tests ------------------------------------------------------------

test_that("default uncertainty_opts equals explicit specification", {
  opts_default <- uncertainty_opts()

  opts_explicit <- uncertainty_opts(
    model = uncertainty_nb(strategy = uncertainty_by_horizon()),
    aggregation = aggregation_observed()
  )

  expect_identical(opts_default$model$family, opts_explicit$model$family)
  expect_identical(
    attr(opts_default$model$strategy, "name"),
    attr(opts_explicit$model$strategy, "name")
  )
  expect_identical(
    class(opts_default$aggregation),
    class(opts_explicit$aggregation)
  )
})

# Edge Cases -------------------------------------------------------------------

test_that("uncertainty_opts handles custom strategy in model", {
  custom_strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      return(function(obs, pred) base_fit(as.vector(obs), as.vector(pred)))
    },
    name = "pooled"
  )

  opts <- uncertainty_opts(
    model = uncertainty_nb(strategy = custom_strategy)
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(attr(opts$model$strategy, "name"), "pooled")
})

test_that("uncertainty_opts preserves all model components", {
  model <- uncertainty_nb()
  opts <- uncertainty_opts(model = model)

  expect_identical(opts$model$fit, model$fit)
  expect_identical(opts$model$sample, model$sample)
  expect_identical(opts$model$family, model$family)
  expect_identical(opts$model$strategy, model$strategy)
})

test_that("uncertainty_opts preserves all aggregation components", {
  agg <- aggregation_observed()
  opts <- uncertainty_opts(aggregation = agg)

  expect_identical(opts$aggregation$ref_time, agg$ref_time)
  expect_identical(opts$aggregation$delay, agg$delay)
})

# Multiple Instances -----------------------------------------------------------

test_that("multiple uncertainty_opts instances are independent", {
  opts1 <- uncertainty_opts(model = uncertainty_nb())
  opts2 <- uncertainty_opts(model = uncertainty_poisson())

  expect_false(identical(opts1$model, opts2$model))
  expect_false(opts1$model$family == opts2$model$family)
})

test_that("uncertainty_opts can create many different configurations", {
  configs <- list(
    uncertainty_opts(),
    uncertainty_opts(model = uncertainty_poisson()),
    uncertainty_opts(aggregation = aggregation_opts()),
    uncertainty_opts(
      model = uncertainty_poisson(),
      aggregation = aggregation_opts()
    )
  )

  expect_length(configs, 4)
  expect_true(all(sapply(configs, inherits, "uncertainty_opts")))
})

# Real-world Configuration Examples --------------------------------------------

test_that("uncertainty_opts supports realistic NB configuration", {
  opts <- uncertainty_opts(
    model = uncertainty_nb(strategy = uncertainty_by_horizon()),
    aggregation = aggregation_observed()
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$model$family, "nb")

  # Can extract and use components
  expect_type(opts$model$fit, "closure")
  expect_type(opts$model$sample, "closure")
  expect_type(opts$aggregation$ref_time, "closure")
  expect_type(opts$aggregation$delay, "closure")
})

test_that("uncertainty_opts supports Poisson with custom aggregation", {
  opts <- uncertainty_opts(
    model = uncertainty_poisson(),
    aggregation = aggregation_opts(
      ref_time = identity,
      delay = function(x) colSums(x, na.rm = TRUE)
    )
  )

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$model$family, "poisson")
  expect_s3_class(opts$aggregation, "aggregation_custom")
})
