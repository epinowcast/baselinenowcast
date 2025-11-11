# Tests for aggregation_opts() and aggregation_observed()

# aggregation_opts() Tests -----------------------------------------------------

test_that("aggregation_opts creates valid object with defaults", {
  agg <- aggregation_opts()

  expect_s3_class(agg, "aggregation_opts")
  expect_s3_class(agg, "aggregation_custom")
  expect_type(agg, "list")
  expect_identical(names(agg), c("ref_time", "delay"))
})

test_that("aggregation_opts stores functions correctly", {
  ref_fn <- function(x) x[1:2, ]
  delay_fn <- function(x) colSums(x)

  agg <- aggregation_opts(ref_time = ref_fn, delay = delay_fn)

  expect_identical(agg$ref_time, ref_fn)
  expect_identical(agg$delay, delay_fn)
})

test_that("aggregation_opts uses identity for ref_time by default", {
  agg <- aggregation_opts()

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  result <- agg$ref_time(test_matrix)

  expect_identical(result, test_matrix)
})

test_that("aggregation_opts uses rowSums for delay by default", {
  agg <- aggregation_opts()

  test_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  result <- agg$delay(test_matrix)

  expected <- rowSums(test_matrix, na.rm = TRUE)
  expect_identical(result, expected)
})

test_that("aggregation_opts accepts custom ref_time function", {
  custom_ref <- function(x) x * 2

  agg <- aggregation_opts(ref_time = custom_ref)

  test_matrix <- matrix(1:6, nrow = 2, ncol = 3)
  result <- agg$ref_time(test_matrix)

  expect_identical(result, test_matrix * 2)
})

test_that("aggregation_opts accepts custom delay function", {
  custom_delay <- function(x) colSums(x, na.rm = TRUE)

  agg <- aggregation_opts(delay = custom_delay)

  test_matrix <- matrix(1:6, nrow = 2, ncol = 3)
  result <- agg$delay(test_matrix)

  expected <- colSums(test_matrix, na.rm = TRUE)
  expect_identical(result, expected)
})

test_that("aggregation_opts errors with non-function ref_time", {
  expect_error(
    aggregation_opts(ref_time = "not a function"),
    regexp = "function"
  )
})

test_that("aggregation_opts errors with non-function delay", {
  expect_error(
    aggregation_opts(delay = 123),
    regexp = "function"
  )
})

test_that("aggregation_opts handles NA values correctly", {
  agg <- aggregation_opts()

  test_matrix <- matrix(c(1, 2, NA, 4, 5, 6), nrow = 2, ncol = 3)
  result <- agg$delay(test_matrix)

  expected <- c(6, 12)
  expect_identical(as.vector(result), expected)
})

# aggregation_observed() Tests -------------------------------------------------

test_that("aggregation_observed creates valid object", {
  agg <- aggregation_observed()

  expect_s3_class(agg, "aggregation_observed")
  expect_s3_class(agg, "aggregation_opts")
  expect_type(agg, "list")
})

test_that("aggregation_observed uses identity for ref_time", {
  agg <- aggregation_observed()

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  result <- agg$ref_time(test_matrix)

  expect_identical(result, test_matrix)
})

test_that("aggregation_observed uses rowSums for delay", {
  agg <- aggregation_observed()

  test_matrix <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
  result <- agg$delay(test_matrix)

  expected <- rowSums(test_matrix, na.rm = TRUE)
  expect_identical(result, expected)
})

test_that("aggregation_observed equals default aggregation_opts", {
  agg_obs <- aggregation_observed()
  agg_custom <- aggregation_opts()

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_identical(
    agg_obs$ref_time(test_matrix),
    agg_custom$ref_time(test_matrix)
  )
  expect_identical(
    agg_obs$delay(test_matrix),
    agg_custom$delay(test_matrix)
  )
})

test_that("aggregation_observed has correct class hierarchy", {
  agg <- aggregation_observed()

  expect_true(inherits(agg, "aggregation_observed"))
  expect_true(inherits(agg, "aggregation_opts"))
  expect_identical(
    class(agg),
    c("aggregation_observed", "aggregation_opts")
  )
})

# print.aggregation_opts() Tests -----------------------------------------------

test_that("print.aggregation_opts displays observed aggregation correctly", {
  agg <- aggregation_observed()
  output <- capture.output(print(agg))

  expect_true(any(grepl("Aggregation Options", output)))
  expect_true(any(grepl("identity", output)))
  expect_true(any(grepl("rowSums", output)))
})

test_that("print.aggregation_opts displays custom aggregation", {
  agg <- aggregation_opts(
    ref_time = function(x) x[1:2, ],
    delay = function(x) colSums(x)
  )

  output <- capture.output(print(agg))

  expect_true(any(grepl("Aggregation Options", output)))
  expect_true(any(grepl("custom function", output)))
})

test_that("print.aggregation_opts returns object invisibly", {
  agg <- aggregation_observed()

  result <- withVisible(print(agg))
  expect_false(result$visible)
  expect_identical(result$value, agg)
})

test_that("print.aggregation_opts detects identity correctly", {
  agg <- aggregation_opts(ref_time = identity)
  output <- capture.output(print(agg))

  expect_true(any(grepl("identity", output)))
})

test_that("print.aggregation_opts detects default delay correctly", {
  agg <- aggregation_opts(delay = function(x) rowSums(x, na.rm = TRUE))
  output <- capture.output(print(agg))

  expect_true(any(grepl("rowSums", output)))
})

# assert_aggregation_opts() Tests ----------------------------------------------

test_that("assert_aggregation_opts accepts valid aggregation", {
  agg <- aggregation_observed()

  expect_no_error(assert_aggregation_opts(agg))
  expect_invisible(assert_aggregation_opts(agg))
})

test_that("assert_aggregation_opts accepts custom aggregation", {
  agg <- aggregation_opts(
    ref_time = function(x) x,
    delay = function(x) colSums(x)
  )

  expect_no_error(assert_aggregation_opts(agg))
})

test_that("assert_aggregation_opts errors with non-aggregation object", {
  expect_error(
    assert_aggregation_opts("not an aggregation"),
    regexp = "aggregation_opts"
  )
})

test_that("assert_aggregation_opts errors with missing ref_time", {
  bad_agg <- structure(
    list(delay = function(x) rowSums(x)),
    class = c("aggregation_custom", "aggregation_opts")
  )

  expect_error(
    assert_aggregation_opts(bad_agg),
    regexp = "ref_time"
  )
})

test_that("assert_aggregation_opts errors with missing delay", {
  bad_agg <- structure(
    list(ref_time = identity),
    class = c("aggregation_custom", "aggregation_opts")
  )

  expect_error(
    assert_aggregation_opts(bad_agg),
    regexp = "delay"
  )
})

test_that("assert_aggregation_opts errors with non-function ref_time", {
  bad_agg <- structure(
    list(ref_time = "not a function", delay = function(x) rowSums(x)),
    class = c("aggregation_custom", "aggregation_opts")
  )

  expect_error(
    assert_aggregation_opts(bad_agg),
    regexp = "function"
  )
})

test_that("assert_aggregation_opts errors with non-function delay", {
  bad_agg <- structure(
    list(ref_time = identity, delay = 123),
    class = c("aggregation_custom", "aggregation_opts")
  )

  expect_error(
    assert_aggregation_opts(bad_agg),
    regexp = "function"
  )
})

test_that("assert_aggregation_opts validates with test_data", {
  agg <- aggregation_observed()
  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_no_error(assert_aggregation_opts(agg, test_data = test_matrix))
})

test_that("assert_aggregation_opts errors when ref_time fails on test_data", {
  bad_agg <- aggregation_opts(
    ref_time = function(x) stop("deliberate error"),
    delay = function(x) rowSums(x)
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_error(
    assert_aggregation_opts(bad_agg, test_data = test_matrix),
    regexp = "ref_time.*failed"
  )
})

test_that("assert_aggregation_opts errors when delay fails on test_data", {
  bad_agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) stop("deliberate error")
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_error(
    assert_aggregation_opts(bad_agg, test_data = test_matrix),
    regexp = "delay.*failed"
  )
})

test_that("assert_aggregation_opts errors when ref_time returns non-matrix", {
  bad_agg <- aggregation_opts(
    ref_time = function(x) as.vector(x),
    delay = function(x) rowSums(x)
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_error(
    assert_aggregation_opts(bad_agg, test_data = test_matrix),
    regexp = "ref_time.*must return a matrix"
  )
})

test_that("assert_aggregation_opts errors when delay returns non-numeric", {
  bad_agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) as.character(rowSums(x))
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)

  expect_error(
    assert_aggregation_opts(bad_agg, test_data = test_matrix),
    regexp = "delay.*must return a numeric"
  )
})

test_that("assert_aggregation_opts requires matrix for test_data", {
  agg <- aggregation_observed()

  expect_error(
    assert_aggregation_opts(agg, test_data = c(1, 2, 3)),
    regexp = "matrix"
  )
})

# Custom Aggregation Examples --------------------------------------------------

test_that("aggregation_opts works with rolling sum", {
  skip_if_not_installed("zoo")

  agg <- aggregation_opts(
    ref_time = function(x) zoo::rollsum(x, k = 3, align = "right", fill = NA),
    delay = function(x) rowSums(x, na.rm = TRUE)
  )

  test_matrix <- matrix(1:12, nrow = 4, ncol = 3)
  result <- agg$ref_time(test_matrix)

  expect_true(is.matrix(result))
  expect_identical(dim(result), dim(test_matrix))
})

test_that("aggregation_opts works with column aggregation", {
  agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) colSums(x, na.rm = TRUE)
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  result <- agg$delay(test_matrix)

  expected <- colSums(test_matrix, na.rm = TRUE)
  expect_identical(result, expected)
})

test_that("aggregation_opts works with matrix subset", {
  agg <- aggregation_opts(
    ref_time = function(x) x[nrow(x), , drop = FALSE],
    delay = function(x) rowSums(x, na.rm = TRUE)
  )

  test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  result <- agg$ref_time(test_matrix)

  expect_true(is.matrix(result))
  expect_identical(nrow(result), 1L)
  expect_identical(ncol(result), ncol(test_matrix))
})

# Integration Tests ------------------------------------------------------------

test_that("aggregation_opts can be used in uncertainty_opts", {
  agg <- aggregation_observed()
  opts <- uncertainty_opts(aggregation = agg)

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$aggregation, agg)
})

test_that("custom aggregation can be used in uncertainty_opts", {
  agg <- aggregation_opts(
    ref_time = identity,
    delay = function(x) colSums(x)
  )

  opts <- uncertainty_opts(aggregation = agg)

  expect_s3_class(opts, "uncertainty_opts")
  expect_identical(opts$aggregation, agg)
})
