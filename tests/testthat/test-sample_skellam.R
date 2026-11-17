test_that("sample_skellam() returns numeric vector matching length of `pred`", {
  set.seed(123)
  pred <- c(5, 10, 20, 50)
  out <- sample_skellam(pred, uncertainty_params = 100)
  expect_type(out, "integer")
  expect_length(out, length(pred))
})

test_that("sample_skellam() returns mean and variance close to parameters", {
  set.seed(42)
  out <- sample_skellam(rep(5, 10000), uncertainty_params = 20)
  expect_equal(mean(out), 5, tolerance = 0.1)

  set.seed(42)
  out <- sample_skellam(rep(0, 10000), uncertainty_params = 20)
  expect_equal(mean(out), 0, tolerance = 0.1)
  expect_equal(var(out), 20, tolerance = 0.1)
})

test_that("sample_skellam() can sample negative values", {
  set.seed(42)
  out <- sample_skellam(rep(-5, 100), uncertainty_params = 10)
  expect_true(any(out < 0))
  expect_equal(mean(out), -5, tolerance = 0.2)
})

test_that("sample_skellam() is reproducible under set.seed", {
  pred <- c(3.2, 4.6, 10.1)
  set.seed(7)
  out1 <- sample_skellam(pred, uncertainty_params = 50)
  set.seed(7)
  out2 <- sample_skellam(pred, uncertainty_params = 50)
  expect_identical(out1, out2)
})

test_that("sample_skellam() handles scalar vs vector uncertainty_params", {
  pred <- c(5, 10, 20)
  set.seed(2024)
  scalar_out <- sample_skellam(pred, uncertainty_params = 100)
  set.seed(2024)
  vector_out <- sample_skellam(pred, uncertainty_params = rep(100, 3))
  expect_identical(scalar_out, vector_out)
})

test_that("sample_skellam() preserves matrix shape and dimnames when `pred` is a matrix", { # nolint
  set.seed(99)
  pred <- matrix(c(5, 10, 20, 50),
    nrow = 2, ncol = 2,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  out <- sample_skellam(pred, uncertainty_params = 100)
  expect_true(is.matrix(out))
  expect_identical(dim(out), dim(pred))
  expect_identical(dimnames(out), dimnames(pred))
})

test_that("sample_skellam() errors when variance is not greater than |pred|", {
  expect_error(
    sample_skellam(5, uncertainty_params = 4),
    regexp = "variance must be greater than \\|pred\\|"
  )
  expect_error(
    sample_skellam(c(1, -5), uncertainty_params = 5),
    regexp = "variance must be greater than \\|pred\\|"
  )
})

test_that("sample_skellam() with robust = TRUE widens invalid variances", {
  set.seed(42)
  out <- expect_no_error(
    sample_skellam(rep(5, 10000), uncertainty_params = 4, robust = TRUE)
  )
  expect_equal(mean(out), 5, tolerance = 0.1)
})

test_that("sample_skellam() with robust = TRUE leaves valid variances unchanged", { # nolint
  pred <- rep(5, 100)
  set.seed(42)
  out_robust <- sample_skellam(pred, uncertainty_params = 20, robust = TRUE)
  set.seed(42)
  out_default <- sample_skellam(pred, uncertainty_params = 20)
  expect_identical(out_robust, out_default)
})

test_that("sample_skellam() errors on invalid input", {
  expect_error(
    sample_skellam("a", uncertainty_params = 10),
    regexp = "`pred` must be numeric"
  )
  expect_error(
    sample_skellam(c(1, 2, 3), uncertainty_params = "a"),
    regexp = "`uncertainty_params` must be numeric"
  )
  expect_error(
    sample_skellam(c(1, 2, 3), uncertainty_params = c(10, 20)),
    regexp = "`uncertainty_params` must have length 1 or match length"
  )
  expect_error(
    sample_skellam(c(1, 2, 3), uncertainty_params = -1),
    regexp = "`uncertainty_params` must be non-negative"
  )
  expect_error(
    sample_skellam(c(1, 2, 3), uncertainty_params = 10, robust = "yes"),
    regexp = "robust"
  )
})
