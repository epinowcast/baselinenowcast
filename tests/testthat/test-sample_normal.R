test_that("sample_normal() returns numeric vector matching length of `pred`", {
  set.seed(123)
  pred <- c(5, 10, 20, 50)
  out <- sample_normal(pred, uncertainty_params = 4)
  expect_type(out, "double")
  expect_length(out, length(pred))
})

test_that("sample_normal() returns integer-valued draws", {
  set.seed(123)
  out <- sample_normal(c(5.3, 10.7, -2.2), uncertainty_params = 4)
  expect_identical(out, round(out))
})

test_that("sample_normal() returns mean and variance close to parameters", {
  set.seed(42)
  out <- sample_normal(rep(100, 10000), uncertainty_params = 4)
  expect_equal(mean(out), 100, tolerance = 0.005)

  set.seed(42)
  out <- sample_normal(rep(0, 10000), uncertainty_params = 9)
  expect_equal(mean(out), 0, tolerance = 0.1)
  expect_equal(var(out), 9, tolerance = 0.1)
})

test_that("sample_normal() returns `pred` when the variance is zero", {
  pred <- c(5, 10, -3)
  expect_identical(sample_normal(pred, uncertainty_params = 0), pred)
})

test_that("sample_normal() can sample negative values", {
  set.seed(42)
  out <- sample_normal(rep(-5, 100), uncertainty_params = 4)
  expect_true(any(out < 0))
})

test_that("sample_normal() is reproducible under set.seed", {
  pred <- c(3.2, 4.6, 10.1)
  set.seed(7)
  out1 <- sample_normal(pred, uncertainty_params = 5)
  set.seed(7)
  out2 <- sample_normal(pred, uncertainty_params = 5)
  expect_identical(out1, out2)
})

test_that("sample_normal() handles scalar vs vector uncertainty_params", {
  pred <- c(5, 10, 20)
  set.seed(2024)
  scalar_out <- sample_normal(pred, uncertainty_params = 4)
  set.seed(2024)
  vector_out <- sample_normal(pred, uncertainty_params = rep(4, 3))
  expect_identical(scalar_out, vector_out)
})

test_that("sample_normal() preserves matrix shape and dimnames when `pred` is a matrix", { # nolint
  set.seed(99)
  pred <- matrix(c(5, 10, 20, 50),
    nrow = 2, ncol = 2,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  out <- sample_normal(pred, uncertainty_params = 4)
  expect_true(is.matrix(out))
  expect_identical(dim(out), dim(pred))
  expect_identical(dimnames(out), dimnames(pred))
})

test_that("sample_normal() errors on invalid input", {
  expect_error(
    sample_normal("a", uncertainty_params = 10),
    regexp = "`pred` must be numeric"
  )
  expect_error(
    sample_normal(c(1, 2, 3), uncertainty_params = "a"),
    regexp = "`uncertainty_params` must be numeric"
  )
  expect_error(
    sample_normal(c(1, 2, 3), uncertainty_params = c(10, 20)),
    regexp = "`uncertainty_params` must have length 1 or match length"
  )
  expect_error(
    sample_normal(c(1, 2, 3), uncertainty_params = -1),
    regexp = "`uncertainty_params` must be non-negative"
  )
  expect_error(
    sample_normal(c(1, 2, 3), uncertainty_params = NA_real_),
    regexp = "`uncertainty_params` must be non-negative"
  )
})
