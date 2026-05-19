test_that("sample_nb() returns numeric vector matching length of `pred`", {
  set.seed(123)
  pred <- c(5, 10, 20, 50)
  out <- sample_nb(pred, uncertainty_params = 100)
  expect_type(out, "double")
  expect_length(out, length(pred))
})

test_that("sample_nb() returns mean close to `pred` under large uncertainty", {
  set.seed(42)
  pred <- rep(20, 2000)
  out <- sample_nb(pred, uncertainty_params = 1e8)
  expect_equal(mean(out), 20, tolerance = 0.05)
})

test_that("sample_nb() is reproducible under set.seed", {
  pred <- c(3.2, 4.6, 10.1)
  set.seed(7)
  out1 <- sample_nb(pred, uncertainty_params = 50)
  set.seed(7)
  out2 <- sample_nb(pred, uncertainty_params = 50)
  expect_identical(out1, out2)
})

test_that("sample_nb() returns all zeros when `pred` is all zero", {
  set.seed(1)
  pred <- rep(0, 5)
  out <- sample_nb(pred, uncertainty_params = 10)
  expect_identical(out, rep(0, 5))
})

test_that("sample_nb() handles scalar vs vector uncertainty_params", {
  pred <- c(5, 10, 20)
  set.seed(2024)
  scalar_out <- sample_nb(pred, uncertainty_params = 100)
  set.seed(2024)
  vector_out <- sample_nb(pred, uncertainty_params = rep(100, 3))
  expect_identical(scalar_out, vector_out)
})

test_that("sample_nb() preserves matrix shape and dimnames when `pred` is a matrix", { # nolint
  set.seed(99)
  pred <- matrix(c(5, 10, 20, 50),
    nrow = 2, ncol = 2,
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  out <- sample_nb(pred, uncertainty_params = 1e6)
  expect_true(is.matrix(out))
  expect_identical(dim(out), dim(pred))
  expect_identical(dimnames(out), dimnames(pred))
})

test_that("sample_nb() returns NA for NA elements in `pred`", {
  set.seed(5)
  pred <- c(5, NA, 10)
  out <- suppressWarnings(sample_nb(pred, uncertainty_params = 100))
  expect_length(out, 3)
  expect_true(is.na(out[2]))
  expect_false(is.na(out[1]))
  expect_false(is.na(out[3]))
})

test_that("sample_nb() errors on invalid input", {
  expect_error(
    sample_nb("a", uncertainty_params = 10),
    regexp = "`pred` must be numeric"
  )
  expect_error(
    sample_nb(c(1, 2, 3), uncertainty_params = "a"),
    regexp = "`uncertainty_params` must be numeric"
  )
  expect_error(
    sample_nb(c(1, 2, 3), uncertainty_params = c(10, 20)),
    regexp = "`uncertainty_params` must have length 1 or match length"
  )
})
