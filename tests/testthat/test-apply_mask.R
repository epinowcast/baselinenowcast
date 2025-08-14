test_that(".apply_mask returns matrix with correct dimensions", {
  mat <- matrix(1:12, nrow = 3, ncol = 4)
  indices_1 <- matrix(
    c(
      TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
      FALSE, TRUE, TRUE, FALSE, TRUE, FALSE
    ),
    nrow = 3, ncol = 4
  )
  indices_2 <- matrix(
    c(
      TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      TRUE, FALSE, TRUE, TRUE, FALSE, TRUE
    ),
    nrow = 3, ncol = 4
  )

  result <- .apply_mask(mat, indices_1, indices_2)

  expect_is(result, "matrix")
  expect_identical(dim(result), dim(mat))
})

test_that(".apply_mask works with a single row matrix", {
  mat <- matrix(1:12, nrow = 1, ncol = 12)
  indices_1 <- matrix(
    c(
      TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
      FALSE, TRUE, TRUE, FALSE, TRUE, FALSE
    ),
    nrow = 1, ncol = 12
  )
  indices_2 <- matrix(
    c(
      TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,
      TRUE, FALSE, TRUE, TRUE, FALSE, TRUE
    ),
    nrow = 1, ncol = 12
  )

  result <- .apply_mask(mat, indices_1, indices_2)

  expect_is(result, "matrix")
  expect_identical(dim(result), dim(mat))
  expect_gt(result[1], 0)
  expect_gt(result[4], 0)
  expect_gt(result[9], 0)
  expect_identical(result[2], 0L)
})

test_that(".apply_mask correctly applies logical AND operation", {
  mat <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
  indices_1 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2)
  indices_2 <- matrix(c(TRUE, TRUE, FALSE, TRUE), nrow = 2, ncol = 2)

  expected <- matrix(c(1, 0, 0, 4), nrow = 2, ncol = 2)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles all TRUE indices", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  indices_1 <- matrix(TRUE, nrow = 2, ncol = 3)
  indices_2 <- matrix(TRUE, nrow = 2, ncol = 3)

  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, mat)
})

test_that(".apply_mask handles all FALSE indices in first mask", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  indices_1 <- matrix(FALSE, nrow = 2, ncol = 3)
  indices_2 <- matrix(TRUE, nrow = 2, ncol = 3)

  expected <- matrix(0L, nrow = 2, ncol = 3)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles all FALSE indices in second mask", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  indices_1 <- matrix(TRUE, nrow = 2, ncol = 3)
  indices_2 <- matrix(FALSE, nrow = 2, ncol = 3)

  expected <- matrix(0L, nrow = 2, ncol = 3)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles matrices with NA values", {
  mat <- matrix(c(1, NA, 3, 4), nrow = 2, ncol = 2)
  indices_1 <- matrix(c(TRUE, TRUE, FALSE, TRUE), nrow = 2, ncol = 2)
  indices_2 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2)

  expected <- matrix(c(1, 0, NA, 4), nrow = 2, byrow = TRUE, ncol = 2)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles matrices with negative values", {
  mat <- matrix(c(-1, 2, -3, 4), nrow = 2, ncol = 2)
  indices_1 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2)
  indices_2 <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2, ncol = 2)

  expected <- matrix(c(-1, 0, -3, 0), nrow = 2, ncol = 2)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles single element matrices", {
  mat <- matrix(5, nrow = 1, ncol = 1)
  indices_1 <- matrix(TRUE, nrow = 1, ncol = 1)
  indices_2 <- matrix(FALSE, nrow = 1, ncol = 1)

  expected <- matrix(0, nrow = 1, ncol = 1)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask handles decimal values", {
  mat <- matrix(c(1.5, 2.7, 3.14, 4.0), nrow = 2, ncol = 2)
  indices_1 <- matrix(c(TRUE, TRUE, FALSE, TRUE), nrow = 2, ncol = 2)
  indices_2 <- matrix(c(FALSE, TRUE, TRUE, TRUE), nrow = 2, ncol = 2)

  expected <- matrix(c(0, 2.7, 0, 4.0), nrow = 2, ncol = 2)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})

test_that(".apply_mask preserves matrix class and structure", {
  mat <- matrix(1:9, nrow = 3, ncol = 3)
  indices_1 <- matrix(sample(c(TRUE, FALSE), 9, replace = TRUE),
    nrow = 3, ncol = 3
  )
  indices_2 <- matrix(sample(c(TRUE, FALSE), 9, replace = TRUE),
    nrow = 3, ncol = 3
  )

  result <- .apply_mask(mat, indices_1, indices_2)

  expect_is(result, "matrix")
  expect_identical(nrow(result), 3L)
  expect_identical(ncol(result), 3L)
})

test_that(".apply_mask zero multiplication property", {
  # Test that FALSE * anything = 0
  mat <- matrix(c(100, -50, 0, 999), nrow = 2, ncol = 2)
  indices_1 <- matrix(c(FALSE, FALSE, FALSE, FALSE), nrow = 2, ncol = 2)
  indices_2 <- matrix(c(TRUE, TRUE, TRUE, TRUE), nrow = 2, ncol = 2)

  expected <- matrix(0, nrow = 2, ncol = 2)
  result <- .apply_mask(mat, indices_1, indices_2)

  expect_identical(result, expected)
})
