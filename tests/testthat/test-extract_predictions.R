test_that("function returns correct matrix for example case", {
  # Setup example from documentation
  full_mat <- matrix(
    c(
      1, 3, 5, 7,
      4, 7, 8, 9,
      9, 10, 3, 5,
      3, 4, 8, 5
    ),
    nrow = 4,
    byrow = TRUE
  )

  subset_mat <- matrix(
    c(
      1, 3, 5, 7,
      4, 7, 8, NA,
      9, 10, NA, NA,
      3, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Expected result
  expected_result <- matrix(
    c(
      NA, NA, NA, NA,
      NA, NA, NA, 9,
      NA, NA, 3, 5,
      NA, 4, 8, 5
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Run function
  result <- extract_missing_elements(full_mat, subset_mat)

  # Test
  expect_equal(result, expected_result)
})

test_that("function returns matrix of correct dimensions", {
  # Test with different matrix sizes
  full_mat <- matrix(1:20, nrow = 5)
  subset_mat <- matrix(c(1:15, NA, NA, NA, NA, NA), nrow = 5)

  result <- extract_missing_elements(full_mat, subset_mat)

  # Check dimensions
  expect_equal(dim(result), dim(full_mat))
  expect_equal(dim(result), dim(subset_mat))
})

test_that("function handles case where subset_mat has no NAs", {
  # In this case, all elements in result should be NA
  full_mat <- matrix(1:9, nrow = 3)
  subset_mat <- matrix(1:9, nrow = 3)

  result <- extract_missing_elements(full_mat, subset_mat)

  # All elements should be NA
  expect_true(all(is.na(result)))
})

test_that("function handles case where subset_mat is all NAs", {
  # Should throw an error due to assert_matrix(subset_mat, all.missing = FALSE)
  full_mat <- matrix(1:9, nrow = 3)
  subset_mat <- matrix(NA, nrow = 3, ncol = 3)

  expect_error(extract_missing_elements(full_mat, subset_mat),
    regexp = "Contains only missing values",
    class = "checkmate_error"
  )
})

test_that("function errors when full_mat contains NAs", {
  # Should throw an error due to assert_matrix(full_mat, any.missing = FALSE)
  full_mat <- matrix(c(1:8, NA), nrow = 3)
  subset_mat <- matrix(c(1:4, NA, NA, NA, NA, NA), nrow = 3)

  expect_error(extract_missing_elements(full_mat, subset_mat),
    regexp = "Contains missing values",
    class = "checkmate_error"
  )
})

test_that("function errors when matrices have different dimensions", {
  full_mat <- matrix(1:12, nrow = 3)
  subset_mat <- matrix(c(1:4, NA, NA), nrow = 2)

  expect_error(extract_missing_elements(full_mat, subset_mat))
})

test_that("function errors when non-NA values in subset_mat differ from full_mat", {
  full_mat <- matrix(1:9, nrow = 3)
  subset_mat <- matrix(c(1, 2, 3, 4, 99, 6, 7, NA, NA), nrow = 3) # 99 != 5

  expect_error(extract_missing_elements(full_mat, subset_mat),
    regexp = "is not a subset of"
  )
})

test_that("function works with single-element matrices", {
  full_mat <- matrix(5, nrow = 1)
  subset_mat <- matrix(NA, nrow = 1)

  result <- extract_missing_elements(full_mat, subset_mat)

  expect_equal(result, matrix(5, nrow = 1))
})

test_that("function works with large matrices", {
  # Create larger matrices for performance testing
  set.seed(123)
  n <- 100
  full_mat <- matrix(runif(n * n), nrow = n)

  # Create subset with ~50% NA values
  subset_mat <- full_mat
  na_indices <- sample(1:(n * n), size = floor(n * n / 2))
  subset_mat[na_indices] <- NA

  # Run function
  result <- extract_missing_elements(full_mat, subset_mat)

  # Check basic properties
  expect_equal(dim(result), c(n, n))

  # Check that only NA elements in subset_mat have values in result
  expect_true(all(is.na(result[!is.na(subset_mat)])))
  expect_true(all(!is.na(result[is.na(subset_mat)])))

  # Check values match original full_mat where they should
  expect_equal(result[is.na(subset_mat)], full_mat[is.na(subset_mat)])
})

test_that("function preserves row and column names", {
  full_mat <- matrix(1:9, nrow = 3)
  rownames(full_mat) <- c("A", "B", "C")
  colnames(full_mat) <- c("X", "Y", "Z")

  subset_mat <- full_mat
  subset_mat[c(2, 5, 8)] <- NA

  result <- extract_missing_elements(full_mat, subset_mat)

  expect_equal(rownames(result), c("A", "B", "C"))
  expect_equal(colnames(result), c("X", "Y", "Z"))
})

test_that("function works with sparse matrices", {
  # Create a mostly-NA matrix as the subset
  full_mat <- matrix(1:100, nrow = 10)
  subset_mat <- matrix(NA, nrow = 10, ncol = 10)

  # Only a few values are non-NA
  subset_mat[1, 1] <- 1
  subset_mat[5, 5] <- 45
  subset_mat[10, 10] <- 100

  result <- extract_missing_elements(full_mat, subset_mat)

  # Check dimensions
  expect_equal(dim(result), c(10, 10))

  # Check result has NAs where subset_mat has values
  expect_true(is.na(result[1, 1]))
  expect_true(is.na(result[5, 5]))
  expect_true(is.na(result[10, 10]))

  # Check a few values that should be preserved
  expect_equal(result[1, 2], 11)
  expect_equal(result[6, 6], 56)
  expect_equal(result[9, 10], 90)
})
