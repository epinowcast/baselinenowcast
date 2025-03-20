test_that("Basic functionality with valid input", {
  # Create test matrices
  mat1 <- matrix(1:4, nrow = 2, byrow = TRUE)
  mat2 <- matrix(5:8, nrow = 2, byrow = TRUE)
  nowcast_list <- list(mat1, mat2)

  result <- generate_prob_nowcast_df(nowcast_list)

  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_identical(colnames(result), c("time", "delay", "count", "draw"))
  expect_identical(nrow(result), 8L) # 2 matrices * 2 rows * 2 cols
})

test_that("Draw numbers are correctly assigned", {
  mat1 <- matrix(1:4, nrow = 2)
  mat2 <- matrix(5:8, nrow = 2)
  nowcast_list <- list(mat1, mat2)

  result <- generate_prob_nowcast_df(nowcast_list)

  # Verify draw numbers
  expect_identical(unique(result$draw), c(1L, 2L))
  expect_identical(sum(result$draw == 1), 4L) # 2x2 matrix
  expect_identical(sum(result$draw == 2), 4L)
})

test_that("NA values are preserved in output", {
  na_matrix <- matrix(c(1, NA, 3, 4), nrow = 2)
  nowcast_list <- list(na_matrix)

  result <- generate_prob_nowcast_df(nowcast_list)
  expect_true(any(is.na(result$count)))
})

test_that("Input validation works correctly", {
  # Non-matrix element in list
  invalid_list <- list(matrix(1:4, nrow = 2), data.frame(a = 1:2))
  expect_error(generate_prob_nowcast_df(invalid_list))

  # Empty list
  expect_error(generate_prob_nowcast_df(list()))
})

test_that("Handles matrices of different dimensions", {
  mat1 <- matrix(1:4, nrow = 2) # 2x2
  mat2 <- matrix(1:6, nrow = 3) # 3x2
  nowcast_list <- list(mat1, mat2)

  result <- generate_prob_nowcast_df(nowcast_list)
  expect_identical(nrow(result), 10L) # 4 + 6 = 10 rows
})

test_that("Single matrix handling", {
  single_mat <- matrix(1:4, nrow = 2)
  result <- generate_prob_nowcast_df(list(single_mat))

  expect_identical(unique(result$draw), 1L)
  expect_identical(nrow(result), 4L)
})

test_that("Column values match original matrices", {
  test_matrix <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
  nowcast_list <- list(test_matrix)

  result <- generate_prob_nowcast_df(nowcast_list)

  # Check values are ordered correctly (row-wise)
  expected_values <- as.vector(t(test_matrix))
  expect_equal(result$count, expected_values)

  # Check time/delay mapping
  expect_equal(result$time, c(1, 1, 2, 2))
  expect_equal(result$delay, c(1, 2, 1, 2))
})
