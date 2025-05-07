test_that("nowcast_matrix_list_to_df basic functionality with valid input", {
  # Create test matrices
  mat1 <- matrix(1:4, nrow = 2, byrow = TRUE)
  mat2 <- matrix(5:8, nrow = 2, byrow = TRUE)
  nowcast_list <- list(mat1, mat2)

  result <- nowcast_matrix_list_to_df(nowcast_list)

  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_identical(colnames(result), c("time", "delay", "count", "draw"))
  expect_identical(nrow(result), 8L) # 2 matrices * 2 rows * 2 cols
})

test_that("nowcast_matrix_list_to_df draw numbers are correctly assigned", {
  mat1 <- matrix(1:4, nrow = 2)
  mat2 <- matrix(5:8, nrow = 2)
  nowcast_list <- list(mat1, mat2)

  result <- nowcast_matrix_list_to_df(nowcast_list)

  # Verify draw numbers
  expect_identical(unique(result$draw), c(1L, 2L))
  expect_identical(sum(result$draw == 1), 4L) # 2x2 matrix
  expect_identical(sum(result$draw == 2), 4L)
})

test_that("nowcast_matrix_list_to_df NA values are preserved in output", {
  na_matrix <- matrix(c(1, NA, 3, 4), nrow = 2)
  nowcast_list <- list(na_matrix)

  result <- nowcast_matrix_list_to_df(nowcast_list)
  expect_true(anyNA(result$count))
})

test_that("nowcast_matrix_list_to_df input validation works correctly", {
  # Non-matrix element in list
  invalid_list <- list(matrix(1:4, nrow = 2), data.frame(a = 1:2))
  expect_error(nowcast_matrix_list_to_df(invalid_list))

  # Empty list
  expect_error(nowcast_matrix_list_to_df(list()))
})

test_that(
  "nowcast_matrix_list_to_df handles matrices of different dimensions",
  {
    mat1 <- matrix(1:4, nrow = 2) # 2x2
    mat2 <- matrix(1:6, nrow = 3) # 3x2
    nowcast_list <- list(mat1, mat2)

    result <- nowcast_matrix_list_to_df(nowcast_list)
    expect_identical(nrow(result), 10L) # 4 + 6 = 10 rows
  }
)

test_that("nowcast_matrix_list_to_df single matrix handling", {
  single_mat <- matrix(1:4, nrow = 2)
  result <- nowcast_matrix_list_to_df(list(single_mat))

  expect_identical(unique(result$draw), 1L)
  expect_identical(nrow(result), 4L)
})

test_that("nowcast_matrix_list_to_df column values match original matrices", {
  test_matrix <- matrix(c(10, 20, 30, 40), nrow = 2, byrow = TRUE)
  nowcast_list <- list(test_matrix)

  result <- nowcast_matrix_list_to_df(nowcast_list)

  # Check values are ordered correctly (row-wise)
  expected_values <- as.vector(t(test_matrix))
  expect_identical(result$count, expected_values)

  # Check time/delay mapping
  expect_identical(result$time, c(1L, 1L, 2L, 2L))
  expect_identical(result$delay, c(1L, 2L, 1L, 2L))
})
