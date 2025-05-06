test_matrix <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, 18,
    80, 40, 24, 16,
    70, 35, 21, 19,
    67, 34, 15, 9
  ),
  nrow = 6,
  byrow = TRUE
)

result <- nowcast_matrix_to_df(test_matrix)

pivoted_longer_df <- data.frame(
  time = rep(1:6, each = 4),
  delay = rep(1:4, 6),
  count = as.vector(t(test_matrix))
)


test_that("nowcast_matrix_to_df function mimics pivot longer", {
  expect_identical(result$time, pivoted_longer_df$time)
  expect_identical(result$delay, pivoted_longer_df$delay)
  expect_identical(result$count, pivoted_longer_df$count)
})

test_that("nowcast_matrix_to_df works correctly", {
  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_identical(colnames(result), c("time", "delay", "count"))
  expect_identical(nrow(result), nrow(test_matrix) * ncol(test_matrix))

  # Verify time/delay sequences
  expect_identical(result$time, rep(1:6, each = 4))
  expect_identical(result$delay, rep(1:4, times = 6))

  # Check values are ordered correctly (row-wise)
  expected_values <- as.vector(t(test_matrix))
  expect_identical(result$count, expected_values)

  ### Test 4: Draw Parameter Handling ------------------------------------------
  # With draw specified
  result_draw <- nowcast_matrix_to_df(test_matrix, draw = 5)
  expect_true("draw" %in% colnames(result_draw))
  expect_true(all(result_draw$draw == 5))

  # Without draw specified
  expect_false("draw" %in% colnames(result))

  ### Test 5: Edge Cases -------------------------------------------------------
  # Single row matrix
  single_row <- matrix(1:3, nrow = 1)
  result_single <- nowcast_matrix_to_df(single_row)
  expect_identical(nrow(result_single), 3L)
  expect_identical(unique(result_single$time), 1L)

  # Matrix with NA values still works
  na_matrix <- test_matrix
  na_matrix[2, 2] <- NA
  result_na <- nowcast_matrix_to_df(na_matrix)
  expect_true(anyNA(result_na$count))

  ### Test 6: Input Validation -------------------------------------------------
  # Non-matrix input, should still work
  expect_silent(nowcast_matrix_to_df(as.data.frame(test_matrix)))

  # Invalid draw type
  expect_error(nowcast_matrix_to_df(test_matrix, draw = "invalid"))
})

test_that("nowcast_matrix_to_df column ordering and naming works correctly", {
  # Matrix with named columns
  named_matrix <- matrix(1:4, nrow = 2)
  result_named <- nowcast_matrix_to_df(named_matrix)

  # Verify column names are ignored in conversion
  expect_identical(colnames(result_named), c("time", "delay", "count"))
})

test_that("nowcast_matrix_to_df large matrix handling", {
  # Create 100x100 matrix
  large_matrix <- matrix(runif(10000), nrow = 100)
  result_large <- nowcast_matrix_to_df(large_matrix)

  # Verify dimensions
  expect_identical(nrow(result_large), 100L * 100L)
  expect_identical(max(result_large$time), 100L)
  expect_identical(max(result_large$delay), 100L)
})
