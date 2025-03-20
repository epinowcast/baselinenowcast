test_that("convert_rep_square_to_df function mimics pivot longer", {
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

  result <- convert_reporting_square_to_df(test_matrix)
  ### Test 1: Matches pivot longer output-----------------------------------
  # result using pivot longer
  df <- as.data.frame(test_matrix)
  df$time <- seq_len(nrow(df))
  df_long <- df |>
    tidyr::pivot_longer(
      cols = starts_with("V"),
      names_to = "delay",
      names_prefix = "V",
      values_to = "count"
    )
  df_long$delay <- as.integer(df_long$delay)

  expect_identical(result$time, df_long$time)
  expect_identical(result$delay, df_long$delay)
  expect_identical(result$count, df_long$count)
})

test_that("convert_reporting_square_to_df works correctly", {
  ### Test 2: Basic Functionality ----------------------------------------------

  # Verify output structure
  expect_s3_class(result, "data.frame")
  expect_identical(colnames(result), c("time", "delay", "count"))
  expect_identical(nrow(result), nrow(test_matrix) * ncol(test_matrix))

  # Verify time/delay sequences
  expect_identical(result$time, rep(1:6, each = 4))
  expect_identical(result$delay, rep(1:4, times = 6))

  ### Test 3: Value Preservation -----------------------------------------------
  # Check values are ordered correctly (row-wise)
  expected_values <- as.vector(t(test_matrix))
  expect_identical(result$count, expected_values)

  ### Test 4: Draw Parameter Handling ------------------------------------------
  # With draw specified
  result_draw <- convert_reporting_square_to_df(test_matrix, draw = 5)
  expect_true("draw" %in% colnames(result_draw))
  expect_true(all(result_draw$draw == 5))

  # Without draw specified
  expect_false("draw" %in% colnames(result))

  ### Test 5: Edge Cases -------------------------------------------------------
  # Single row matrix
  single_row <- matrix(1:3, nrow = 1)
  result_single <- convert_reporting_square_to_df(single_row)
  expect_identical(nrow(result_single), 3L)
  expect_identical(unique(result_single$time), 1L)

  # Matrix with NA values still works
  na_matrix <- test_matrix
  na_matrix[2, 2] <- NA
  result_na <- convert_reporting_square_to_df(na_matrix)
  expect_true(anyNA(result_na$count))

  ### Test 6: Input Validation -------------------------------------------------
  # Non-matrix input, should still work
  expect_silent(convert_reporting_square_to_df(as.data.frame(test_matrix)))

  # Invalid draw type
  expect_error(convert_reporting_square_to_df(test_matrix, draw = "invalid"))
})

test_that("Column ordering and naming works correctly", {
  # Matrix with named columns
  named_matrix <- matrix(1:4, nrow = 2)
  result_named <- convert_reporting_square_to_df(named_matrix)

  # Verify column names are ignored in conversion
  expect_identical(colnames(result_named), c("time", "delay", "count"))
})

test_that("Large matrix handling", {
  # Create 100x100 matrix
  large_matrix <- matrix(runif(10000), nrow = 100)
  result_large <- convert_reporting_square_to_df(large_matrix)

  # Verify dimensions
  expect_identical(nrow(result_large), 100L * 100L)
  expect_identical(max(result_large$time), 100L)
  expect_identical(max(result_large$delay), 100L)
})
