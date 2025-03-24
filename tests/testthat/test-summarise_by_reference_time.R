test_that("summarise_by_reference_time works correctly", {
  # Setup test data
  example_df <- data.frame(
    time = c(1, 1, 1, 1, 2, 2, 2, 2),
    delay = c(1, 2, 1, 2, 1, 2, 1, 2),
    draw = c(1, 1, 2, 2, 1, 1, 2, 2),
    count = c(3, 6, 4, 7, 1, 2, 2, 3)
  )

  # Test 1: Correctly sums counts for example data
  result <- summarise_by_reference_time(example_df)
  expected <- data.frame(
    time = c(1, 2, 1, 2),
    draw = c(1, 1, 2, 2),
    total_count = c(9, 3, 11, 5)
  )
  expect_identical(result$total_count, expected$total_count)

  # Test 2: Handles single-row groups
  single_row_df <- data.frame(
    time = 1, delay = 1, draw = 1, count = 5
  )
  expect_identical(summarise_by_reference_time(single_row_df)$total_count, 5)

  # Test 3: Handles zero counts
  zero_df <- data.frame(
    time = c(1, 1), delay = c(1, 2), draw = c(1, 1), count = c(0, 0)
  )
  expect_identical(summarise_by_reference_time(zero_df)$total_count, 0)

  # Test 4: Ignores extra columns
  extra_col_df <- cbind(example_df, extra = rnorm(nrow(example_df)))
  expect_identical(
    summarise_by_reference_time(extra_col_df),
    expected
  )

  # Test 5: Input validation
  no_count_df <- example_df[, -which(names(example_df) == "count")]
  expect_error(summarise_by_reference_time(no_count_df), "Names must include")
})

test_that("Column order and names are correct", {
  result <- summarise_by_reference_time(example_df)
  expect_named(result, c("time", "draw", "total_count"))
  expect_type(result$time, "double")
  expect_type(result$draw, "double")
  expect_type(result$total_count, "double")
})
