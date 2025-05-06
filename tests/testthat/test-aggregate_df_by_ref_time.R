# Setup test data
example_df <- data.frame(
  time = c(1, 1, 1, 1, 2, 2, 2, 2),
  delay = c(1, 2, 1, 2, 1, 2, 1, 2),
  draw = c(1, 1, 2, 2, 1, 1, 2, 2),
  count = c(3, 6, 4, 7, 1, 2, 2, 3)
)

test_that("aggregate_df_by_ref_time correctly sums counts", {
  result <- aggregate_df_by_ref_time(example_df)
  expected <- data.frame(
    time = c(1, 2, 1, 2),
    draw = c(1, 1, 2, 2),
    total_count = c(9, 3, 11, 5)
  )
  expect_identical(result$total_count, expected$total_count)
})

test_that("aggregate_df_by_ref_time handles single-row groups", {
  single_row_df <- data.frame(
    time = 1, delay = 1, draw = 1, count = 5
  )
  expect_identical(aggregate_df_by_ref_time(single_row_df)$total_count, 5)
})

test_that("aggregate_df_by_ref_time handles zero counts", {
  zero_df <- data.frame(
    time = c(1, 1),
    delay = c(1, 2),
    draw = c(1, 1),
    count = c(0, 0)
  )
  expect_identical(aggregate_df_by_ref_time(zero_df)$total_count, 0)
})

test_that("aggregate_df_by_ref_time ignores extra columns", {
  extra_col_df <- cbind(example_df, extra = rnorm(nrow(example_df)))
  expected <- data.frame(
    time = c(1, 2, 1, 2),
    draw = c(1, 1, 2, 2),
    total_count = c(9, 3, 11, 5)
  )
  expect_identical(
    aggregate_df_by_ref_time(extra_col_df),
    expected
  )
})

test_that("aggregate_df_by_ref_time validates input columns", {
  no_count_df <- example_df[, -which(names(example_df) == "count")]
  expect_error(aggregate_df_by_ref_time(no_count_df), "Names must include")
})

test_that("Column order and names are correct", {
  result <- aggregate_df_by_ref_time(example_df)
  expect_named(result, c("time", "draw", "total_count"))
  expect_type(result$time, "double")
  expect_type(result$draw, "double")
  expect_type(result$total_count, "double")
})
