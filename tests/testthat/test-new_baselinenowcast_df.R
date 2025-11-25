ref_dates <- seq(
  from = as.Date("2025-01-01"), to = as.Date("2025-01-10"),
  by = "day"
)
data_df <- data.frame(
  reference_date = rep(ref_dates, 3),
  draw = rep.int(1:3, times = 10),
  pred_count = sample.int(20, 30, replace = TRUE)
)
output_type <- "samples"
test_that("new_baselinenowcast_df() creates a dataframe from a set of dates and named list of strata", { # nolint
  new_df <- new_baselinenowcast_df(data_df,
    reference_dates = ref_dates,
    output_type = output_type
  )

  expected_cols <- c("draw", "pred_count", "reference_date", "output_type")

  expect_s3_class(new_df, "data.frame")
  expect_identical(nrow(new_df), 30L)
  expect_true(all(expected_cols %in% colnames(new_df)))

  # Check reference dates are preserved
  expect_identical(sort(unique(new_df$reference_date)), ref_dates)
})

test_that("new_baselinenowcast_df correctly adds output_type column", {
  # Setup
  nowcast_df <- data.frame(
    reference_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    pred_count = c(100, 200, 300),
    draw = c(1, 1, 1)
  )
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  result <- new_baselinenowcast_df(
    nowcast_df,
    reference_dates,
    output_type
  )

  expect_identical(result$reference_date, reference_dates)
  expect_identical(result$pred_count, c(100, 200, 300))
  expect_identical(result$output_type[1], "samples")
})

test_that("new_baselinenowcast_df preserves reference_date column", {
  # Setup
  nowcast_df <- data.frame(
    reference_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    pred_count = c(10, 20, 30),
    draw = c(1, 1, 1)
  )
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  result <- new_baselinenowcast_df(nowcast_df, reference_dates, output_type)

  expect_true("reference_date" %in% names(result))
  expect_identical(result$reference_date, reference_dates)
})

test_that("new_baselinenowcast_df orders by reference_date and draw", {
  # Setup with out-of-order data
  nowcast_df <- data.frame(
    reference_date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-02")),
    pred_count = c(30, 10, 20),
    draw = c(1, 1, 1)
  )
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  result <- new_baselinenowcast_df(nowcast_df, reference_dates, output_type)

  expect_identical(nrow(result), 3L)
  expect_identical(
    result$reference_date,
    as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  )
  expect_identical(result$pred_count, c(10, 20, 30))
})

test_that("new_baselinenowcast_df errors if incorrect `output_type`", {
  expect_error(
    new_baselinenowcast_df(data_df,
      reference_dates = ref_dates,
      output_type = "pt"
    ),
    regexp = "Assertion on 'output_type' failed" # nolint
  )
})
