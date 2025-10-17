data_df <- data.frame(
  time = rep(1:10, 3),
  draw = rep.int(1:3, times = 10),
  pred_count = sample.int(20, 30, replace = TRUE)
)
ref_dates <- seq(
  from = as.Date("2025-01-01"), to = as.Date("2025-01-10"),
  by = "day"
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

  # time corresponds to reference dates starting at 1
  expected_dates <- min(ref_dates) + lubridate::days(unique(data_df$time) - 1)
  expect_equal(expected_dates, sort(unique(new_df$reference_date))) # nolint
})

test_that("new_baselinenowcast_df correctly merges reference dates", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 2, 3),
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

test_that("new_baselinenowcast_df removes time column from output", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 2, 3),
    pred_count = c(10, 20, 30),
    draw = c(1, 1, 1)
  )
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  result <- new_baselinenowcast_df(nowcast_df, reference_dates, output_type)

  expect_false("time" %in% names(result))
})

test_that("new_baselinenowcast_df handles non-sequential time values", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 3, 5),
    pred_count = c(10, 30, 50),
    draw = c(1, 1, 1)
  )
  reference_dates <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2024-01-04", "2024-01-05"
  ))

  result <- new_baselinenowcast_df(nowcast_df, reference_dates, output_type)

  expect_identical(nrow(result), 3L)
  expect_identical(
    result$reference_date,
    as.Date(c("2024-01-01", "2024-01-03", "2024-01-05"))
  )
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
