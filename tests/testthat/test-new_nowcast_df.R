data_df <- data.frame(
  time = rep(1:10, 3),
  draw = rep.int(1:3, times = 10),
  pred_count = sample.int(20, 30, replace = TRUE)
)
ref_dates <- seq(
  from = as.Date("2025-01-01"), to = as.Date("2025-01-10"),
  by = "day"
)
strata <- c("00+", "south")
test_that("new_nowcast_df() creates a dataframe from a set of dates and named list of strata", { # nolint
  new_df <- new_nowcast_df(data_df,
    reference_dates = ref_dates,
    strata = strata
  )

  expected_cols <- c("draw", "pred_count", "strata")

  expect_s3_class(new_df, "data.frame")
  expect_identical(nrow(new_df), 30L)

  expect_true(all(expected_cols %in% colnames(new_df)))

  # time corresponds to reference dates starting at 1
  expected_dates <- min(ref_dates) + lubridate::days(unique(data_df$time) - 1)
  expect_equal(expected_dates, sort(unique(new_df$reference_date))) # nolint
  expect_identical(new_df$strata, rep("00+-south", 30))



  # no additional columns if no strata are specified

  new_df_ns <- new_nowcast_df(data_df,
    reference_dates = ref_dates,
    strata = NULL
  )
  expected_cols <- c("draw", "pred_count", "reference_date")
  expect_identical(expected_cols, colnames(new_df_ns))
})

test_that("new_nowcast_df correctly merges reference dates", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 2, 3),
    pred_count = c(100, 200, 300),
    draw = c(1, 1, 1)
  )
  strata <- NULL
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))

  result <- new_nowcast_df(nowcast_df, strata, reference_dates)

  expect_identical(result$reference_date, reference_dates)
  expect_identical(result$pred_count, c(100, 200, 300))
})

test_that("new_nowcast_df removes time column from output", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 2, 3),
    pred_count = c(10, 20, 30),
    draw = c(1, 1, 1)
  )
  strata <- "0-17"
  reference_dates <- as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))
  result <- new_nowcast_df(nowcast_df, strata, reference_dates)

  expect_false("time" %in% names(result))
})

test_that("new_nowcast_df handles non-sequential time values", {
  # Setup
  nowcast_df <- data.frame(
    time = c(1, 3, 5),
    pred_count = c(10, 30, 50),
    draw = c(1, 1, 1)
  )
  strata <- "East"
  reference_dates <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2024-01-04", "2024-01-05"
  ))

  result <- new_nowcast_df(nowcast_df, strata, reference_dates)

  expect_identical(nrow(result), 3L)
  expect_identical(
    result$reference_date,
    as.Date(c("2024-01-01", "2024-01-03", "2024-01-05"))
  )
})

test_that("new_nowcast_df handles multiple strata", {
  nowcast_df <- data.frame(
    time = c(1, 3, 5),
    pred_count = c(10, 30, 50),
    draw = c(1, 1, 1)
  )
  strata <- c("East", "00-17")
  reference_dates <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2024-01-04", "2024-01-05"
  ))

  result <- new_nowcast_df(nowcast_df, strata, reference_dates)

  expected_val <- "East-00-17"
  expect_identical(result$strata[1], expected_val)
  expect_length(unique(result$strata), 1)
})
