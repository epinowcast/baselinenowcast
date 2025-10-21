test_data <- data.frame(
  reference_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-07")), # nolint
  report_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-09")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "05-14", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

test_that("combine_triangle_dfs combines data across strata correctly", {
  result <- combine_triangle_dfs(
    data = test_data,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count"
  )
  # Correct columns
  expect_false("location" %in% names(result))
  expect_false("age_group" %in% names(result))
  expect_true("reference_date" %in% names(result))
  expect_true("report_date" %in% names(result))
  expect_true("count" %in% names(result))

  expect_identical(nrow(result), 3L)

  row1 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-08"), ]
  expect_identical(row1$count, 80)

  row2 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-10"), ]
  expect_identical(row2$count, 20)
})

test_that("combine_triangle_dfs returns correct column names", {
  test_data <- data.frame(
    ref_date = as.Date(c("2021-04-06", "2021-04-06")),
    rpt_date = as.Date(c("2021-04-08", "2021-04-08")),
    n = c(10, 20),
    stringsAsFactors = FALSE
  )

  result <- combine_triangle_dfs(
    data = test_data,
    reference_date = "ref_date",
    report_date = "rpt_date",
    count = "n"
  )

  expect_named(result, c("ref_date", "rpt_date", "n"))
})


test_that("combine_triangle_dfs handles all same date combinations by summing everything", { # nolint
  test_data <- data.frame(
    reference_date = as.Date(rep("2021-04-06", 4)),
    report_date = as.Date(rep("2021-04-08", 4)),
    location = c("DE", "FR", "IT", "ES"),
    count = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  result <- combine_triangle_dfs(
    data = test_data,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count"
  )

  expect_identical(nrow(result), 1L)
  expect_identical(result$count, 100)
})



test_that("combine_triangle_dfs handles multiple strata columns", {
  test_data <- data.frame(
    reference_date = as.Date(rep("2021-04-06", 8)),
    report_date = as.Date(rep("2021-04-08", 8)),
    location = rep(c("DE", "FR"), each = 4),
    age_group = rep(c("00+", "05-14"), 4),
    sex = rep(c("M", "F"), 4),
    count = c(10, 15, 20, 25, 30, 35, 40, 45),
    stringsAsFactors = FALSE
  )

  result <- combine_triangle_dfs(
    data = test_data,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count"
  )

  # Should collapse all strata
  expect_identical(nrow(result), 1L)
  expect_identical(result$count, sum(test_data$count))

  # Strata columns should be removed
  expect_false("location" %in% names(result))
  expect_false("age_group" %in% names(result))
  expect_false("sex" %in% names(result))
})

test_that("combine_triangle_dfs works with numeric counts", {
  test_data <- data.frame(
    reference_date = as.Date(c("2021-04-06", "2021-04-06")),
    report_date = as.Date(c("2021-04-08", "2021-04-08")),
    count = c(10.5, 20.3)
  )

  result <- combine_triangle_dfs(
    data = test_data,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count"
  )

  expect_equal(result$count, 30.8, tolerance = 1e-10)
})
