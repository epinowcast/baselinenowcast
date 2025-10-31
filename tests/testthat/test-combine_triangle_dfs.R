test_data_to_fail <- data.frame(
  reference_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-07")), # nolint
  report_date = as.Date(c("2021-04-08", "2021-04-07", "2021-04-10", "2021-04-09")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "05-14", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

test_data_partial_overlap <- data.frame(
  reference_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-07")), # nolint
  report_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-09")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "00+", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

test_data_also_fail <- data.frame(
  reference_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-07")), # nolint
  report_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-09")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "05-14", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

example_data <- data.frame(
  ref_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-06")), # nolint
  rep_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-10")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "00+", "00+"),
  cases = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

test_that("combine_triangle_dfs combines data across strata correctly", {
  result <- combine_triangle_dfs(
    data = example_data,
    reference_date = "ref_date",
    report_date = "rep_date",
    count = "cases"
  )
  # Correct columns
  expect_false("location" %in% names(result))
  expect_false("age_group" %in% names(result))
  expect_true("ref_date" %in% names(result))
  expect_true("rep_date" %in% names(result))
  expect_true("cases" %in% names(result))

  expect_identical(nrow(result), 2L)

  row1 <- result[result$ref_date == as.Date("2021-04-06") &
    result$rep_date == as.Date("2021-04-08"), ]
  expect_identical(row1$cases, 80)

  row2 <- result[result$ref_date == as.Date("2021-04-06") &
    result$rep_date == as.Date("2021-04-10"), ]
  expect_identical(row2$cases, 60)

  result2 <- expect_warning(
    combine_triangle_dfs(
      data = test_data_partial_overlap
    ),
    regexp = "Not all reference dates and report dates combinations are available" # nolint
  )
  # Correct columns
  expect_false("location" %in% names(result2))
  expect_false("age_group" %in% names(result2))
  expect_true("reference_date" %in% names(result2))
  expect_true("report_date" %in% names(result2))
  expect_true("count" %in% names(result2))

  expect_identical(nrow(result2), 1L)

  row1 <- result2[result2$reference_date == as.Date("2021-04-06") &
    result2$report_date == as.Date("2021-04-08"), ]
  expect_identical(row1$count, 80)
})

test_that("combine_triangle_dfs errors if no set of shared reference and report dates exists", { # nolint
  expect_error(
    combine_triangle_dfs(
      data = test_data_to_fail,
      reference_date = "reference_date",
      report_date = "report_date",
      count = "count"
    ),
    regexp = "There is no overlapping set of reference and report dates across" # nolint
  )
  # This one fails because the dates for 05-14 for DE aren't overlapping with
  # dates for 00+
  expect_error(
    combine_triangle_dfs(
      data = test_data_also_fail,
      reference_date = "reference_date",
      report_date = "report_date",
      count = "count"
    ),
    regexp = "There is no overlapping set of reference and report dates across" # nolint
  )
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
