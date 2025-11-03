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
  reference_date = as.Date(c("2021-04-06", "2021-04-06", "2021-04-06", "2021-04-06")), # nolint
  report_date = as.Date(c("2021-04-08", "2021-04-08", "2021-04-10", "2021-04-10")), # nolint
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "00+", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)
expected_cols <- c("reference_date", "report_date", "count")
strata_cols <- c("location", "age_group")
test_that(".combine_triangle_dfs combines data across strata correctly", {
  result <- .combine_triangle_dfs(
    data = example_data,
    strata_cols = strata_cols
  )
  # Correct columns
  expect_true(all(expected_cols %in% colnames(result)))
  expect_false("location" %in% names(result))
  expect_false("age_group" %in% names(result))

  expect_identical(nrow(result), 2L)

  row1 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-08"), ]
  expect_identical(row1$count, 80)

  row2 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-10"), ]
  expect_identical(row2$count, 60)

  result2 <- expect_warning(
    .combine_triangle_dfs(
      data = test_data_partial_overlap,
      strata_cols = strata_cols
    ),
    regexp = "Not all reference dates and report dates combinations are available" # nolint
  )
  # Correct columns
  expect_true(all(expected_cols %in% colnames(result2)))
  expect_false("location" %in% names(result2))
  expect_false("age_group" %in% names(result2))

  expect_identical(nrow(result2), 1L)

  row1 <- result2[result2$reference_date == as.Date("2021-04-06") &
    result2$report_date == as.Date("2021-04-08"), ]
  expect_identical(row1$count, 80)
})

test_that(".combine_triangle_dfs errors if no set of shared reference and report dates exists", { # nolint
  expect_error(
    .combine_triangle_dfs(
      data = test_data_to_fail,
      strata_cols = strata_cols
    ),
    regexp = "There is no overlapping set of reference and report dates across" # nolint
  )
  # This one fails because the dates for 05-14 for DE aren't overlapping with
  # dates for 00+
  expect_error(
    .combine_triangle_dfs(
      data = test_data_also_fail,
      strata_cols = strata_cols
    ),
    regexp = "There is no overlapping set of reference and report dates across" # nolint
  )
})


test_that(".combine_triangle_dfs handles all same date combinations by summing everything", { # nolint
  test_data <- data.frame(
    reference_date = as.Date(rep("2021-04-06", 4)),
    report_date = as.Date(rep("2021-04-08", 4)),
    location = c("DE", "FR", "IT", "ES"),
    count = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  result <- .combine_triangle_dfs(
    data = test_data,
    strata_cols = "location"
  )
  expect_identical(nrow(result), 1L)
  expect_identical(result$count, 100)
})



test_that(".combine_triangle_dfs handles multiple strata columns", {
  test_data <- data.frame(
    reference_date = as.Date(c(rep("2021-04-06", 8))),
    report_date = as.Date(rep("2021-04-08", 8)),
    location = rep(c("DE", "FR"), each = 4),
    age_group = rep(c("00+", "05-14"), 4),
    sex = rep(c("M", "F"), 4),
    count = c(10, 15, 20, 25, 30, 35, 40, 45),
    stringsAsFactors = FALSE
  )

  result <- .combine_triangle_dfs(
    data = test_data,
    strata_cols = c(strata_cols, "sex")
  )

  # Should collapse all strata
  expect_identical(nrow(result), 1L)
  expect_identical(result$count, sum(test_data$count))

  # Strata columns should be removed
  expect_false("location" %in% names(result))
  expect_false("age_group" %in% names(result))
  expect_false("sex" %in% names(result))
})

test_that(".combine_triangle_dfs works with numeric counts", {
  test_data <- data.frame(
    reference_date = as.Date(c("2021-04-06", "2021-04-06")),
    report_date = as.Date(c("2021-04-08", "2021-04-08")),
    count = c(10.5, 20.3)
  )

  result <- .combine_triangle_dfs(
    data = test_data
  )
  expect_equal(result$count, 30.8, tolerance = 1e-10)
})
