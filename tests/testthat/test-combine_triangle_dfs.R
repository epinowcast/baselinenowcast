# Test-specific constants
test_data_partial_overlap <- data.frame(
  reference_date = as.Date(c(
    "2021-04-06", "2021-04-06", "2021-04-06",
    "2021-04-07"
  )),
  report_date = as.Date(c(
    "2021-04-08", "2021-04-08", "2021-04-10",
    "2021-04-09"
  )),
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "00+", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

example_data <- data.frame(
  reference_date = as.Date(c(
    "2021-04-06", "2021-04-06", "2021-04-06",
    "2021-04-06"
  )),
  report_date = as.Date(c(
    "2021-04-08", "2021-04-08", "2021-04-10",
    "2021-04-10"
  )),
  location = c("DE", "FR", "DE", "FR"),
  age_group = c("00+", "00+", "00+", "00+"),
  count = c(50, 30, 20, 40),
  stringsAsFactors = FALSE
)

expected_cols <- c("reference_date", "report_date", "count")
strata_cols <- c("location", "age_group")
covid_data <- germany_covid19_hosp[
  germany_covid19_hosp$report_date <=
    max(germany_covid19_hosp$reference_date) &
    germany_covid19_hosp$age_group %in% c("00+", "60-79", "80+"),
]
covid_data$weekday_ref_date <- lubridate::wday(
  covid_data$reference_date,
  label = TRUE
)
covid_data_single_strata <- dplyr::filter(covid_data, age_group == "00+")

test_that(".combine_triangle_dfs combines data across strata correctly", {
  result <- .combine_triangle_dfs(
    data = example_data,
    strata_cols = strata_cols
  )

  expect_columns_present(result, expected_cols)
  expect_columns_absent(result, c("location", "age_group"))
  expect_identical(nrow(result), 2L)

  row1 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-08"), ]
  expect_identical(row1$count, sum(example_data$count[1:2]))

  row2 <- result[result$reference_date == as.Date("2021-04-06") &
    result$report_date == as.Date("2021-04-10"), ]
  expect_identical(row2$count, sum(example_data$count[3:4]))

  result2 <- expect_warning_partial_overlap(
    .combine_triangle_dfs(
      data = test_data_partial_overlap,
      strata_cols = strata_cols
    )
  )

  expect_columns_present(result2, expected_cols)
  expect_columns_absent(result2, c("location", "age_group"))
  expect_identical(nrow(result2), 1L)
  expect_identical(
    sum(test_data_partial_overlap$count[1:2]),
    result2$count[1]
  )
})

test_that(".combine_triangle_dfs can handle weekday with overlaps.", {
  skip_if_not_installed("lubridate")
  example_data_wday <- example_data
  example_data_wday$weekday_ref_date <- lubridate::wday(
    example_data_wday$reference_date,
    label = TRUE
  )
  result4 <- .combine_triangle_dfs(
    data = example_data_wday,
    strata_cols = c(strata_cols, "weekday_ref_date")
  )
  expect_identical(result4$count[1], sum(example_data_wday$count[1:2]))

  test_data_overlap_wday <- test_data_partial_overlap
  test_data_overlap_wday$weekday_ref_date <- lubridate::wday(
    test_data_overlap_wday$reference_date,
    label = TRUE
  )
  expect_error_no_overlap(
    .combine_triangle_dfs(
      data = test_data_overlap_wday,
      strata_cols = c(strata_cols, "weekday_ref_date")
    )
  )
})

test_that(paste0(
  ".combine_triangle_dfs errors if no reference and ",
  "report dates exists"
), {
  test_data_to_fail <- data.frame(
    reference_date = as.Date(c(
      "2021-04-06", "2021-04-06", "2021-04-06",
      "2021-04-07"
    )),
    report_date = as.Date(c(
      "2021-04-07", "2021-04-08", "2021-04-10",
      "2021-04-09"
    )),
    location = c("DE", "FR", "DE", "FR"),
    age_group = c("00+", "00+", "05-14", "00+"),
    count = c(50, 30, 20, 40),
    stringsAsFactors = FALSE
  )
  test_data_also_fail <- data.frame(
    reference_date = as.Date(c(
      "2021-04-06", "2021-04-06", "2021-04-06",
      "2021-04-07"
    )),
    report_date = as.Date(c(
      "2021-04-08", "2021-04-08", "2021-04-10",
      "2021-04-09"
    )),
    location = c("DE", "FR", "DE", "FR"),
    age_group = c("00+", "00+", "05-14", "00+"),
    count = c(50, 30, 20, 40),
    stringsAsFactors = FALSE
  )
  skip_if_not_installed("dplyr")
  expect_error_no_overlap(
    .combine_triangle_dfs(
      data = test_data_to_fail,
      strata_cols = strata_cols
    )
  )
  expect_error_no_overlap(
    .combine_triangle_dfs(
      data = test_data_also_fail,
      strata_cols = strata_cols
    )
  )
})


test_that(paste0(
  ".combine_triangle_dfs handles all same date combinations ",
  "by summing everything"
), {
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

  expect_identical(nrow(result), 1L)
  expect_identical(result$count, sum(test_data$count))
  expect_columns_absent(result, c("location", "age_group", "sex"))
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

test_that(paste0(
  ".combine_triangles_df returns the the full set of ",
  "reference and report dates when there is only one strata"
), {
  skip_if_not_installed("dplyr")
  df1 <- dplyr::arrange(
    covid_data_single_strata,
    reference_date, report_date
  )
  result <- .combine_triangle_dfs(
    data = df1,
    strata_cols = strata_cols
  ) |> dplyr::arrange(reference_date, report_date)
  expect_identical(result$count, df1$count)
})

test_that(paste0(
  ".combine_triangles_df returns the sum across multiple age ",
  "groups correctly given complete and non overlapping dates"
), {
  skip_if_not_installed("dplyr")
  df2 <- dplyr::arrange(
    covid_data,
    reference_date, report_date
  )
  test2 <- .combine_triangle_dfs(df2,
    strata_cols = strata_cols
  ) |>
    dplyr::arrange(reference_date, report_date)
  expect_identical(test2$count[1], sum(df2$count[1:3]))

  df3 <- df2[-1, ]
  test3 <- expect_warning_partial_overlap(
    .combine_triangle_dfs(df3,
      strata_cols = strata_cols
    )
  )
  test3 <- dplyr::arrange(
    test3,
    reference_date, report_date
  )
  expect_identical(test3$count, test2$count[2:nrow(test2)])
})
