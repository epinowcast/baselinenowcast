# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that("as_reporting_triangle.data.frame() works as expected", { # nolint
  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
  expected_structure <- 1
  expect_identical(rep_tri$structure, expected_structure)

  # even if we add other columns
  data_as_of_df$test_col <- 2

  rep_tri2 <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )
  expect_identical(
    nrow(rep_tri2$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
  expected_structure <- 1
  expect_identical(rep_tri2$structure, expected_structure)

  # or lower max delay
  rep_tri3 <- as_reporting_triangle(data_as_of_df,
    max_delay = 20
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri3$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
})

test_that("as_reporting_triangle.data.frame() errors if max delay is too large", { # nolint
  expect_error(
    as_reporting_triangle(data_as_of_df,
      max_delay = 500
    ),
    regexp = "`max_delay` specified is larger than the maximum delay in the data." # nolint
  )
})

test_that("as_reporting_triangle.data.frame() can handle different temporal granularities", { # nolint
  skip_if_not_installed("lubridate")
  skip_if_not_installed("dplyr")
  weekly_weekly <- data_as_of_df |>
    dplyr::filter(
      lubridate::wday(reference_date) == 1,
      lubridate::wday(report_date) == 1
    )

  rep_tri <- as_reporting_triangle(weekly_weekly,
    max_delay = 3,
    delays_unit = "weeks"
  )
  expected_days_diff <- as.numeric(difftime(
    rep_tri$reference_dates[2],
    rep_tri$reference_dates[1]
  ))
  expect_identical(expected_days_diff, 7)
  expect_true(is.matrix(rep_tri$reporting_triangle_matrix))
  expect_identical(rep_tri$delays_unit, "weeks")

  # Now just filter the reference dates, keep all delays (needed for doing the
  # weekday filtering)
  weekly_daily <- data_as_of_df |>
    dplyr::filter(lubridate::wday(reference_date) == 1)
  rep_tri2 <- expect_warning(as_reporting_triangle(weekly_daily,
    max_delay = 25,
    delays_unit = "days"
  ))
  expected_days_diff <- as.numeric(difftime(
    rep_tri2$reference_dates[2],
    rep_tri2$reference_dates[1]
  ))
  expect_identical(expected_days_diff, 7)
  expect_true(is.matrix(rep_tri2$reporting_triangle_matrix))
  expect_identical(rep_tri2$delays_unit, "days")

  # Now just filter the report dates to simulate reporting on one day of the
  # week but with reference dates for all days
  daily_weekly <- data_as_of_df |>
    dplyr::filter(lubridate::wday(report_date) == 1)
  rep_tri3 <- as_reporting_triangle(daily_weekly,
    max_delay = 25,
    delays_unit = "days"
  )
  expected_days_diff <- as.numeric(difftime(
    rep_tri3$reference_dates[2],
    rep_tri3$reference_dates[1]
  ))
  expect_identical(expected_days_diff, 1)
  expect_true(is.matrix(rep_tri3$reporting_triangle_matrix))
  expect_identical(rep_tri3$delays_unit, "days")
  # we expect a lot of 0s in the reporting triangle because it gets
  # converted to daily daily so test for this
  n_zeros <- sum(rep_tri3$reporting_triangle_matrix == 0, na.rm = TRUE)
  n_elements <- nrow(rep_tri3$reporting_triangle_matrix) * ncol(rep_tri3$reporting_triangle_matrix) # nolint
  prop_zeros <- n_zeros / n_elements
  expect_gt(prop_zeros, 5 / 7)

  # Check for user errors:
  # User specifies weekly but data is daily -- should error because delays
  # are not integers
  expect_error(
    expect_warning(
      as_reporting_triangle(data_as_of_df,
        max_delay = 25,
        delays_unit = "weeks"
      )
    ),
    regexp = "Check that `delays_unit` is specified correctly."
  )

  # User specifies daily but the data is weekly. -- this will create a daily
  # matrix with 0s for all the missing report dates
  rep_tri4 <- expect_warning(
    as_reporting_triangle(weekly_weekly,
      max_delay = 25,
      delays_unit = "days"
    ),
    regexp = "Data does not contain case counts for all possible reference dates." # nolint
  ) # nolint
  expect_identical(
    ncol(rep_tri4$reporting_triangle_matrix),
    ncol(rep_tri2$reporting_triangle_matrix)
  )
})
test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- expect_warning(
    as_reporting_triangle(test,
      max_delay = 25
    ),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(test$reference_date))
  )

  # calculate the expected structure which will be all 1s except for 1 2 where
  # ref date is missing.
  expected_structure <- c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) # nolint
  expect_identical(rep_tri$structure, expected_structure)
})

test_that("as_reporting_triangle.data.frame() errors if there are duplicate pairs of reference and report dates", { # nolint
  df_dup <- rbind(data_as_of_df, data_as_of_df)
  expect_error(
    as_reporting_triangle(df_dup,
      max_delay = 25
    ),
    regexp = "Data contains duplicate `reference_date` and `report_date` combinations" # nolint
  ) # nolint
})

test_that("as_reporting_triangle.data.frame() can handle different column names", { # nolint
  data <- data_as_of_df
  old_names <- c("reference_date", "report_date", "count")
  new_names <- c("ref_date", "rep_date", "cases")
  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  rep_tri <- as_reporting_triangle(data,
    max_delay = 25,
    reference_date = "ref_date",
    report_date = "rep_date",
    count = "cases"
  )
  expect_identical(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(data$ref_date))
  )
})

test_that("as_reporting_triangle.data.frame() errors if missing required columns", { # nolint
  data <- data_as_of_df[-1] # remove a column
  expect_error(
    as_reporting_triangle(data,
      max_delay = 25
    ),
    regexp = "Required columns missing from data"
  )
})


test_that("as_reporting_triangle.data.frame() returns appropriate strata", { # nolint
  data_as_of_df$age_group <- "00+"
  data_as_of_df$location <- "south"
  rep_tri <- as_reporting_triangle(
    data_as_of_df,
    max_delay = 25,
    strata = c("age_group", "location")
  )
  exp_strata_list <- list(age_group = "00+", location = "south")

  expect_identical(exp_strata_list, rep_tri$strata)

  # Just pass one
  rep_tri <- as_reporting_triangle(
    data_as_of_df,
    max_delay = 25,
    strata = "age_group"
  )
  exp_strata_list <- list(age_group = "00+")

  expect_identical(exp_strata_list, rep_tri$strata)
})

test_that("as_reporting_triangle.data.frame() errors if multiple strata", { # nolint
  data_as_of_df$age_group <- "00+"
  data_as_of_df$age_group[1:10] <- "5-14"
  expect_error(
    as_reporting_triangle(
      data_as_of_df,
      max_delay = 25,
      strata = "age_group"
    ),
    regexp = "Multiple values found for the specified `strata` when trying to create " # nolint
  )
  expect_error(
    as_reporting_triangle(
      data_as_of_df,
      max_delay = 25,
      strata = "region"
    ),
    regexp = "`strata` specified are not columns in `data`"
  )
})


test_that("as_reporting_triangle.matrix() can handle specification of each arg", { # nolint
  rep_tri_mat <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, NA,
      9, 10, 0, NA, NA,
      3, 0, NA, NA, NA,
      6, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )

  reference_dates <- seq(
    from = as.Date("2025-01-01"),
    to = as.Date("2025-01-05"),
    by = "day"
  )
  max_delay <- 4

  rep_tri <- as_reporting_triangle(
    data = rep_tri_mat,
    reference_dates = reference_dates,
    max_delay = max_delay
  )
  expect_identical(rep_tri$reporting_triangle_matrix, rep_tri_mat)
  expect_identical(rep_tri$reference_date, reference_dates)
  expect_identical(rep_tri$structure, 1)
})

test_that("as_reporting_triangle.matrix() errors if reference dates don't align with rows of the matrix", { # nolint
  rep_tri_mat <- matrix(
    c(
      1, 3, 5, 7, 9,
      4, 7, 8, 0, NA,
      9, 10, 0, NA, NA,
      3, 0, NA, NA, NA,
      6, NA, NA, NA, NA
    ),
    nrow = 5,
    byrow = TRUE
  )

  reference_dates <- seq.Date(
    from = as.Date("2025-01-01"),
    to = as.Date("2025-01-06"),
    by = "day"
  )
  max_delay <- 4

  expect_error(
    as_reporting_triangle(
      data = rep_tri_mat,
      reference_dates = reference_dates,
      max_delay = max_delay
    ),
    regexp = "Length of `reference_dates` must equal number of rows in"
  ) # nolint
})

test_that("`as_reporting_triangle.data.frame()` inputs are of the right type", {
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      reference_date = 4
    ),
    regexp = "Assertion on 'reference_date' failed: Must be of type 'character', not 'double'." # nolint
  )
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      report_date = 4
    ),
    regexp = "Assertion on 'report_date' failed: Must be of type 'character', not 'double'." # nolint
  )
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      count = 4
    ),
    regexp = "Assertion on 'count' failed: Must be of type 'character', not 'double'." # nolint
  )
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      delays_unit = 4
    ),
    regexp = "Assertion on 'delays_unit' failed: Must be of type 'character', not 'double'." # nolint
  )
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      strata = 4
    ),
    regexp = "Assertion on 'strata' failed:"
  )
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      delays_unit = "daily"
    ),
    regexp = "Assertion on 'delays_unit' failed:"
  )
})

test_that("assert on reporting triangle works as expected", {
  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )
  expect_s3_class(rep_tri, "reporting_triangle")
  expect_no_error(assert_reporting_triangle(rep_tri))

  rep_tri1 <- rep_tri
  rep_tri1$reference_dates <- c(1, 3, 4)
  expect_error(assert_reporting_triangle(rep_tri1))

  rep_tri2 <- rep_tri
  rep_tri2$max_delay <- 0
  expect_error(assert_reporting_triangle(rep_tri2))

  rep_tri3 <- rep_tri
  rep_tri3$strata <- NULL
  expect_no_error(assert_reporting_triangle(rep_tri3))
  rep_tri3$strata <- list(region = "south")
  expect_no_error(assert_reporting_triangle(rep_tri3))
  rep_tri3$strata <- 6
  expect_error(assert_reporting_triangle(rep_tri3))

  rep_tri4 <- rep_tri
  rep_tri4$delays_unit <- "months"
  expect_no_error(assert_reporting_triangle(rep_tri4))
  rep_tri4$delays_unit <- "month"
  expect_error(assert_reporting_triangle(rep_tri4))
  rep_tri4$delays_unit <- months
  expect_error(assert_reporting_triangle(rep_tri4))
  rep_tri4$delays_unit <- 8
  expect_error(assert_reporting_triangle(rep_tri4))

  rep_tri5 <- rep_tri
  rep_tri5$structure <- 1
  expect_no_error(assert_reporting_triangle(rep_tri5))
  rep_tri5$structure <- c(3, 4)
  expect_no_error(assert_reporting_triangle(rep_tri5))
  rep_tri5$structure <- rep(6, ncol(rep_tri$reporting_triangle_matrix))
  expect_error(assert_reporting_triangle(rep_tri5))
})
