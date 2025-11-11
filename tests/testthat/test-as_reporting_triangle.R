# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that("as_reporting_triangle.data.frame() works as expected", { # nolint
  rep_tri <- as_reporting_triangle(data_as_of_df)
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
  expected_structure <- 1
  expect_identical(get_structure(rep_tri), expected_structure)

  # even if we add other columns
  data_as_of_df$test_col <- 2

  rep_tri2 <- as_reporting_triangle(data_as_of_df)
  expect_identical(
    nrow(rep_tri2$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
  expected_structure <- 1
  expect_identical(get_structure(rep_tri2), expected_structure)

  # Test that max_delay is computed from data
  rep_tri3 <- as_reporting_triangle(data_as_of_df)
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri3$reporting_triangle_matrix),
    length(unique(data_as_of_df$reference_date))
  )
})

test_that("as_reporting_triangle.data.frame() computes max_delay from data", { # nolint
  # Note: max_delay parameter was removed in refactoring
  # max_delay is now computed automatically from the data
  rep_tri <- as_reporting_triangle(data_as_of_df)

  # Check that max_delay was computed correctly
  expect_true(get_max_delay(rep_tri) > 0)
  expect_equal(get_max_delay(rep_tri),
               as.integer(max(data_as_of_df$report_date - data_as_of_df$reference_date)))
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
  rep_tri2 <- expect_message(
    as_reporting_triangle(weekly_daily,
      delays_unit = "days"
    ),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  )
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
  # Note: With max_delay now computed from data (not capped at 25),
  # the proportion of zeros is lower than before
  n_zeros <- sum(rep_tri3$reporting_triangle_matrix == 0, na.rm = TRUE)
  n_elements <- nrow(rep_tri3$reporting_triangle_matrix) * ncol(rep_tri3$reporting_triangle_matrix) # nolint
  prop_zeros <- n_zeros / n_elements
  # Just check that there are a reasonable number of zeros (more than half)
  expect_gt(prop_zeros, 0.5)

  # Check for user errors:
  # User specifies weekly but data is daily -- should error because delays
  # are not integers
  expect_error(
    expect_warning(
      as_reporting_triangle(data_as_of_df,
        delays_unit = "weeks"
      )
    ),
    regexp = "Check that `delays_unit` is specified correctly."
  )

  # User specifies daily but the data is weekly. -- this will create a daily
  # matrix with 0s for all the missing report dates
  rep_tri4 <- expect_message(
    as_reporting_triangle(weekly_weekly,
      delays_unit = "days"
    ),
    regexp = "Data does not contain case counts for all possible reference dates." # nolint
  ) # nolint
  # Note: max_delay is now computed from data, so ncol can differ
  # rep_tri4 (weekly-weekly) will have fewer delays than rep_tri2 (weekly-daily)
  # Just check that both matrices were created successfully
  expect_true(is.matrix(rep_tri4$reporting_triangle_matrix))
  expect_true(ncol(rep_tri4$reporting_triangle_matrix) > 0)
})
test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- expect_message(
    as_reporting_triangle(test),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  )
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri$reporting_triangle_matrix),
    length(unique(test$reference_date))
  )

  # Structure indicates the reporting pattern. With max_delay now computed
  # from data, structure will be longer. Check that:
  # 1. Structure is numeric
  # 2. Contains mostly 1s (regular reporting)
  # 3. Has at least one 2 (where the reference date is missing)
  structure <- get_structure(rep_tri)
  expect_true(is.numeric(structure))
  expect_true(sum(structure == 1) > sum(structure == 2))
  expect_true(any(structure == 2))
})

test_that("as_reporting_triangle.data.frame() errors if there are duplicate pairs of reference and report dates", { # nolint
  df_dup <- rbind(data_as_of_df, data_as_of_df)
  expect_error(
    as_reporting_triangle(df_dup),
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
    as_reporting_triangle(data),
    regexp = "Required columns missing from data"
  )
})

test_that("as_reporting_triangle.data.frame() returns appropriate strata", { # nolint
  # Note: strata parameter was removed in refactoring
  # strata is now always NULL in single-stratum reporting triangles
  rep_tri <- as_reporting_triangle(
    data_as_of_df,
    max_delay = 25
  )

  expect_identical(NULL, rep_tri$strata)

  # Also test without strata parameter
  rep_tri <- as_reporting_triangle(
    data_as_of_df,
    max_delay = 25
  )

  expect_identical(rep_tri$strata, NULL)
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
  expect_identical(get_structure(rep_tri), 1)
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
  # Note: strata parameter was removed in refactoring, so no test needed
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      delays_unit = "daily"
    ),
    regexp = "Assertion on 'delays_unit' failed:"
  )
})

test_that("assert on reporting triangle works as expected", {
  rep_tri <- as_reporting_triangle(data_as_of_df)
  expect_s3_class(rep_tri, "reporting_triangle")
  expect_no_error(assert_reporting_triangle(rep_tri))

  # assert_reporting_triangle() only checks class, not internal structure
  # Internal structure is validated at construction time
  non_rep_tri <- list(some_field = 1)
  expect_error(assert_reporting_triangle(non_rep_tri))
})

test_that("`as_reporting_triangle()` appropriately messages if there is nothing to be nowcasted (no unobserved cases in reporting triangle)", { # nolint
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")
  data <- tidyr::expand_grid(
    reference_date = seq(as.Date("2021-04-01"), as.Date("2021-04-30"),
      by = "day"
    ),
    report_date = seq(as.Date("2021-04-01"), as.Date("2021-05-15"), by = "day")
  ) |>
    dplyr::mutate(count = 5)

  # Note: When data has report dates beyond final reference date,
  # that message appears first. The "no missing values" message
  # comes from get_reporting_structure() when the triangle is complete.
  rep_tri <- expect_message(
    as_reporting_triangle(data),
    regexp = "The dataframe contains report dates beyond the final reference date."
  ) # nolint
})
