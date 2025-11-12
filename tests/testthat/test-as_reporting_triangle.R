# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that("as_reporting_triangle.data.frame() creates matrix with correct dimensions and structure", { # nolint
  rep_tri <- as_reporting_triangle(data_as_of_df)
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri),
    length(unique(data_as_of_df$reference_date))
  )

  # even if we add other columns
  data_as_of_df$test_col <- 2

  rep_tri2 <- as_reporting_triangle(data_as_of_df)
  expect_identical(
    nrow(rep_tri2),
    length(unique(data_as_of_df$reference_date))
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
    delays_unit = "weeks"
  )
  ref_dates <- get_reference_dates(rep_tri)
  expected_days_diff <- as.numeric(difftime(
    ref_dates[2],
    ref_dates[1]
  ))
  expect_identical(expected_days_diff, 7)
  expect_true(is.matrix(rep_tri))
  expect_identical(attr(rep_tri, "delays_unit"), "weeks")

  # Now just filter the reference dates, keep all delays (needed for doing the
  # weekday filtering)
  weekly_daily <- data_as_of_df |>
    dplyr::filter(lubridate::wday(reference_date) == 1)
  rep_tri2 <- expect_message(
    as_reporting_triangle(weekly_daily,
      delays_unit = "days"
    ),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  ) |>
    truncate_to_delay(max_delay = 28)
  ref_dates2 <- get_reference_dates(rep_tri2)
  expected_days_diff <- as.numeric(difftime(
    ref_dates2[2],
    ref_dates2[1]
  ))
  expect_identical(expected_days_diff, 7)
  expect_true(is.matrix(rep_tri2))
  expect_identical(attr(rep_tri2, "delays_unit"), "days")

  # Now just filter the report dates to simulate reporting on one day of the
  # week but with reference dates for all days
  daily_weekly <- data_as_of_df |>
    dplyr::filter(lubridate::wday(report_date) == 1)
  rep_tri3 <- as_reporting_triangle(daily_weekly,
    delays_unit = "days"
  ) |>
    truncate_to_delay(max_delay = 28)
  ref_dates3 <- get_reference_dates(rep_tri3)
  expected_days_diff <- as.numeric(difftime(
    ref_dates3[2],
    ref_dates3[1]
  ))
  expect_identical(expected_days_diff, 1)
  expect_true(is.matrix(rep_tri3))
  expect_identical(attr(rep_tri3, "delays_unit"), "days")
  # we expect a lot of 0s in the reporting triangle because it gets
  # converted to daily daily so test for this
  n_zeros <- sum(rep_tri3 == 0, na.rm = TRUE)
  n_elements <- nrow(rep_tri3) * ncol(rep_tri3) # nolint
  prop_zeros <- n_zeros / n_elements
  expect_gt(prop_zeros, 5 / 7)

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
  ) |>
    truncate_to_delay(max_delay = 28)
  # Both rep_tri2 and rep_tri4 should have the same max delay after truncation
  expect_identical(
    ncol(rep_tri4),
    ncol(rep_tri2)
  )
})
test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- expect_message(
    as_reporting_triangle(test),
    regexp = "Data does not contain case counts for all possible reference dates" # nolint
  ) |>
    truncate_to_delay(max_delay = 23)
  # Check that the same number of reference dates is in the reporting triangle
  expect_identical(
    nrow(rep_tri),
    length(unique(test$reference_date))
  )

  # The structure is computed dynamically and not stored as an attribute
  # Verify that get_reporting_structure correctly identifies the pattern
  # Structure shows counts per delay, with "2" indicating the gap where
  # reference date "2026-03-26" is missing (ragged)
  expected_structure <- c(1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) # nolint
  expect_identical(get_reporting_structure(rep_tri), expected_structure)
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
    nrow(rep_tri),
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

test_that("as_reporting_triangle.data.frame() successfully creates reporting triangle", { # nolint
  rep_tri <- as_reporting_triangle(data_as_of_df)

  # Verify object is created successfully
  expect_s3_class(rep_tri, "reporting_triangle")
  expect_no_error(assert_reporting_triangle(rep_tri))
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

  rep_tri <- as_reporting_triangle(
    data = rep_tri_mat,
    reference_dates = reference_dates
  )
  # Check matrix values match (ignoring dimnames and other attributes)
  mat_compare <- unclass(rep_tri)
  attributes(mat_compare) <- list(dim = dim(mat_compare))
  expect_identical(mat_compare, rep_tri_mat)
  expect_equal(get_reference_dates(rep_tri), reference_dates) # nolint: expect_identical_linter
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

  expect_error(
    as_reporting_triangle(
      data = rep_tri_mat,
      reference_dates = reference_dates
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
  # Test invalid delays_unit value
  expect_error(
    as_reporting_triangle(
      data = data_as_of_df,
      delays_unit = "daily"
    ),
    regexp = "Assertion on 'delays_unit' failed:"
  )
})

test_that("assert_reporting_triangle validates attributes correctly", {
  rep_tri <- as_reporting_triangle(data_as_of_df)
  expect_s3_class(rep_tri, "reporting_triangle")
  expect_no_error(assert_reporting_triangle(rep_tri))

  # Test with modified rownames (reference dates stored as rownames)
  rep_tri1 <- rep_tri
  # Unclass, modify rownames, then restore class
  mat1 <- unclass(rep_tri1)
  rownames(mat1) <- rep("invalid", nrow(mat1))
  class(mat1) <- class(rep_tri1)
  attributes(mat1)$delays_unit <- attr(rep_tri1, "delays_unit")
  expect_error(assert_reporting_triangle(mat1))

  # Test with modified delays_unit attribute
  rep_tri2 <- rep_tri
  attr(rep_tri2, "delays_unit") <- "months"
  expect_no_error(assert_reporting_triangle(rep_tri2))
  attr(rep_tri2, "delays_unit") <- "month"
  expect_error(assert_reporting_triangle(rep_tri2))
  attr(rep_tri2, "delays_unit") <- 8
  expect_error(assert_reporting_triangle(rep_tri2))
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

  # Check that no NAs are present in the resulting triangle after truncation
  rep_tri <- suppressMessages(as_reporting_triangle(data)) |>
    truncate_to_delay(max_delay = 10)
  expect_false(anyNA(rep_tri))
})
