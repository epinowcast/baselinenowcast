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



test_that("as_reporting_triangle.data.frame() can handle a ragged triangle with a single missing reference date", { # nolint

  test <- data_as_of_df[data_as_of_df$reference_date != "2026-03-26", ]

  rep_tri <- expect_warning(
    as_reporting_triangle(test,
      max_delay = 25
    ),
    regexp = "Data does not contain case counts for all possible reference dates",
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
    regexp = "Data contains duplicate `reference_date` and `report_date` combinations"
  ) # nolint
})

test_that("as_reporting_triangle.data.frame() can handle different column names", {
  data <- data_as_of_df
  old_names <- c("reference_date", "report_date", "count")
  new_names <- c("ref_date", "rep_date", "cases")
  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  rep_tri <- as_reporting_triangle(data,
    max_delay = 25,
    reference_date_col_name = "ref_date",
    report_date_col_name = "rep_date",
    count_col_name = "cases"
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

test_that("as_reporting_triangle.matrix() can handle specification of each arg", {
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
    from = ymd("2025-01-01"),
    to = ymd("2025-01-05"),
    by = "day"
  )
  max_delay <- 4

  rep_tri <- as_reporting_triangle(
    reporting_triangle = rep_tri_mat,
    reference_dates = reference_dates,
    max_delay = max_delay
  )
  expect_identical(rep_tri$reporting_triangle_matrix, rep_tri_mat)
  expect_identical(rep_tri$reference_date, reference_dates)
  expect_identical(rep_tri$structure, 1)
})

test_that("as_reporting_triangle.matrix() errors if reference dates don't align with rows of the matrix", {
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
    from = ymd("2025-01-01"),
    to = ymd("2025-01-06"),
    by = "day"
  )
  max_delay <- 4

  expect_error(
    as_reporting_triangle(
      reporting_triangle = rep_tri_mat,
      reference_dates = reference_dates,
      max_delay = max_delay
    ),
    regexp = "Length of `reference_dates` must equal number of rows in"
  ) # nolint
})
