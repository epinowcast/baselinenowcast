# Test data uses the NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]

test_that(
  "as_ChainLadder_triangle() converts reporting_triangle to ChainLadder
  triangle", {
  skip_if_not_installed("ChainLadder")

  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )

  cl_triangle <- as_ChainLadder_triangle(rep_tri)

  # Check that the result is a ChainLadder triangle
  expect_s3_class(cl_triangle, "triangle")
  expect_s3_class(cl_triangle, "matrix")

  # Check dimensions match
  expect_identical(
    nrow(cl_triangle),
    nrow(rep_tri$reporting_triangle_matrix)
  )
  expect_identical(
    ncol(cl_triangle),
    ncol(rep_tri$reporting_triangle_matrix)
  )

  # Check that the matrix values are preserved
  expect_identical(
    unclass(as.matrix(cl_triangle)),
    rep_tri$reporting_triangle_matrix,
    ignore_attr = TRUE
  )
})

test_that(
  "as_reporting_triangle.triangle() converts ChainLadder triangle back", {
  skip_if_not_installed("ChainLadder")

  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )

  # Convert to ChainLadder triangle
  cl_triangle <- as_ChainLadder_triangle(rep_tri)

  # Convert back to reporting_triangle
  rep_tri_2 <- as_reporting_triangle(
    data = cl_triangle,
    max_delay = rep_tri$max_delay,
    reference_dates = rep_tri$reference_dates,
    delays_unit = rep_tri$delays_unit
  )

  # Check that we get a reporting_triangle back
  expect_s3_class(rep_tri_2, "reporting_triangle")
  expect_no_error(assert_reporting_triangle(rep_tri_2))

  # Check that key components match (ignore dimname attributes)
  expect_identical(
    rep_tri_2$reporting_triangle_matrix,
    rep_tri$reporting_triangle_matrix,
    ignore_attr = TRUE
  )
  expect_identical(rep_tri_2$reference_dates, rep_tri$reference_dates)
  expect_identical(rep_tri_2$max_delay, rep_tri$max_delay)
  expect_identical(rep_tri_2$delays_unit, rep_tri$delays_unit)
  expect_identical(rep_tri_2$structure, rep_tri$structure)
})

test_that("round-trip conversion preserves all data", { # nolint
  skip_if_not_installed("ChainLadder")

  rep_tri_original <- as_reporting_triangle(data_as_of_df,
    max_delay = 25,
    strata = "test_strata",
    delays_unit = "days"
  )

  # Round trip: reporting_triangle -> ChainLadder -> reporting_triangle
  cl_triangle <- as_ChainLadder_triangle(rep_tri_original)
  rep_tri_final <- as_reporting_triangle(
    data = cl_triangle,
    max_delay = rep_tri_original$max_delay,
    reference_dates = rep_tri_original$reference_dates,
    strata = rep_tri_original$strata,
    delays_unit = rep_tri_original$delays_unit
  )

  # Everything should be identical after round trip
  expect_identical(
    rep_tri_final$reporting_triangle_matrix,
    rep_tri_original$reporting_triangle_matrix,
    ignore_attr = TRUE
  )
  expect_identical(
    rep_tri_final$reference_dates,
    rep_tri_original$reference_dates
  )
  expect_identical(rep_tri_final$max_delay, rep_tri_original$max_delay)
  expect_identical(rep_tri_final$strata, rep_tri_original$strata)
  expect_identical(
    rep_tri_final$delays_unit,
    rep_tri_original$delays_unit
  )
  expect_identical(rep_tri_final$structure, rep_tri_original$structure)
})

test_that(
  "as_reporting_triangle.triangle() errors without reference_dates", {
  skip_if_not_installed("ChainLadder")

  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )

  cl_triangle <- as_ChainLadder_triangle(rep_tri)

  # Remove row names so reference_dates cannot be inferred
  rownames(cl_triangle) <- NULL

  expect_error(
    as_reporting_triangle(
      data = cl_triangle,
      max_delay = 25
    ),
    regexp = "`reference_dates` must be provided"
  )
})

test_that(
  "as_reporting_triangle.triangle() can infer dates from rownames", {
  skip_if_not_installed("ChainLadder")

  rep_tri <- as_reporting_triangle(data_as_of_df,
    max_delay = 25
  )

  cl_triangle <- as_ChainLadder_triangle(rep_tri)

  # Set row names to dates
  rownames(cl_triangle) <- as.character(rep_tri$reference_dates)

  # Should work without explicit reference_dates
  rep_tri_2 <- as_reporting_triangle(
    data = cl_triangle,
    max_delay = 25
  )

  expect_s3_class(rep_tri_2, "reporting_triangle")
  expect_identical(rep_tri_2$reference_dates, rep_tri$reference_dates)
})

test_that(
  "ChainLadder triangle works with different temporal units", {
  skip_if_not_installed("ChainLadder")
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

  # Convert to ChainLadder and back
  cl_triangle <- as_ChainLadder_triangle(rep_tri)
  rep_tri_2 <- as_reporting_triangle(
    data = cl_triangle,
    max_delay = rep_tri$max_delay,
    reference_dates = rep_tri$reference_dates,
    delays_unit = "weeks"
  )

  expect_identical(rep_tri_2$delays_unit, "weeks")
  expect_identical(
    rep_tri_2$reporting_triangle_matrix,
    rep_tri$reporting_triangle_matrix,
    ignore_attr = TRUE
  )
})

test_that("as_ChainLadder_triangle() validates input", { # nolint
  skip_if_not_installed("ChainLadder")

  # Should error on non-reporting_triangle input
  expect_error(
    as_ChainLadder_triangle(matrix(1:9, nrow = 3))
  )

  expect_error(
    as_ChainLadder_triangle(list(a = 1, b = 2))
  )
})
