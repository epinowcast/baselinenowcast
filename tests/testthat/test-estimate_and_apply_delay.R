sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

counts <- c(30, 40, 50, 60, 70, 50, 40, 50, 80, 40)

complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

reporting_triangle <- construct_triangle(
  to_reporting_triangle(complete_triangle)
)

test_that("estimate_and_apply_delay works as expected with defaults", {
  point_nowcast_matrix <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle
  )
  # Test that the function returns the expected point nowcast
  expect_equal(complete_triangle, matrix(point_nowcast_matrix, nrow = nrow(point_nowcast_matrix)), tol = 0.2)

  # Test that output is the same as when run individually with defaults
  delay_pmf <- estimate_delay(reporting_triangle)
  pt_nowcast_matrix_2 <- apply_delay(reporting_triangle, delay_pmf)
  expect_equal(point_nowcast_matrix, pt_nowcast_matrix_2, tol = 0.2)

  # And with specified n
  pt_nowcast_matrix_joint <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle,
    n = 8
  )

  delay_pmf <- estimate_delay(reporting_triangle, n = 8)
  pt_nowcast_matrix_2 <- apply_delay(
    reporting_triangle,
    delay_pmf
  )
  expect_equal(pt_nowcast_matrix_joint, pt_nowcast_matrix_2, tol = 0.2)
})

test_that(
  "estimate_and_apply_delay errors when n_history_delay is misspecified",
  {
    expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        n = 2
      ),
      regexp = "The rows used for delay estimation in the reporting triangle must" # nolint
    )

    expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        n = 12
      ),
      regexp = "Number of observations in input reporting triangle is insufficient" # nolint
    )
  }
)

test_that(
  "estimate_and_apply_delay behaves appropriately when max_delay is misspecified", # nolint
  {
    expect_message(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        max_delay = 2
      ),
      regexp = "Additional columns of the reporting triangle were provided than are needed for the specified maximum delay." # nolint
    )

    pt_nowcast1 <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      max_delay = 2
    )
    expect_identical(ncol(pt_nowcast1), 3L)

    expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        max_delay = 8
      ),
      regexp = "The maximum delay must be less than the number of columns" # nolint
    )
  }
)

test_that(
  "estimate_and_apply_delay works with every other day reporting of daily data (ragged triangle test) ", # nolint
  {
    sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

    counts <- c(30, 40, 50, 60, 70)

    complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
    complete_triangle <- do.call(rbind, complete_triangle)

    reporting_triangle <- construct_triangle(
      to_reporting_triangle(complete_triangle),
      structure = 2
    )

    # Get delay estimate
    point_nowcast_matrix <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      n = 5
    )
    # Test that the function returns the expected PMF
    expect_equal(complete_triangle, matrix(point_nowcast_matrix, nrow = nrow(point_nowcast_matrix)), tol = 0.2)
  }
)

test_that(
  "estimate_and_apply_delay messages if max delay is specified as higher than reporting triangle ", # nolint
  {
    sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

    counts <- c(30, 40, 50, 60, 70)

    complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
    complete_triangle <- do.call(rbind, complete_triangle)

    reporting_triangle <- construct_triangle(
      to_reporting_triangle(complete_triangle),
      structure = 2
    )

    point_nowcast_matrix <- expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        max_delay = 8,
        n = 5
      ),
      regexp = "The maximum delay must be less than the number of columns"
    )
  }
)
