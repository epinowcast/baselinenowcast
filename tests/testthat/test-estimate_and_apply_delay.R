sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

counts <- c(30, 40, 50, 60, 70, 50, 40, 50, 80, 40)

complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

reporting_triangle <- construct_triangle(
  complete_triangle
)

test_that("estimate_and_apply_delay works as expected with defaults", {
  point_nowcast_matrix <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle
  )
  # Test that the function returns the expected PMF
  expect_equal(complete_triangle, point_nowcast_matrix, tol = 0.2)
})

test_that(
  "estimate_and_apply_delay errors when n_history_delay is misspecified",
  {
    expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        n_history_delay = 2
      ),
      regexp = "The rows used for delay estimation in the reporting triangle must"
    )

    expect_error(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        n_history_delay = 12
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
      regexp = "Only the first 3 delays are being nowcasted"
    )

    pt_nowcast1 <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      max_delay = 2
    )
    expect_identical(ncol(pt_nowcast1), 3L)

    expect_message(
      estimate_and_apply_delay(
        reporting_triangle = reporting_triangle,
        max_delay = 8
      ),
      regexp = "The maximum delay must be less than the number of columns" # nolint
    )

    pt_nowcast2 <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      max_delay = 8
    )
    expect_identical(ncol(pt_nowcast2), 7L)
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
      complete_triangle,
      structure = 2
    )

    # Get delay estimate
    point_nowcast_matrix <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      n_history_delay = 5
    )
    # Test that the function returns the expected PMF
    expect_equal(complete_triangle, point_nowcast_matrix, tol = 0.2)
  }
)
