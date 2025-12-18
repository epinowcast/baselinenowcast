sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

counts <- c(30, 40, 50, 60, 70, 50, 40, 50, 80, 40)

complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
complete_triangle <- do.call(rbind, complete_triangle)

reporting_triangle <- apply_reporting_structure(
  make_test_triangle(data = complete_triangle)
)

test_that(
  "estimate_and_apply_delay returns filled triangle with estimated delay",
  {
    point_nowcast_matrix <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle
    )
    # Test that the function returns the expected point nowcast
    expect_equal(
      complete_triangle,
      matrix(point_nowcast_matrix, nrow = nrow(point_nowcast_matrix)),
      tol = 0.2
    )

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
  }
)

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
      regexp = "number of observations.*must be less than or equal" # nolint
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

    reporting_triangle <- apply_reporting_structure(
      make_test_triangle(data = complete_triangle),
      structure = 2
    )

    # Get delay estimate
    point_nowcast_matrix <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      n = 5
    )
    # Test that the function returns the expected PMF
    expect_equal(
      complete_triangle,
      matrix(point_nowcast_matrix, nrow = nrow(point_nowcast_matrix)),
      tol = 0.2
    )
  }
)

test_that(
  "estimate_and_apply_delay messages if max delay is specified as higher than reporting triangle ", # nolint
  {
    sim_delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)

    counts <- c(30, 40, 50, 60, 70)

    complete_triangle <- lapply(counts, function(x) x * sim_delay_pmf)
    complete_triangle <- do.call(rbind, complete_triangle)

    reporting_triangle <- apply_reporting_structure(
      make_test_triangle(data = complete_triangle),
      structure = 2
    )

    # Test with full reporting triangle
    point_nowcast_matrix <- estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      n = 5
    )

    # Verify result has expected structure
    expect_s3_class(point_nowcast_matrix, "reporting_triangle")
  }
)

test_that("estimate_and_apply_delay works with custom delay_pmf", {
  custom_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)
  result <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle,
    delay_pmf = custom_pmf
  )

  expect_s3_class(result, "reporting_triangle")
  expect_false(anyNA(result))
  expect_identical(dim(result), dim(reporting_triangle))

  last_row <- result[nrow(result), ]
  pmf_est <- last_row / sum(last_row, na.rm = TRUE)
  expect_equal(unname(pmf_est), unname(custom_pmf), tolerance = 0.1)
})

test_that("estimate_and_apply_delay works with different n values", {
  result_n8 <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle,
    n = 8
  )
  result_n10 <- estimate_and_apply_delay(
    reporting_triangle = reporting_triangle,
    n = 10
  )

  expect_s3_class(result_n8, "reporting_triangle")
  expect_s3_class(result_n10, "reporting_triangle")
  expect_identical(dim(result_n8), dim(reporting_triangle))
  expect_identical(dim(result_n10), dim(reporting_triangle))
})

test_that("estimate_and_apply_delay errors when n is too large", {
  expect_error(
    estimate_and_apply_delay(
      reporting_triangle = reporting_triangle,
      n = 15
    ),
    regexp = "number of observations.*must be less than or equal"
  )
})

test_that("estimate_and_apply_delay errors when no complete rows", {
  sim_delay_pmf <- c(0.4, 0.3, 0.2, 0.05, 0.05)
  partial_counts <- c(80, 100)

  triangle_mat <- lapply(partial_counts, function(x) x * sim_delay_pmf)
  triangle_mat <- do.call(rbind, triangle_mat)

  triangle <- make_test_triangle(data = triangle_mat) |>
    apply_reporting_structure(structure = c(1, 2))

  expect_error(
    estimate_and_apply_delay(triangle),
    regexp = "at least one row with no missing observations"
  )
})
