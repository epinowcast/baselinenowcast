point_nowcast_matrix <- matrix(
  c(
    10, 70, 90, 6,
    100, 50, 30, 20,
    90, 45, 25, 16.8,
    80, 40, 21.2, 19.5
  ),
  nrow = 4,
  byrow = TRUE
)
dispersion <- c(0.8, 12.4, 8)
reporting_triangle <- apply_reporting_structure(
  make_test_triangle(data = point_nowcast_matrix)
)

test_that("sample_nowcast: returns a matrix of correct length", {
  set.seed(2026)
  result <- sample_nowcast(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  # Result is a matrix not a vector, since delay aggregator will not
  # always turn this into a vector (at least it doesn't have to)
  expect_identical(nrow(result), nrow(point_nowcast_matrix))
  expect_gte(ncol(result), 1)
  expect_is(result, "matrix")
  expect_true(all(is.finite(result)))
  expect_true(all(result >= 0))
})

test_that(
  "sample_nowcast: mean of many draws is close to point nowcast row sums",
  {
    set.seed(123)
    n_draws <- 1000
    draws <- replicate(
      n_draws,
      sample_nowcast(point_nowcast_matrix, reporting_triangle, dispersion)
    )
    means <- rowMeans(draws)
    expected <- rowSums(point_nowcast_matrix)

    # With 1000 draws the standard error per row is comfortably below 5.
    expect_equal(means, expected, tolerance = 5)
  }
)

test_that("sample_nowcast: errors when uncertainty_params is wrong length", {
  expect_error(
    sample_nowcast(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion[1:2]
    ),
    regexp = "Vector of uncertainty parameters is less than the number"
  )

  expect_error(
    sample_nowcast(
      point_nowcast_matrix,
      reporting_triangle,
      c(dispersion, 1, 2)
    ),
    regexp = "Vector of uncertainty parameters is greater than the number"
  )
})

test_that("sample_nowcast: errors when delay_aggregator returns a matrix", {
  expect_error(
    sample_nowcast(
      point_nowcast_matrix,
      reporting_triangle,
      dispersion,
      delay_aggregator = identity
    ),
    regexp = "Got [0-9]+ columns from `delay_aggregator`"
  )
})
