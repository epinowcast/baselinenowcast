test_that("sample_nowcast: returns a matrix of correct length", {
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
  reporting_triangle <- construct_triangle(point_nowcast_matrix)

  result <- sample_nowcast(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  # Result is a matrix not a vector, since delay aggregator will not
  # always turn this into a vector (at least it doesn't have to)
  expect_identical(nrow(result), nrow(point_nowcast_matrix))
  expect_true(ncol(result) >= 1)
  expect_is(result, "matrix")
})
