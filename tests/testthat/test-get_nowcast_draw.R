test_that("get_nowcast_draw: returns a vector of correct length", {
  point_nowcast_matrix <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, 16.8,
      80, 40, 21.2, 19.5
    ),
    nrow = 3,
    byrow = TRUE
  )
  dispersion <- c(0.8, 12.4)
  reporting_triangle <- generate_triangle(point_nowcast_matrix)

  result <- get_nowcast_draw(
    point_nowcast_matrix,
    reporting_triangle,
    dispersion
  )

  expect_length(result, nrow(point_nowcast_matrix))
  expect_is(result, "numeric")
})
