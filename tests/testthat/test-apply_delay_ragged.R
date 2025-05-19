test_that("apply_delay works with structure=2 ragged reporting triangles", {
  delay_pmf <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)
  partial_counts <- c(80, 100, 180, 80, 140)

  # Create a complete triangle based on the known delay PMF
  triangles <- lapply(partial_counts, function(x) x * delay_pmf)
  complete_triangle <- do.call(rbind, triangles)
  ragged_triangle <- generate_triangle(complete_triangle, structure = 2)

  result <- apply_delay(
    rep_tri_to_nowcast = ragged_triangle,
    delay_pmf = delay_pmf
  )
  cols <- colSums(result[3:5, ])
  pmf <- cols / sum(cols)
  expect_equal(pmf, delay_pmf, tolerance = 0.01)
})