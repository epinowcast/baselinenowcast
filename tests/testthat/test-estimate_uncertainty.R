test_that(c(
  "estimate_uncertainty function generates dispersion parameter",
  "estimates"
), {
  set.seed(123)
  # Make a simple triangle
  triangle <- matrix(
    c(
      100, 50, 30, 20,
      90, 45, 25, NA,
      80, 40, NA, NA,
      70, NA, NA, NA
    ),
    nrow = 4,
    byrow = TRUE
  )

  delay_pmf <- c(0.4, 0.3, 0.2, 0.1)
  disp_params <- estimate_uncertainty(
    triangle_to_nowcast = triangle,
    delay_pmf = delay_pmf,
    n_history_dispersion = 4
  )
})
