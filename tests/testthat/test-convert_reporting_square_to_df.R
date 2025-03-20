test_that("convert_rep_square_to_df function mimics pivot longer", {
  rep_square <- matrix(
    c(
      80, 50, 25, 10,
      100, 50, 30, 20,
      90, 45, 25, 18,
      80, 40, 24, 16,
      70, 35, 21, 19,
      67, 34, 15, 9
    ),
    nrow = 6,
    byrow = TRUE
  )

  result <- convert_reporting_square_to_df(rep_square)

  # result using pivot longer
  df <- as.data.frame(rep_square)
  df$time <- seq_len(nrow(df))
  df_long <- df |>
    tidyr::pivot_longer(
      cols = starts_with("V"),
      names_to = "delay",
      names_prefix = "V",
      values_to = "count"
    )
  df_long$delay <- as.integer(df_long$delay)

  expect_identical(result$time, df_long$time)
  expect_identical(result$delay, df_long$delay)
  expect_identical(result$count, df_long$count)
})
