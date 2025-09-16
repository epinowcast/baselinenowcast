triangle <- matrix(
  data = sample(10, 12 * 5, replace = TRUE),
  nrow = 12,
  ncol = 5
) |> construct_triangle()
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = triangle,
  n = 6
)

tv <- allocate_reference_times(triangle)
n_delay_valid <- tv$n_history_delay
n_retro_valid <- tv$n_retrospective_nowcasts


test_that("estimate_and_apply_uncertainty works as expected with the default settings", {
  set.seed(123)
  nowcast_draws_df <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid
  )
  set.seed(123)
  df_w_default <- expect_message(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle
    ),
    regexp = "Using 6 reference times for delay estimation." # nolint
  )
  expect_equal(nowcast_draws_df, df_w_default)

  expect_message(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = 6
    ),
    regexp = "`n_retrospective_nowcasts` was not specified, " # nolint
  )
  expect_message(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_retrospective_nowcasts = 6
    ),
    regexp = "`n_history_delay` was not specified, " # nolint
  )

  set.seed(123)
  df_w_non_default <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = 10,
    n_retrospective_nowcasts = 2
  )
  expect_false(all(nowcast_draws_df == df_w_non_default))
})

test_that("estimate_and_apply_uncertainty error when things are specified incorrectly", {
  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = 4
    ),
    regexp = "Insufficient `n_history_delay`."
  )

  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_retrospective_nowcasts = 1
    ),
    regexp = "Insufficient `n_retrospective_nowcasts`."
  )

  triangle <- matrix(
    data = sample(10, 6 * 5, replace = TRUE),
    nrow = 6,
    ncol = 5
  ) |> construct_triangle()
  pt_nowcast_matrix <- estimate_and_apply_delay(
    reporting_triangle = triangle,
    n = 6
  )

  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle
    ),
    regexp = "Insufficient reference times in reporting triangle"
  )
})
