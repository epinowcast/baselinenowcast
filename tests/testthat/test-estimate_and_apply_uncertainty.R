triangle <- make_test_triangle(
  data = matrix(
    data = sample.int(10, 12 * 5, replace = TRUE),
    nrow = 12,
    ncol = 5
  ),
  construct = TRUE
)
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = triangle,
  n = 6
)

tv <- allocate_reference_times(triangle)
n_delay_valid <- tv$n_history_delay
n_retro_valid <- tv$n_retrospective_nowcasts

test_that("estimate_and_apply_uncertainty produces correct results", {
  set.seed(123)
  nowcast_draws_df <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid
  )

  set.seed(123)
  # Test against manual workflow
  trunc_rep_tris <- truncate_triangles(triangle, n = n_retro_valid)
  retro_rep_tris <- apply_reporting_structures(trunc_rep_tris)
  retro_pt_nowcasts <- estimate_and_apply_delays(retro_rep_tris, n = n_delay_valid)
  disp_params <- estimate_uncertainty(
    retro_pt_nowcasts,
    trunc_rep_tris,
    retro_rep_tris,
    n = n_retro_valid
  )
  nowcast_draws_df2 <- sample_nowcasts(
    pt_nowcast_matrix,
    triangle,
    disp_params
  )

  expect_equal(nowcast_draws_df, nowcast_draws_df2, tolerance = 0.1)
})

test_that("estimate_and_apply_uncertainty errors when things are specified incorrectly", { # nolint
  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = 4,
      n_retrospective_nowcasts = 2
    ),
    regexp = "Insufficient `n_history_delay`."
  )

  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = 6,
      n_retrospective_nowcasts = 1
    ),
    regexp = "Insufficient `n_retrospective_nowcasts`."
  )

  triangle <- make_test_triangle(
    data = matrix(
      data = sample.int(10, 6 * 5, replace = TRUE),
      nrow = 6,
      ncol = 5
    ),
    construct = TRUE
  )
  pt_nowcast_matrix <- estimate_and_apply_delay(
    reporting_triangle = triangle,
    n = 6
  )

  expect_error(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = 6,
      n_retrospective_nowcasts = 2
    ),
    regexp = "Insufficient reference times in reporting triangle"
  )
})

test_that("estimate_and_apply_uncertainty is able to detect the structure of a jagged reporting triangle", { # nolint
  jagged_triangle <- make_test_triangle(
    data = matrix(
      data = sample.int(10, 12 * 5, replace = TRUE),
      nrow = 12,
      ncol = 5
    ),
    construct = TRUE,
    structure = 2
  )
  pt_nowcast_matrix2 <- estimate_and_apply_delay(
    reporting_triangle = jagged_triangle,
    n = 6
  )

  tv <- allocate_reference_times(jagged_triangle)
  n_delay_valid <- tv$n_history_delay
  n_retro_valid <- tv$n_retrospective_nowcasts
  set.seed(123)
  nowcast_draws_df <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix2,
    jagged_triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid
  )
  set.seed(123)
  nowcast_draws_df2 <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix2,
    jagged_triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid,
    structure = 2
  )
  expect_equal(mean(nowcast_draws_df$pred_count),
    mean(nowcast_draws_df2$pred_count),
    tol = 0.01
  )
})
