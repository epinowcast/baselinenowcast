triangle <- matrix(
  data = sample.int(10, 12 * 5, replace = TRUE),
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
  retro_rep_tris <- construct_triangles(trunc_rep_tris)
  retro_pt_nowcasts <- fill_triangles(retro_rep_tris, n = n_delay_valid)
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
