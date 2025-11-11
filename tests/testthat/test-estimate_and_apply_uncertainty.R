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

  triangle <- matrix(
    data = sample.int(10, 6 * 5, replace = TRUE),
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
      triangle,
      n_history_delay = 6,
      n_retrospective_nowcasts = 2
    ),
    regexp = "Insufficient reference times in reporting triangle"
  )
})

test_that("estimate_and_apply_uncertainty is able to detect the structure of a jagged reporting triangle", { # nolint
  jagged_triangle <- matrix(
    data = sample.int(10, 12 * 5, replace = TRUE),
    nrow = 12,
    ncol = 5
  ) |> construct_triangle(structure = 2)
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

# New API Tests (uncertainty_opts) ---------------------------------------------

test_that("estimate_and_apply_uncertainty works with uncertainty_opts", {
  set.seed(123)
  nowcast_draws <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid,
    uncertainty = uncertainty_opts()
  )

  expect_is(nowcast_draws, "data.frame")
  expect_true(all(c("pred_count", "time", "draw") %in% names(nowcast_draws)))
})

test_that("estimate_and_apply_uncertainty works with Poisson model", {
  set.seed(456)
  nowcast_draws <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid,
    uncertainty = uncertainty_opts(model = uncertainty_poisson())
  )

  expect_is(nowcast_draws, "data.frame")
  expect_true(all(nowcast_draws$pred_count == floor(nowcast_draws$pred_count)))
})

test_that("estimate_and_apply_uncertainty works with custom aggregation", {
  skip_if_not_installed("zoo")
  set.seed(789)

  nowcast_draws <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid - 1,
    uncertainty = uncertainty_opts(
      aggregation = aggregation_opts(
        ref_time = function(x) zoo::rollsum(x, k = 2, align = "right")
      )
    )
  )

  expect_is(nowcast_draws, "data.frame")
})

test_that("estimate_and_apply_uncertainty new API produces same results", {
  set.seed(100)
  result_old <- suppressWarnings(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = n_delay_valid,
      n_retrospective_nowcasts = n_retro_valid,
      uncertainty_model = fit_by_horizon,
      uncertainty_sampler = sample_nb,
      ref_time_aggregator = identity,
      delay_aggregator = function(x) rowSums(x, na.rm = TRUE)
    )
  )

  set.seed(100)
  result_new <- estimate_and_apply_uncertainty(
    pt_nowcast_matrix,
    triangle,
    n_history_delay = n_delay_valid,
    n_retrospective_nowcasts = n_retro_valid,
    uncertainty = uncertainty_opts()
  )

  expect_equal(result_old, result_new, tolerance = 0.001)
})

test_that("estimate_and_apply_uncertainty works with deprecated API", {
  # Deprecation warning shown once per session, suppress for test
  result <- suppressWarnings(
    estimate_and_apply_uncertainty(
      pt_nowcast_matrix,
      triangle,
      n_history_delay = n_delay_valid,
      n_retrospective_nowcasts = n_retro_valid,
      uncertainty_model = fit_by_horizon
    )
  )

  # Verify deprecated API still produces valid output
  expect_s3_class(result, "data.frame")
})
