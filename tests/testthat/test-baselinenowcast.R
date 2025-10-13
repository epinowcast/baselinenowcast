data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df$age_group <- "00+"
rep_tri <- as_reporting_triangle(
  data = data_as_of_df,
  max_delay = 25,
  strata = "age_group"
)
expected_cols <- c("pred_count", "draw", "reference_date", "age_group")
test_that("baselinenowcast.reporting_triangle() works as expected", {
  nowcast_df <- baselinenowcast(rep_tri, draws = 100)
  expect_s3_class(nowcast_df, "data.frame")
  expect_s3_class(nowcast_df, "nowcast_df")
  expect_true(all(expected_cols %in% colnames(nowcast_df)))
})

test_that("baselinenowcast.reporting_triangle() handles separate delay and uncertainty estimates appropriately", { # nolint
  expect_error(
    baselinenowcast(rep_tri,
      delay_pmf = rep(1 / 24, 24),
      draws = 100
    ),
    regexp = "`delay_pmf` is not the same length as the number"
  )

  test_df <- expect_no_error(baselinenowcast(rep_tri,
    delay_pmf = rep(1 / 26, 26),
    draws = 100
  ))
  expect_s3_class(test_df, "data.frame")
  expect_true(all(expected_cols %in% colnames(test_df)))

  expect_warning(
    baselinenowcast(rep_tri,
      delay_pmf = rep(0.2, 26),
      draws = 100
    ),
    regexp = "`delay_pmf` does not sum to approximately one."
  ) # nolint

  expect_error(
    baselinenowcast(rep_tri,
      uncertainty_params = rep(1, 10)
    ),
    regexp = "`uncertainty_params` are not the same length"
  )

  test_df2 <- baselinenowcast(rep_tri,
    uncertainty_params = rep(1, 25),
    draws = 100
  )

  expect_s3_class(test_df2, "data.frame")
  expect_true(all(expected_cols %in% colnames(test_df2)))
})

test_that("baselinenowcast specifying not to include draws works as expected", {
  skip_if_not_installed("dplyr")
  pt_nowcast <- baselinenowcast(rep_tri,
    include_draws = FALSE
  )
  expect_s3_class(pt_nowcast, "data.frame")
  expect_true(all(c("pred_count", "reference_date", "age_group")
  %in% colnames(pt_nowcast)))
  prob_nowcast <- baselinenowcast(rep_tri)

  summarised_prob_nowcast <- prob_nowcast |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(mean_nowcast = mean(pred_count))

  expect_equal(summarised_prob_nowcast$mean_nowcast,
    pt_nowcast$pred_count,
    tol = 2
  )
  expect_identical(nrow(summarised_prob_nowcast), nrow(pt_nowcast))
})

test_that("baselinenowcast passing in a separate delay/uncertainty parameters returns something different than using the triangle", { # nolint
  skip_if_not_installed("dplyr")
  dif_delay_df <- baselinenowcast(rep_tri,
    delay_pmf = rep(1 / 26, 26),
    draws = 100
  )
  mean_dif_delay <- dif_delay_df |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(mean_nc = mean(pred_count))
  nowcast_df <- baselinenowcast(rep_tri,
    draws = 100
  )
  mean_nowcast <- nowcast_df |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(
      mean_nc = mean(pred_count),
      sd_nc = sd(pred_count)
    )

  expect_failure(expect_equal(
    mean_dif_delay$mean_nc,
    mean_nowcast$mean_nc
  ))

  dif_uq <- baselinenowcast(rep_tri,
    uncertainty_params = rep(1, 25),
    draws = 100
  )
  sd_uq <- dif_uq |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(sd_nc = sd(pred_count))

  expect_failure(expect_equal(
    sd_uq$sd_nc,
    mean_nowcast$sd_nc
  ))
})
