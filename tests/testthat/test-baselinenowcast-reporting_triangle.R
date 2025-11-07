data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
data_as_of_df$age_group <- "00+"
rep_tri <- as_reporting_triangle(
  data = data_as_of_df,
  max_delay = 25,
  strata = "00+"
)
expected_cols <- c("pred_count", "draw", "reference_date", "output_type")

test_that("baselinenowcast.reporting_triangle() works as expected", {
  nowcast_df <- baselinenowcast(rep_tri, draws = 100)
  expect_baselinenowcast_structure(
    nowcast_df,
    expected_cols,
    output_type = "samples"
  )
  pt_nowcast_df <- baselinenowcast(rep_tri,
    output_type = "point"
  )
  expect_baselinenowcast_structure(
    pt_nowcast_df,
    expected_cols,
    output_type = "point"
  )
  expect_identical(pt_nowcast_df$draw[1], 1)

  # Expect draws are ordered
  expect_identical(
    nowcast_df$draw[nowcast_df$reference_date == "2026-04-01"],
    1:100
  )
})

test_that("baselinenowcast.reporting_triangle() errors sensibly with inappropriate inputs", { # nolint
  expect_error(
    baselinenowcast(rep_tri,
      output_type = "pt"
    ),
    regexp = "`output_type` must be one of "
  )
  expect_error(
    baselinenowcast(rep_tri,
      draws = TRUE
    ),
    regexp = "Assertion on 'draws' failed: Must be of type 'integerish'"
  )
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
  expect_baselinenowcast_structure(test_df, expected_cols)

  expect_no_warning(
    baselinenowcast(rep_tri,
      delay_pmf = rep(0.2, 26),
      draws = 100
    )
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

  expect_baselinenowcast_structure(test_df2, expected_cols)
})

test_that("baselinenowcast specifying not to include draws works as expected", {
  skip_if_not_installed("dplyr")
  pt_nowcast <- baselinenowcast(rep_tri,
    output_type = "point"
  )
  expect_baselinenowcast_structure(pt_nowcast, expected_cols,
    output_type = "point"
  )
  prob_nowcast <- baselinenowcast(rep_tri)

  summarised_prob_nowcast <- prob_nowcast |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(mean_nowcast = mean(pred_count))

  expect_equal(summarised_prob_nowcast$mean_nowcast,
    pt_nowcast$pred_count,
    tol = 0.1
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

  expect_estimates_differ(
    mean_dif_delay$mean_nc,
    mean_nowcast$mean_nc,
    tol = 0.1
  )

  dif_uq <- baselinenowcast(rep_tri,
    uncertainty_params = rep(1, 25),
    draws = 100
  )
  sd_uq <- dif_uq |>
    dplyr::group_by(reference_date) |>
    dplyr::summarise(sd_nc = sd(pred_count))

  expect_estimates_differ(
    sd_uq$sd_nc,
    mean_nowcast$sd_nc,
    tol = 0.1
  )
})

test_that("assert_baselinenowcast_df errors when appropriate", {
  nowcast_df <- baselinenowcast(rep_tri, draws = 100)
  expect_no_error(assert_baselinenowcast_df(nowcast_df))
  pt_nowcast_df <- baselinenowcast(rep_tri,
    output_type = "point"
  )
  expect_no_error(assert_baselinenowcast_df(pt_nowcast_df))

  expect_error(
    assert_baselinenowcast_df(
      nowcast_df[, -1]
    ),
    regexp = "Required columns missing from data"
  )

  nowcast_df2 <- rbind(nowcast_df, nowcast_df)
  expect_error(
    assert_baselinenowcast_df(
      nowcast_df2
    ),
    regexp = "Data contains multiple `reference_date`s"
  )

  nowcast_df_dates <- nowcast_df
  nowcast_df_dates$reference_date <- "dates"
  expect_error_wrong_date_class(
    assert_baselinenowcast_df(nowcast_df_dates)
  )

  expect_error(
    assert_baselinenowcast_df(
      list(nowcast_df)
    ),
    regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'list'" # nolint
  )
})

test_that("baselinenowcast.reporting_triangle errors if nothing to nowcast", {
  skip_if_not_installed("tidyr")
  skip_if_not_installed("dplyr")
  data <- tidyr::expand_grid(
    reference_date = seq(as.Date("2021-04-01"), as.Date("2021-04-30"),
      by = "day"
    ),
    report_date = seq(as.Date("2021-04-01"), as.Date("2021-05-15"), by = "day")
  ) |>
    dplyr::mutate(count = 5)

  rep_tri <- expect_message(
    as_reporting_triangle(data, max_delay = 10),
    regexp = "The reporting triangle does not contain any missing values."
  ) # nolint

  expect_error(baselinenowcast(rep_tri),
    regexp = "doesn't contain any missing values"
  ) # nolint
})

test_that(
  "baselinenowcast with preprocess = NULL produces point nowcast",
  {
    # Convert matrix to reporting_triangle object
    reference_dates <- seq(
      from = as.Date("2025-01-01"),
      length.out = nrow(example_downward_corr_mat),
      by = "day"
    )
    triangle <- as_reporting_triangle(
      data = example_downward_corr_mat,
      reference_dates = reference_dates,
      max_delay = 3
    )

    # Test that baselinenowcast() completes with preprocess = NULL
    # Using output_type = "point" since uncertainty estimation
    # does not support negative predictions from negative PMF
    result <- expect_no_error(
      suppressWarnings(
        baselinenowcast(
          data = triangle,
          preprocess = NULL,
          output_type = "point"
        )
      )
    )

    # Verify output structure
    result_expected_cols <- c(
      "reference_date",
      "pred_count",
      "draw",
      "output_type"
    )
    expect_baselinenowcast_structure(
      result,
      result_expected_cols,
      output_type = "point"
    )

    # Verify nowcast values exist
    expect_false(anyNA(result$pred_count))

    # Verify output has rows
    expect_gt(nrow(result), 0)
  }
)
