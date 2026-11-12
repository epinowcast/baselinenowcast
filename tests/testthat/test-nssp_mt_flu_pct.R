test_that("NSSP percentage increments have the documented structure", {
  dat <- nssp_mt_flu_pct_df
  expect_named(dat, c("reference_date", "report_date", "count"))
  expect_s3_class(dat$reference_date, "Date")
  expect_s3_class(dat$report_date, "Date")
  expect_type(dat$count, "double")
  expect_identical(nrow(dat), 1283L)
  expect_identical(anyDuplicated(dat[c("reference_date", "report_date")]), 0L)
  expect_true(all(is.finite(dat$count)))
  expect_true(all(dat$report_date >= dat$reference_date))
  expect_identical(sum(dat$count < 0), 20L)
  expect_true(any(abs(dat$count - round(dat$count)) > 1e-8))
})

test_that("NSSP increments reconstruct archived published percentages", {
  # Independent values from the fixed Delphi archive extract, including
  # initial publications, downward revisions, and the last reference week.
  snapshots <- data.frame(
    reference_date = as.Date(c(
      "2024-11-02", "2024-11-16", "2024-11-16", "2025-02-01",
      "2025-02-01", "2025-02-01", "2025-02-08", "2025-08-30"
    )),
    report_date = as.Date(c(
      "2024-11-09", "2024-11-23", "2024-12-07", "2025-02-08",
      "2025-04-26", "2025-05-10", "2025-03-15", "2025-11-08"
    )),
    value = c(0.04, 0.06, 0.05, 4.79, 5.45, 5.44, 7.55, 0.08)
  )
  dat <- nssp_mt_flu_pct_df
  reconstructed <- vapply(seq_len(nrow(snapshots)), function(i) {
    return(sum(dat$count[
      dat$reference_date == snapshots$reference_date[i] &
        dat$report_date <= snapshots$report_date[i]
    ]))
  }, numeric(1))
  expect_equal(reconstructed, snapshots$value, tolerance = 1e-12)
})

test_that("both nowcast interfaces retain fractional observations and draws", {
  dat <- nssp_mt_flu_pct_df[
    nssp_mt_flu_pct_df$report_date <= as.Date("2025-04-05"),
  ]
  triangle <- as_reporting_triangle(dat, delays_unit = "weeks") |>
    truncate_to_delay(max_delay = 8)
  set.seed(123)
  from_triangle <- baselinenowcast(
    triangle,
    scale_factor = 2.5,
    draws = 10,
    preprocess = NULL,
    uncertainty_model = fit_by_horizon_with(fit_normal),
    uncertainty_sampler = sample_normal
  )
  set.seed(123)
  from_dataframe <- baselinenowcast(
    dat,
    delays_unit = "weeks",
    max_delay = 8,
    scale_factor = 2.5,
    draws = 10,
    preprocess = NULL,
    uncertainty_model = fit_by_horizon_with(fit_normal),
    uncertainty_sampler = sample_normal
  )
  rownames(from_dataframe) <- NULL
  rownames(from_triangle) <- NULL
  expect_identical(from_dataframe, from_triangle)
  for (result in list(from_triangle, from_dataframe)) {
    expect_true(all(is.finite(result$pred_count)))
    expect_nowcast_draws(result, n_draws = 10, n_dates = nrow(triangle))
    draws <- result$pred_count[result$nowcast]
    expect_true(any(abs(draws - round(draws)) > 1e-8))
    complete <- !result$nowcast
    observed <- unname(rowSums(triangle)[!apply(is.na(triangle), 1, any)])
    expect_equal(
      result$pred_count[complete], rep(observed, each = 10), tolerance = 1e-12
    )
  }
})
