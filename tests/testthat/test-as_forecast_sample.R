test_that("as_forecast_sample.baselinenowcast_df converts samples to forecast_sample", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs)
  )

  expect_s3_class(fs, "forecast_sample")
  expect_true(all(
    c("observed", "predicted", "sample_id") %in% names(fs)
  ))
})

test_that("as_forecast_sample.baselinenowcast_df merges strata columns", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE),
    location = "DE",
    stringsAsFactors = FALSE
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs)
  )

  expect_true("location" %in% names(fs))
  expect_true(all(fs$location == "DE"))
})

test_that("as_forecast_sample.baselinenowcast_df accepts a custom observed column", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    confirm = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs, observed = "confirm")
  )

  expect_s3_class(fs, "forecast_sample")
})

test_that("as_forecast_sample.baselinenowcast_df errors on point nowcasts", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast_pt <- suppressWarnings(
    baselinenowcast(
      example_reporting_triangle,
      output_type = "point"
    )
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  expect_error(
    scoringutils::as_forecast_sample(nowcast_pt, latest_obs),
    regexp = "samples"
  )
})

test_that("as_forecast_sample.baselinenowcast_df errors when latest_obs lacks required columns", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 5)
  )

  bad_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle)
  )
  expect_error(
    scoringutils::as_forecast_sample(nowcast, bad_obs),
    regexp = "count"
  )
})
