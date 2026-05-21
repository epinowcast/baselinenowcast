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

test_that("as_forecast_sample.baselinenowcast_df merges on strata columns", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  base <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  # Build a two-strata nowcast by duplicating the single-strata nowcast
  nowcast_de <- base
  nowcast_de$location <- "DE"
  nowcast_uk <- base
  nowcast_uk$location <- "UK"
  nowcast <- rbind(nowcast_de, nowcast_uk)
  class(nowcast) <- c("baselinenowcast_df", "data.frame")

  ref_dates <- get_reference_dates(example_reporting_triangle)
  obs_counts <- rowSums(example_reporting_triangle, na.rm = TRUE)
  latest_obs <- data.frame(
    reference_date = rep(ref_dates, 2),
    count = c(obs_counts, obs_counts + 5L),
    location = rep(c("DE", "UK"), each = length(ref_dates)),
    stringsAsFactors = FALSE
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs)
  )

  expect_true("location" %in% names(fs))
  expect_setequal(unique(fs$location), c("DE", "UK"))
  # The UK observations are offset by +5; check that they routed correctly
  fs_df <- as.data.frame(fs)
  cols <- c("reference_date", "observed")
  uk_obs <- unique(fs_df[fs_df$location == "UK", cols])
  de_obs <- unique(fs_df[fs_df$location == "DE", cols])
  uk_obs <- uk_obs[order(uk_obs$reference_date), ]
  de_obs <- de_obs[order(de_obs$reference_date), ]
  expect_identical(
    as.integer(uk_obs$observed - de_obs$observed),
    rep(5L, nrow(de_obs))
  )
})

test_that("as_forecast_sample.baselinenowcast_df errors on duplicated latest_obs merge keys", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  ref_dates <- get_reference_dates(example_reporting_triangle)
  obs_counts <- rowSums(example_reporting_triangle, na.rm = TRUE)
  dup_obs <- data.frame(
    reference_date = c(ref_dates, ref_dates[1]),
    count = c(obs_counts, obs_counts[1])
  )

  expect_error(
    scoringutils::as_forecast_sample(nowcast, dup_obs),
    regexp = "duplicated rows"
  )
})

test_that("as_forecast_sample.baselinenowcast_df warns when latest_obs misses reference dates", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  ref_dates <- get_reference_dates(example_reporting_triangle)
  # Drop the most recent date, which is within the scored nowcast window
  partial_obs <- data.frame(
    reference_date = head(ref_dates, -1),
    count = head(rowSums(example_reporting_triangle, na.rm = TRUE), -1)
  )

  expect_warning(
    suppressPackageStartupMessages(
      scoringutils::as_forecast_sample(nowcast, partial_obs)
    ),
    regexp = "no matching observation"
  )
})

test_that("as_forecast_sample.baselinenowcast_df scores only the flagged nowcast dates", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  nowcast_dates <- unique(nowcast$reference_date[nowcast$nowcast])
  ref_dates <- get_reference_dates(example_reporting_triangle)
  # Some dates are fully observed, so the scored set is a strict subset
  expect_lt(length(nowcast_dates), length(ref_dates))

  latest_obs <- data.frame(
    reference_date = ref_dates,
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs)
  )

  expect_setequal(unique(fs$reference_date), nowcast_dates)
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

test_that("as_forecast_point.baselinenowcast_df converts point to forecast_point", { # nolint
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast_pt <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, output_type = "point")
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fp <- suppressPackageStartupMessages(
    scoringutils::as_forecast_point(nowcast_pt, latest_obs)
  )

  expect_s3_class(fp, "forecast_point")
  expect_true(all(c("observed", "predicted") %in% names(fp)))
})

test_that("as_forecast_sample.baselinenowcast_df adds a default model column", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 5)
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs)
  )
  expect_true("model" %in% names(fs))
  expect_true(all(fs$model == "baselinenowcast"))

  fs2 <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs, model = "custom")
  )
  expect_true(all(fs2$model == "custom"))

  fs3 <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs, model = NULL)
  )
  expect_false("model" %in% names(fs3))
})

test_that("as_forecast_sample.baselinenowcast_df keeps existing model column", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 5)
  )
  nowcast$model <- "pre-tagged"
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  fs <- suppressPackageStartupMessages(
    scoringutils::as_forecast_sample(nowcast, latest_obs, model = "ignored")
  )
  # Pre-existing column wins; the arg only fills in when absent
  expect_true(all(fs$model == "pre-tagged"))
})

test_that("as_forecast_point.baselinenowcast_df errors on sample nowcasts", {
  skip_on_cran()
  skip_if_not_installed("scoringutils")

  nowcast <- suppressWarnings(
    baselinenowcast(example_reporting_triangle, draws = 10)
  )
  latest_obs <- data.frame(
    reference_date = get_reference_dates(example_reporting_triangle),
    count = rowSums(example_reporting_triangle, na.rm = TRUE)
  )

  expect_error(
    scoringutils::as_forecast_point(nowcast, latest_obs),
    regexp = "point"
  )
})
