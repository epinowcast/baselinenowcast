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
  expect_s3_class(nowcast_df, "data.frame")
  expect_s3_class(nowcast_df, "baselinenowcast_df")
  expect_true(all(expected_cols %in% colnames(nowcast_df)))
  expect_identical(nowcast_df$output_type[1], "samples")
  pt_nowcast_df <- baselinenowcast(rep_tri,
    output_type = "point"
  )
  expect_s3_class(pt_nowcast_df, "data.frame")
  expect_s3_class(pt_nowcast_df, "baselinenowcast_df")
  expect_identical(pt_nowcast_df$output_type[1], "point")
  expect_identical(pt_nowcast_df$draw[1], 1)
  expect_true(all(expected_cols %in% colnames(pt_nowcast_df)))

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
    output_type = "point"
  )
  expect_s3_class(pt_nowcast, "data.frame")
  expect_true(all(expected_cols %in% colnames(pt_nowcast)))
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

  expect_failure(expect_equal(
    mean_dif_delay$mean_nc,
    mean_nowcast$mean_nc,
    tol = 0.1
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
    mean_nowcast$sd_nc,
    tol = 0.1
  ))
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
  expect_error(
    assert_baselinenowcast_df(
      nowcast_df_dates
    ),
    regexp = "Must be of class 'Date', not 'character'" # nolint
  )

  expect_error(
    assert_baselinenowcast_df(
      list(nowcast_df)
    ),
    regexp = "Assertion on 'data' failed: Must be of type 'data.frame', not 'list'" # nolint
  )
})

# Keep only selected age groups
covid_data <- germany_covid19_hosp[germany_covid19_hosp$report_date <= max(germany_covid19_hosp$reference_date) & # nolint
  germany_covid19_hosp$age_group %in% c("00+", "60-79", "80+"), ] # nolint
covid_data_delay_rm <- covid_data[, names(covid_data) != "delay"]
test_that("baselinenowcast.data.frame works as expected with and without strata sharing", { # nolint
  nowcasts_df <- baselinenowcast(
    data = covid_data,
    max_delay = 40,
    draws = 100,
    nowcast_unit = c("age_group", "location")
  )
  expected_cols <- c(
    "pred_count", "draw", "reference_date", "output_type",
    "location", "age_group"
  )
  expect_s3_class(nowcasts_df, "data.frame")
  expect_s3_class(nowcasts_df, "baselinenowcast_df")
  expect_true(all(expected_cols %in% colnames(nowcasts_df)))
  expect_identical(nowcasts_df$output_type[1], "samples")

  # Check that the outputs are different for different age groups
  expect_failure(
    expect_equal(
      mean(nowcasts_df$pred_count[nowcasts_df$age_group == "00+"]),
      mean(nowcasts_df$pred_count[nowcasts_df$age_group == "60-79"]),
      tol = 0.01
    )
  )

  # Use strata sharing
  # First need to remove all age groups otherwise double count
  covid_data_age_groups <- covid_data_delay_rm[covid_data_delay_rm$age_group != "00+", ] # nolint
  nowcasts_df2 <- baselinenowcast(
    data = covid_data_age_groups,
    max_delay = 40,
    draws = 100,
    nowcast_unit = c("age_group", "location"),
    strata_sharing = c("delay", "uncertainty")
  )
  expected_cols <- c(
    "pred_count", "draw", "reference_date", "output_type",
    "location", "age_group"
  )
  expect_s3_class(nowcasts_df2, "data.frame")
  expect_s3_class(nowcasts_df2, "baselinenowcast_df")
  expect_true(all(expected_cols %in% colnames(nowcasts_df2)))
  expect_identical(nowcasts_df2$output_type[1], "samples")

  # Check that the outputs are different between with and without strata sharing
  expect_failure(
    expect_equal(
      mean(nowcasts_df$pred_count[nowcasts_df$age_group == "60-79" &
        nowcasts_df$reference_date == max(nowcasts_df$reference_date)]),
      mean(nowcasts_df2$pred_count[nowcasts_df2$age_group == "60-79" &
        nowcasts_df2$reference_date == max(nowcasts_df$reference_date)]),
      tol = 0.01
    )
  )
})

test_that("`baselinenowcast` works for all differet nowcast units", {
  skip_if_not_installed("dplyr")
  library(dplyr)
  single_tri_data <- covid_data |>
    filter(age_group == "00+") |>
    select(reference_date, report_date, count)

  single_nowcast_df <- baselinenowcast(single_tri_data,
    draws = 100,
    max_delay = 40
  )
  expected_cols1 <- c(
    "pred_count", "draw", "reference_date", "output_type"
  )
  expect_s3_class(single_nowcast_df, "data.frame")
  expect_s3_class(single_nowcast_df, "baselinenowcast_df")
  expect_true(all(expected_cols1 %in% colnames(single_nowcast_df)))

  single_tri_w_metadata <- covid_data |>
    filter(age_group == "00+") |>
    select(-delay)

  single_nowcast_df_w_metadata <- baselinenowcast(
    data = single_tri_w_metadata,
    draws = 100,
    max_delay = 40
  )
  expected_cols2 <- c(
    "pred_count", "draw", "reference_date", "output_type",
    "location", "age_group"
  )
  expect_s3_class(single_nowcast_df_w_metadata, "data.frame")
  expect_s3_class(single_nowcast_df_w_metadata, "baselinenowcast_df")
  expect_true(all(expected_cols2 %in% colnames(single_nowcast_df_w_metadata)))

  test_df <- baselinenowcast(
    data = covid_data_delay_rm,
    draws = 100,
    max_delay = 40
  )
  expect_s3_class(test_df, "data.frame")
  expect_s3_class(test_df, "baselinenowcast_df")
  expect_true(all(expected_cols2 %in% colnames(test_df)))
  expect_true(all(unique(test_df$age_group) %in% c("00+", "60-79", "80+")))
})

test_that("baselinenowcast errors if extra delay column is passed in because it treats this as a nowcast unit", { # nolint
  expect_error(
    baselinenowcast(
      data = covid_data,
      draws = 100,
      max_delay = 40
    ),
    regexp = "`max_delay` specified is larger than the maximum delay in the data." # nolint
  ) # nolint
})

test_that("baselinenowcast errors if nowcast unit is specified incorrectly", {
  expect_error(
    baselinenowcast(
      data = covid_data_delay_rm,
      draws = 100,
      nowcast_unit = c("age_group", "location", "reference_date")
    ),
    regexp = "`nowcast_unit` cannot contain any of the required columns"
  )
  expect_error(
    baselinenowcast(
      data = covid_data_delay_rm,
      draws = 100,
      reference_date = "ref_date",
      nowcast_unit = c("age_group", "location", "ref_date")
    ),
    regexp = "`nowcast_unit` cannot contain any of the required columns"
  )
  expect_error(
    baselinenowcast(
      data = covid_data_delay_rm,
      draws = 100,
      nowcast_unit = c("region", "age_group")
    ),
    regexp = "`nowcast_unit`, if specified, must be a column in `data`."
  )
})
