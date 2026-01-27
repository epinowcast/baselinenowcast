# Setup test data

covid_data_single_strata_wday <- covid_data[covid_data$age_group == "00+", ]
covid_data_age_groups_wday <- covid_data[covid_data$age_group != "00+", ]
expected_cols <- c("pred_count", "draw", "reference_date", "output_type")
expected_cols_ag <- c(
  "pred_count", "draw", "reference_date", "output_type", "age_group"
)
expected_cols_ag_loc <- c(
  "pred_count", "draw", "reference_date", "output_type",
  "location", "age_group"
)
expected_cols_wday <- c(
  "pred_count", "draw", "reference_date", "output_type",
  "weekday_ref_date"
)


test_that("baselinenowcast.reporting_triangle_df works for single stratum", {
  # Point estimate - use all available data
  nowcast_point <- baselinenowcast(
    rt_df,
    output_type = "point"
  )

  expect_s3_class(nowcast_point, "baselinenowcast_df")
  expect_true("reference_date" %in% names(nowcast_point))
  expect_true("pred_count" %in% names(nowcast_point))
  expect_equal(nowcast_point$draw, rep(1, nrow(nowcast_point)))

  # Samples - use all available data
  nowcast_samples <- baselinenowcast(
    rt_df,
    output_type = "samples",
    draws = 10
  )

  expect_s3_class(nowcast_samples, "baselinenowcast_df")
  expect_true(max(nowcast_samples$draw) == 10)
})

test_that("baselinenowcast.reporting_triangle_df works for multiple strata", {
  nowcast_multi <- baselinenowcast(
    rt_df_ag,
    output_type = "samples",
    draws = 5
  )

  nowcast_multi2 <- baselinenowcast(
    rt_df_ag_loc,
    output_type = "samples",
    draws = 5
  )

  expect_s3_class(nowcast_multi, "baselinenowcast_df")
  expect_true("age_group" %in% names(nowcast_multi))
  expect_true(all(c("00-04", "60-79", "80+") %in%
    unique(nowcast_multi$age_group)))
  expect_s3_class(nowcast_multi2, "baselinenowcast_df")
  expect_true("age_group" %in% names(nowcast_multi2))
  expect_true("location" %in% names(nowcast_multi2))
  expect_true(all(c("00-04", "60-79", "80+") %in%
    unique(nowcast_multi2$age_group)))
  expect_true(all(c("DE") %in%
    unique(nowcast_multi2$location)))
})


test_that("baselinenowcast.default provides helpful error message", {
  expect_error(
    baselinenowcast(list(a = 1:5)),
    "No baselinenowcast method for class 'list'"
  )

  expect_error(
    baselinenowcast(matrix(1:10, 2, 5)),
    "No baselinenowcast method for class 'matrix'"
  )
})

test_that("baselinenowcast.data.frame shows deprecation warning", {
  expect_warning(
    baselinenowcast(
      covid_data_single_strata_wday,
      output_type = "point"
    ),
    "baselinenowcast.data.frame\\(\\) is deprecated"
  )
})


test_that("baselinenowcast.reporting_triangle_df returns expected structure with strata", {
  # Single stratum
  nowcasts_single <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    by = NULL
  )
  expect_blnc_structure(nowcasts_single, expected_cols)

  # Multiple strata
  nowcasts_multi <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group"
  )
  nowcasts_multi2 <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = c("age_group", "location")
  )
  expect_blnc_structure(nowcasts_multi, expected_cols_ag)
  expect_blnc_structure(nowcasts_multi2, expected_cols_ag_loc)
  expect_true(all(c("00-04", "60-79", "80+") %in% unique(nowcasts_multi$age_group)))
  expect_true(all(c("00-04", "60-79", "80+") %in% unique(nowcasts_multi2$age_group)))
  expect_true("DE" %in% unique(nowcasts_multi2$location))
})

test_that("baselinenowcast.reporting_triangle_df supports different strata_sharing modes", {
  # No sharing
  set.seed(123)
  nowcast_no_share <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = "none"
  )

  # Delay sharing only
  set.seed(123)
  nowcast_delay_share <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = "delay"
  )

  # Uncertainty sharing only
  set.seed(123)
  nowcast_uncertainty_share <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = "uncertainty"
  )

  # Both delay and uncertainty sharing
  set.seed(123)
  nowcast_both_share <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = c("delay", "uncertainty")
  )

  # All should have valid structure and cols
  expect_blnc_structure(nowcast_no_share, expected_cols_ag)
  expect_blnc_structure(nowcast_delay_share, expected_cols_ag)
  expect_blnc_structure(nowcast_uncertainty_share, expected_cols_ag)
  expect_blnc_structure(nowcast_both_share, expected_cols_ag)

  # Results should differ between sharing modes
  expect_false(identical(
    nowcast_no_share$pred_count,
    nowcast_both_share$pred_count
  ))
  expect_false(identical(
    nowcast_delay_share$pred_count,
    nowcast_uncertainty_share$pred_count
  ))
})

test_that("baselinenowcast.reporting_triangle_df returns point nowcasts", {
  nowcast_pt <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    output_type = "point"
  )
  expect_blnc_structure(nowcast_pt, expected_cols_ag, "point")
})

test_that("baselinenowcast.reporting_triangle_df handles preprocess parameter", {
  down_corr_neg_rep_tri_df <- as_reporting_triangle_df(example_downward_corr_rt)
  nowcast_corr <- expect_warning(baselinenowcast(
    down_corr_neg_rep_tri_df,
    preprocess = preprocess_negative_values,
    output_type = "point"
  ))
  expect_s3_class(nowcast_corr, "baselinenowcast_df")

  # With NULL preprocess
  nowcast_negs <- expect_warning(baselinenowcast(
    down_corr_neg_rep_tri_df,
    preprocess = NULL,
    output_type = "point"
  ))

  # Ensure they are not identical
  expect_s3_class(nowcast_negs, "baselinenowcast_df")
  expect_false(identical(
    nowcast_corr$pred_count,
    nowcast_negs$pred_count
  ))
})

test_that("baselinenowcast.reporting_triangle_df errors on invalid strata_sharing", {
  rt_df_test <- as_reporting_triangle_df(
    multi_strata_df,
    by = "age_group"
  ) |> truncate_to_delay(40)

  # Invalid by value
  expect_error(
    baselinenowcast_rt_df_test(
      covid_data_age_groups_wday,
      by = "all"
    ),
    regexp = "`by` columns must be in `data`"
  )

  # Cannot combine "none" with others
  expect_error(
    baselinenowcast(
      as_reporting_triangle_df(covid_data_age_groups_wday,
        by = "age_group"
      ),
      strata_sharing = c("none", "delay")
    ),
    regexp = "strata_sharing.*cannot be both 'none'"
  )
})

test_that("baselinenowcast.reporting_triangle_df produces consistent results with reporting_triangle", {
  skip_if_not_installed("dplyr")

  # Create via reporting_triangle
  set.seed(123)
  rep_tri <- as_reporting_triangle(data_as_of_df) |>
    truncate_to_delay(40)
  nowcast_tri <- baselinenowcast(rep_tri, draws = 100)

  # Create via reporting_triangle_df
  set.seed(123)
  nowcast_df <- baselinenowcast_rt_df_test(data_as_of_df, by = NULL)

  # Results should be similar (allowing for small numerical differences)
  expect_equal(
    mean(nowcast_tri$pred_count),
    mean(nowcast_df$pred_count),
    tolerance = 0.1
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df can take explicitly any of the",
  "uncertainty args"
), {
  single_nowcast_df <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    uncertainty_model = fit_by_horizon
  )
  expect_blnc_structure(
    single_nowcast_df,
    expected_cols,
    "samples"
  )
  single_nowcast_df2 <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    uncertainty_sampler = sample_nb
  )
  expect_blnc_structure(
    single_nowcast_df2,
    expected_cols,
    "samples"
  )
  single_nowcast_df3 <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    ref_time_aggregator = identity
  )
  expect_blnc_structure(
    single_nowcast_df3,
    expected_cols,
    "samples"
  )
  single_nowcast_df4 <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)
  )
  expect_blnc_structure(
    single_nowcast_df4,
    expected_cols,
    "samples"
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors if multiple strata are passed",
  "in and this is not specified by nowcast unit"
), {
  expect_error(
    baselinenowcast_rt_df_test(data = covid_data_age_groups_wday),
    regexp = "Each combination of strata, reference_date, and report_date should appear only once" # nolint
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors if column names are incorrect"
), {
  skip_if_not_installed("dplyr")
  covid_data_renamed <- rename(covid_data,
    ref_date = reference_date
  )
  expect_error(
    baselinenowcast_rt_df_test(
      data = covid_data_renamed,
      by = c("age_group", "location")
    ),
    regexp = "Required columns missing from data"
  )
  covid_data_renamed2 <- rename(covid_data,
    rep_date = report_date
  )
  expect_error(
    baselinenowcast_rt_df_test(
      data = covid_data_renamed2,
      strata_cols = c("age_group", "location")
    ),
    regexp = "Required columns missing from data"
  )

  covid_data_renamed3 <- rename(covid_data,
    cases = count
  )
  expect_error(
    baselinenowcast_rt_df_test(
      data = covid_data_renamed3,
      strata_cols = c("age_group", "location")
    ),
    regexp = "Required columns missing from data"
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors if date columns are not of ",
  "date class"
), {
  covid_data1 <- covid_data
  covid_data1$reference_date <- as.character(covid_data1$reference_date)
  expect_error_wrong_date_class(
    baselinenowcast_rt_df_test(
      data = covid_data1,
      by = c("age_group", "location")
    )
  )

  covid_data2 <- covid_data
  covid_data2$report_date <- as.character(covid_data2$report_date)
  expect_error_wrong_date_class(
    baselinenowcast_rt_df_test(
      data = covid_data2,
      strata_cols = c("age_group", "location")
    )
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors if strata sharing args are",
  "invalid"
), {
  expect_error(
    baselinenowcast_rt_df_test(
      data = covid_data,
      by = c("age_group", "location"),
      strata_sharing = "all"
    ),
    regexp = "Assertion on 'strata_sharing' failed:"
  )
  expect_err_strata_sharing(
    baselinenowcast_rt_df_test(
      data = covid_data,
      by = c("age_group", "location"),
      strata_sharing = c("none", "delay")
    )
  )
  expect_err_strata_sharing(
    baselinenowcast_rt_df_test(
      data = covid_data,
      by = c("age_group", "location"),
      strata_sharing = c("none", "uncertainty")
    )
  )
})

test_that("baselinenowcast.reporting_triangle_df works with weekday strata", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lubridate")
  set.seed(123)
  nowcast_df2 <- expect_message(
    baselinenowcast_rt_df_test(
      covid_data_single_strata_wday,
      scale_factor = 4 / 7,
      by = "weekday_ref_date"
    ),
    regexp = paste0(
      "Data does not contain case counts for all possible reference dates"
    )
  )
  nowcast_df_Tue1 <- nowcast_df2 |>
    dplyr::filter(weekday_ref_date == "Tue") |>
    dplyr::arrange(desc(reference_date), draw)

  set.seed(123)
  covid_data_Tue <- dplyr::filter(
    covid_data_single_strata_wday,
    weekday_ref_date == "Tue"
  )
  nowcast_df_Tue2 <- expect_message(
    baselinenowcast_rt_df_test(
      covid_data_Tue,
      scale_factor = 4 / 7,
      by = "weekday_ref_date"
    ),
    regexp = paste0(
      "Data does not contain case counts for all possible reference dates"
    )
  ) |>
    dplyr::arrange(desc(reference_date), draw)

  expect_equal(mean(nowcast_df_Tue1$pred_count),
    mean(nowcast_df_Tue2$pred_count),
    tol = 0.01
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df returns expected structure even when",
  "dates not aligned between strata"
), {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lubridate")
  set.seed(123)
  covid_data_incomplete <- dplyr::filter(
    covid_data_age_groups_wday,
    !(report_date >= "2021-08-08" & age_group == "60-79")
  )
  nowcast_df <- expect_warning_partial_overlap(
    baselinenowcast_rt_df_test(
      covid_data_incomplete,
      by = c("age_group", "location"),
      strata_sharing = c("delay", "uncertainty")
    )
  )
  expect_s3_class(nowcast_df, "data.frame")
  expect_s3_class(nowcast_df, "baselinenowcast_df")
  max_ref_date_60_79 <- nowcast_df |>
    dplyr::filter(age_group == "60-79") |>
    dplyr::summarise(max_ref_date = max(reference_date)) |>
    dplyr::pull()
  expect_identical(max_ref_date_60_79, lubridate::ymd("2021-08-07"))

  nowcast_df2 <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = c("age_group", "location"),
    strata_sharing = c("delay", "uncertainty")
  )
  max_ref_date_60_792 <- nowcast_df2 |>
    dplyr::filter(age_group == "60-79") |>
    dplyr::summarise(max_ref_date = max(reference_date)) |>
    dplyr::pull()
  expect_identical(max_ref_date_60_792, lubridate::ymd("2021-12-01"))
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors if strata_sharing is not none",
  "and weekdays are used as strata"
), {
  skip_if_not_installed("dplyr")
  expect_error_no_overlap(
    expect_warning(
      baselinenowcast_rt_df_test(
        covid_data_single_strata_wday,
        by = "weekday_ref_date",
        strata_sharing = c("delay", "uncertainty")
      )
    )
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df produces different results when",
  "stratifying by weekday vs the default estimate across weekdays"
), {
  skip_if_not_installed("dplyr")

  set.seed(123)
  wday_stratified <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    scale_factor = 3 / 7,
    by = "weekday_ref_date"
  ) |>
    dplyr::mutate(type = "wday stratified")

  final_day_mean_est_wday <- summarise_final_day_mean(
    wday_stratified,
    "reference_date"
  ) |>
    dplyr::pull(mean_est)

  set.seed(123)
  no_wday <- baselinenowcast_rt_df_test(
    covid_data_single_strata_wday,
    scale_factor = 3
  ) |>
    dplyr::mutate(type = "no wday strata")

  final_day_mean_est_no_wday <- summarise_final_day_mean(
    no_wday,
    "reference_date"
  ) |>
    dplyr::pull(mean_est)

  covid_data_summed <- covid_data_single_strata_wday |>
    dplyr::group_by(reference_date, age_group) |>
    dplyr::summarise(cases = sum(count)) |>
    dplyr::ungroup()

  covid_data_summed_final <- covid_data_summed |>
    dplyr::filter(reference_date == max(reference_date)) |>
    dplyr::pull(cases)

  # Check the two final estimates are different
  expect_estimates_differ(
    final_day_mean_est_wday, final_day_mean_est_no_wday
  )

  # Check that each of them are large than the initial reports
  expect_gt(final_day_mean_est_wday, covid_data_summed_final)
  expect_gt(final_day_mean_est_no_wday, covid_data_summed_final)

  if (interactive()) {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("glue")
    library(ggplot2) # nolint

    comb_data <- bind_rows(
      wday_stratified,
      no_wday
    ) |>
      dplyr::left_join(covid_data_summed) |>
      dplyr::mutate(group_var = glue::glue("{type}-{draw}")) |>
      dplyr::filter(
        reference_date >= max(reference_date) - lubridate::days(40)
      )

    ggplot(comb_data) +
      geom_line(
        aes(
          x = reference_date, y = pred_count, group = group_var,
          color = type
        ),
        linewidth = 0.1, alpha = 0.5
      ) +
      geom_point(aes(x = reference_date, y = cases)) +
      theme_bw()
  }
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df produces different results when",
  "sharing across age groups for both delay and uncertainty and just each one ",
  "independently"
), {
  skip_if_not_installed("dplyr")

  multiple_ags_fp <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group"
  ) |> dplyr::mutate(type = "no share")

  final_day_mean_no_share <- summarise_final_day_mean(multiple_ags_fp)

  set.seed(123)
  multiple_ags_full_ag <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = c("delay", "uncertainty")
  ) |> dplyr::mutate(type = "share delay & uncertainty")

  final_day_mean_full_share <- summarise_final_day_mean(
    multiple_ags_full_ag
  )

  set.seed(123)
  multiple_ags_just_delay <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = "delay"
  ) |> dplyr::mutate(type = "share delay")

  final_day_mean_share_delay <- summarise_final_day_mean(
    multiple_ags_just_delay
  )

  set.seed(123)
  multiple_ags_just_uq <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    by = "age_group",
    strata_sharing = "uncertainty"
  ) |> dplyr::mutate(type = "share uncertainty")

  final_day_mean_share_uq <- summarise_final_day_mean(multiple_ags_just_uq)

  covid_data_summed <- covid_data_age_groups_wday |>
    dplyr::group_by(reference_date, age_group) |>
    dplyr::summarise(cases = sum(count)) |>
    dplyr::ungroup()

  covid_data_summed_final <- covid_data_summed |>
    dplyr::filter(reference_date == max(reference_date))

  expect_estimates_differ(
    final_day_mean_no_share$mean_est,
    final_day_mean_full_share$mean_est
  )
  expect_estimates_differ(
    final_day_mean_no_share$mean_est,
    final_day_mean_share_delay$mean_est
  )
  expect_estimates_differ(
    final_day_mean_no_share$mean_est,
    final_day_mean_share_uq$mean_est
  )
  expect_estimates_differ(
    final_day_mean_full_share$mean_est,
    final_day_mean_share_uq$mean_est
  )
  expect_estimates_differ(
    final_day_mean_full_share$mean_est,
    final_day_mean_share_delay$mean_est
  )
  expect_estimates_differ(
    final_day_mean_share_delay$mean_est,
    final_day_mean_share_uq$mean_est
  )
  expect_true(all(final_day_mean_no_share$mean_est >
    covid_data_summed_final$cases))
  expect_true(all(final_day_mean_full_share$mean_est >
    covid_data_summed_final$cases))
  expect_true(all(final_day_mean_share_delay$mean_est >
    covid_data_summed_final$cases))
  expect_true(all(final_day_mean_share_uq$mean_est >
    covid_data_summed_final$cases))

  if (interactive()) {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("glue")
    library(ggplot2) # nolint

    comb_data <- bind_rows(
      multiple_ags_fp,
      multiple_ags_full_ag,
      multiple_ags_just_delay,
      multiple_ags_just_uq
    ) |>
      dplyr::left_join(covid_data_summed) |>
      dplyr::filter(
        reference_date >= max(reference_date) - lubridate::days(40)
      ) |>
      mutate(group_var = glue::glue("{type}-{draw}"))

    ggplot(comb_data) +
      geom_line(aes(
        x = reference_date, y = pred_count, group = group_var,
        color = type
      ), size = 0.1, alpha = 0.5) +
      geom_point(aes(x = reference_date, y = cases)) +
      facet_wrap(~age_group, nrow = 3, scales = "free_y") +
      theme_bw()
  }
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df fails with coherent error messages ",
  "necessary strata are missing"
), {
  expect_error(
    baselinenowcast_rt_df_test(data = covid_data),
    regexp = "Data contains duplicate combinations"
  )
  expect_no_error(
    baselinenowcast_rt_df_test(
      data = covid_data,
      by = "age_group"
    )
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df fails with coherent error messages",
  "when we ask it to stratify by weekday and then share across strata"
), {
  skip_if_not_installed("lubridate")

  expect_error_no_overlap(
    baselinenowcast_rt_df_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      by = "weekday_ref_date",
      strata_sharing = c("delay", "uncertainty")
    )
  )
  expect_error_no_overlap(
    baselinenowcast_rt_df_test(
      covid_data_age_groups_wday,
      scale_factor = 4,
      by = c("age_group", "weekday_ref_date"),
      strata_sharing = c("delay", "uncertainty")
    )
  )
  expect_error_no_overlap(
    baselinenowcast_rt_df_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      by = "weekday_ref_date",
      strata_sharing = "delay"
    )
  )
  expect_error_no_overlap(
    baselinenowcast_rt_df_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      by = "weekday_ref_date",
      strata_sharing = "uncertainty"
    )
  )
})

test_that(paste0(
  "baselinenowcast.reporting_triangle_df errors when trying to do strata ",
  "sharing with weekday strata, but works if separated"
), {
  skip_if_not_installed("dplyr")

  expect_error_no_overlap(
    baselinenowcast_rt_df_test(
      covid_data_age_groups_wday,
      scale_factor = 3 / 7,
      by = c(
        "weekday_ref_date",
        "age_group"
      ),
      strata_sharing = c("delay", "uncertainty")
    )
  )

  # Separately filter by weekday and then combine
  wdays <- unique(covid_data_age_groups_wday$weekday_ref_date)
  bnc_df <- data.frame()
  for (i in 1:7) {
    covid_data_single_wday <- dplyr::filter(
      covid_data_age_groups_wday,
      weekday_ref_date == wdays[i]
    )
    bnc_df_i <- baselinenowcast_rt_df_test(
      covid_data_single_wday,
      scale_factor = 3 / 7,
      by = "age_group",
      strata_sharing = c("delay", "uncertainty")
    )

    bnc_df <- bind_rows(bnc_df, bnc_df_i)
  }
  bnc_df <- dplyr::mutate(bnc_df, type = "age group sharing")

  # Compare to no strata sharing
  no_share_ag <- baselinenowcast_rt_df_test(
    covid_data_age_groups_wday,
    scale_factor = 3 / 7,
    by = c(
      "weekday_ref_date",
      "age_group"
    )
  ) |>
    dplyr::mutate(type = "no sharing")

  final_day_mean_no_share <- bnc_df |>
    filter(reference_date == max(reference_date)) |>
    group_by(reference_date, age_group) |>
    summarise(mean_est = mean(pred_count))

  final_day_mean_share_by_ag <- no_share_ag |>
    filter(reference_date == max(reference_date)) |>
    group_by(reference_date, age_group) |>
    summarise(mean_est = mean(pred_count))

  expect_estimates_differ(
    final_day_mean_no_share$mean_est,
    final_day_mean_share_by_ag$mean_est
  )

  if (interactive()) {
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("glue")
    library(ggplot2) # nolint

    covid_data_summed <- covid_data_age_groups_wday |>
      dplyr::group_by(reference_date, age_group) |>
      dplyr::summarise(cases = sum(count)) |>
      dplyr::ungroup()
    comb_data <- bind_rows(
      no_share_ag,
      bnc_df
    ) |>
      dplyr::left_join(covid_data_summed) |>
      dplyr::filter(
        reference_date >= max(reference_date) - lubridate::days(40)
      ) |>
      dplyr::mutate(group_var = glue::glue("{type}-{draw}"))
    ggplot(comb_data) +
      geom_line(
        aes(
          x = reference_date, y = pred_count, group = group_var,
          color = type
        ),
        size = 0.1, alpha = 0.5
      ) +
      geom_point(aes(x = reference_date, y = cases)) +
      facet_wrap(~age_group, nrow = 3, scales = "free_y") +
      theme_bw()
  }
})
