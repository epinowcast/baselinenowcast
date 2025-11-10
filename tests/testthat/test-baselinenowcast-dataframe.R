# Test-specific constants
covid_data_single_strata_wday <- covid_data[covid_data$age_group == "00+", ]
covid_data_age_groups_wday <- covid_data[covid_data$age_group != "00+", ]

expected_cols <- c("pred_count", "draw", "reference_date", "output_type")
expected_cols_nu <- c(
  "pred_count", "draw", "reference_date", "output_type",
  "location", "age_group"
)

test_that(paste0(
  "baselinenowcast.data.frame returns the expected structure ",
  "with and without strata sharing"
), {
  nowcasts_df <- baselinenowcast_test(
    data = covid_data,
    strata_cols = c("age_group", "location")
  )

  expect_blnc_structure(nowcasts_df, expected_cols_nu, "samples")

  # Check that the outputs are different for different age groups
  expect_estimates_differ(
    mean(nowcasts_df$pred_count[nowcasts_df$age_group == "00+"]),
    mean(nowcasts_df$pred_count[nowcasts_df$age_group == "60-79"])
  )

  # Use strata sharing
  # First need to remove all age groups otherwise double count
  covid_data_age_groups <- covid_data[covid_data$age_group != "00+", ]
  nowcasts_df2 <- baselinenowcast_test(
    data = covid_data_age_groups,
    strata_cols = c("age_group", "location"),
    strata_sharing = c("delay", "uncertainty")
  )
  expect_blnc_structure(nowcasts_df2, expected_cols_nu, "samples")

  # Check that the outputs are different between with and without
  # strata sharing
  expect_estimates_differ(
    mean(nowcasts_df$pred_count[nowcasts_df$age_group == "60-79" &
      nowcasts_df$reference_date == max(nowcasts_df$reference_date)]),
    mean(nowcasts_df2$pred_count[nowcasts_df2$age_group == "60-79" &
      nowcasts_df2$reference_date == max(nowcasts_df2$reference_date)])
  )

  # Check that preprocess can be passed as NULL
  nowcasts_df3 <- baselinenowcast_test(
    data = covid_data,
    strata_cols = c("age_group", "location"),
    preprocess = NULL
  )

  expect_blnc_structure(nowcasts_df3, expected_cols_nu, "samples")
})

test_that(paste0(
  "baselinenowcast returns expected structure without errors for all ",
  "different nowcast units"
), {
  skip_if_not_installed("dplyr")
  library(dplyr)
  single_tri_data <- filter(
    covid_data,
    age_group == "00+"
  ) |>
    select(reference_date, report_date, count)

  single_nowcast_df <- baselinenowcast_test(single_tri_data)
  expect_blnc_structure(
    single_nowcast_df,
    expected_cols,
    "samples"
  )

  single_tri_w_metadata <- filter(covid_data, age_group == "00+")

  single_nowcast_df_w_metadata <- baselinenowcast_test(
    data = single_tri_w_metadata
  )
  expect_blnc_structure(
    single_nowcast_df_w_metadata,
    expected_cols,
    "samples"
  )

  test_df <- baselinenowcast_test(
    data = covid_data,
    strata_cols = c("age_group", "location")
  )
  expect_blnc_structure(test_df, expected_cols_nu, "samples")
  expect_true(all(unique(test_df$age_group) %in%
    c("00+", "60-79", "00-04", "80+")))
})

test_that(paste0(
  "baselinenowcast can take explicitly any of the uncertainty args"
), {
  single_nowcast_df <- baselinenowcast_test(
    covid_data_single_strata_wday,
    uncertainty_model = fit_by_horizon
  )
  expect_blnc_structure(
    single_nowcast_df,
    expected_cols,
    "samples"
  )
  single_nowcast_df2 <- baselinenowcast_test(
    covid_data_single_strata_wday,
    uncertainty_sampler = sample_nb
  )
  expect_blnc_structure(
    single_nowcast_df2,
    expected_cols,
    "samples"
  )
  single_nowcast_df3 <- baselinenowcast_test(
    covid_data_single_strata_wday,
    ref_time_aggregator = identity
  )
  expect_blnc_structure(
    single_nowcast_df3,
    expected_cols,
    "samples"
  )
  single_nowcast_df4 <- baselinenowcast_test(
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
  "baselinenowcast errors if multiple strata are passed in and this ",
  "is not specified by nowcast unit"
), {
  expect_error_duplicate_dates(
    baselinenowcast_test(data = covid_data)
  )
})

test_that(paste0(
  "baselinenowcast errors if strata_cols is specified incorrectly"
), {
  expect_err_strata_required(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = c("age_group", "location", "reference_date")
    )
  )
  expect_err_strata_missing(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = c("region", "age_group")
    )
  )
})

test_that(paste0(
  "baselinenowcast errors if column names are incorrect"
), {
  skip_if_not_installed("dplyr")
  covid_data_renamed <- rename(covid_data,
    ref_date = reference_date
  )
  expect_error_missing_names(
    baselinenowcast_test(
      data = covid_data_renamed,
      strata_cols = c("age_group", "location")
    )
  )
  covid_data_renamed2 <- rename(covid_data,
    rep_date = report_date
  )
  expect_error_missing_names(
    baselinenowcast_test(
      data = covid_data_renamed2,
      strata_cols = c("age_group", "location")
    )
  )

  covid_data_renamed3 <- rename(covid_data,
    cases = count
  )
  expect_error_missing_names(
    baselinenowcast_test(
      data = covid_data_renamed3,
      strata_cols = c("age_group", "location")
    )
  )
})

test_that(paste0(
  "baselinenowcast errors if date columns are not of date class"
), {
  covid_data1 <- covid_data
  covid_data1$reference_date <- as.character(covid_data1$reference_date)
  expect_error_wrong_date_class(
    baselinenowcast_test(
      data = covid_data1,
      strata_cols = c("age_group", "location")
    )
  )

  covid_data2 <- covid_data
  covid_data2$report_date <- as.character(covid_data2$report_date)
  expect_error_wrong_date_class(
    baselinenowcast_test(
      data = covid_data2,
      strata_cols = c("age_group", "location")
    )
  )
})

test_that(paste0(
  "baselinenowcast errors if strata sharing args are invalid"
), {
  expect_error(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = c("age_group", "location"),
      strata_sharing = "all"
    ),
    regexp = "Assertion on 'strata_sharing' failed:"
  )
  expect_err_strata_sharing(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = c("age_group", "location"),
      strata_sharing = c("none", "delay")
    )
  )
  expect_err_strata_sharing(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = c("age_group", "location"),
      strata_sharing = c("none", "uncertainty")
    )
  )
})

test_that(paste0(
  "baselinenowcast results are equivalent if first creating a ",
  "reporting triangle and then running"
), {
  skip_if_not_installed("dplyr")
  set.seed(123)
  rep_tri <- as_reporting_triangle(covid_data_single_strata_wday,
    max_delay = 40
  )
  nowcast_df1 <- baselinenowcast(rep_tri, draws = 100)

  set.seed(123)
  nowcast_df2 <- baselinenowcast_test(covid_data_single_strata_wday)
  expect_equal(nowcast_df1$pred_count, nowcast_df2$pred_count, tol = 0.1)
})

test_that("baselinenowcast works with weekday strata", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lubridate")
  set.seed(123)
  nowcast_df2 <- expect_message(
    baselinenowcast_test(
      covid_data_single_strata_wday,
      scale_factor = 4 / 7,
      strata_cols = "weekday_ref_date"
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
    baselinenowcast_test(
      covid_data_Tue,
      scale_factor = 4 / 7,
      strata_cols = "weekday_ref_date"
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
  "baselinenowcast returns expected structure even when dates not ",
  "aligned between strata"
), {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("lubridate")
  set.seed(123)
  covid_data_incomplete <- dplyr::filter(
    covid_data_age_groups_wday,
    !(report_date >= "2021-08-08" & age_group == "60-79")
  )
  nowcast_df <- expect_warning_partial_overlap(
    baselinenowcast_test(
      covid_data_incomplete,
      strata_cols = c("age_group", "location"),
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

  nowcast_df2 <- baselinenowcast_test(
    covid_data_age_groups_wday,
    strata_cols = c("age_group", "location"),
    strata_sharing = c("delay", "uncertainty")
  )
  max_ref_date_60_792 <- nowcast_df2 |>
    dplyr::filter(age_group == "60-79") |>
    dplyr::summarise(max_ref_date = max(reference_date)) |>
    dplyr::pull()
  expect_identical(max_ref_date_60_792, lubridate::ymd("2021-12-01"))
})

test_that(paste0(
  "baselinenowcast errors if strata_sharing is not none and ",
  "weekdays are used as strata"
), {
  skip_if_not_installed("dplyr")
  expect_error_no_overlap(
    expect_warning(
      baselinenowcast_test(
        covid_data_single_strata_wday,
        strata_cols = "weekday_ref_date",
        strata_sharing = c("delay", "uncertainty")
      )
    )
  )
})

test_that(paste0(
  "baselinenowcast produces different results when stratifying by ",
  "weekday vs the default estimate across weekdays"
), {
  skip_if_not_installed("dplyr")

  set.seed(123)
  wday_stratified <- baselinenowcast_test(
    covid_data_single_strata_wday,
    scale_factor = 3 / 7,
    strata_cols = "weekday_ref_date"
  ) |>
    dplyr::mutate(type = "wday stratified")

  final_day_mean_est_wday <- summarise_final_day_mean(
    wday_stratified,
    "reference_date"
  ) |>
    dplyr::pull(mean_est)

  set.seed(123)
  no_wday <- baselinenowcast_test(
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
        size = 0.1, alpha = 0.5
      ) +
      geom_point(aes(x = reference_date, y = cases)) +
      theme_bw()
  }
})

test_that(paste0(
  "baselinenowcast produces different results when sharing across ",
  "age groups for both delay and uncertainty and just each one ",
  "independently"
), {
  skip_if_not_installed("dplyr")

  multiple_ags_fp <- baselinenowcast_test(
    covid_data_age_groups_wday,
    strata_cols = "age_group"
  ) |> dplyr::mutate(type = "no share")

  final_day_mean_no_share <- summarise_final_day_mean(multiple_ags_fp)

  set.seed(123)
  multiple_ags_full_ag <- baselinenowcast_test(
    covid_data_age_groups_wday,
    strata_cols = "age_group",
    strata_sharing = c("delay", "uncertainty")
  ) |> dplyr::mutate(type = "share delay & uncertainty")

  final_day_mean_full_share <- summarise_final_day_mean(
    multiple_ags_full_ag
  )

  set.seed(123)
  multiple_ags_just_delay <- baselinenowcast_test(
    covid_data_age_groups_wday,
    strata_cols = "age_group",
    strata_sharing = "delay"
  ) |> dplyr::mutate(type = "share delay")

  final_day_mean_share_delay <- summarise_final_day_mean(
    multiple_ags_just_delay
  )

  set.seed(123)
  multiple_ags_just_uq <- baselinenowcast_test(
    covid_data_age_groups_wday,
    strata_cols = "age_group",
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
  "baselinenowcast fails with coherent error messages necessary ",
  "strata are missing"
), {
  expect_error_duplicate_dates(
    baselinenowcast_test(data = covid_data)
  )
  expect_no_error(
    baselinenowcast_test(
      data = covid_data,
      strata_cols = "age_group"
    )
  )
})
test_that(paste0(
  "baselinenowcast fails with coherent error messages when we ask ",
  "it to stratify by weekday and then share across strata"
), {
  skip_if_not_installed("lubridate")

  expect_error_no_overlap(
    baselinenowcast_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      strata_cols = "weekday_ref_date",
      strata_sharing = c("delay", "uncertainty")
    )
  )
  expect_error_no_overlap(
    baselinenowcast_test(
      covid_data_age_groups_wday,
      scale_factor = 4,
      strata_cols = c("age_group", "weekday_ref_date"),
      strata_sharing = c("delay", "uncertainty")
    )
  )
  expect_error_no_overlap(
    baselinenowcast_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      strata_cols = "weekday_ref_date",
      strata_sharing = "delay"
    )
  )
  expect_error_no_overlap(
    baselinenowcast_test(
      covid_data_single_strata_wday,
      scale_factor = 4,
      strata_cols = "weekday_ref_date",
      strata_sharing = "uncertainty"
    )
  )
})

test_that(paste0(
  "baselinenowcast errors when trying to do strata sharing with ",
  "weekday strata, but works if separated"
), {
  skip_if_not_installed("dplyr")

  expect_error_no_overlap(
    baselinenowcast_test(
      covid_data_age_groups_wday,
      scale_factor = 3 / 7,
      strata_cols = c(
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
    bnc_df_i <- baselinenowcast_test(
      covid_data_single_wday,
      scale_factor = 3 / 7,
      strata_cols = "age_group",
      strata_sharing = c("delay", "uncertainty")
    )

    bnc_df <- bind_rows(bnc_df, bnc_df_i)
  }
  bnc_df <- dplyr::mutate(bnc_df, type = "age group sharing")

  # Compare to no strata sharing
  no_share_ag <- baselinenowcast_test(
    covid_data_age_groups_wday,
    scale_factor = 3 / 7,
    strata_cols = c(
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
