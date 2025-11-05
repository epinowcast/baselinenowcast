# This will test:
# - The fully aggregated data.frame
# - The all shared day of week nowcat
# - The delay shared
# - The uncertainty shared
# - No sharing
covid_data <- germany_covid19_hosp[germany_covid19_hosp$report_date <= max(germany_covid19_hosp$reference_date) & # nolint
  germany_covid19_hosp$age_group %in% c("00+", "60-79", "80+"), ] # nolint

covid_data$weekday_ref_date <- lubridate::wday(covid_data$reference_date, # nolint
  label = TRUE
)

covid_data_single_strata <- dplyr::filter(
  covid_data,
  age_group == "00+"
)

# Compare weekday stratification vs no weekday stratification----------------
set.seed(123)
single_age_group <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100
) |> mutate(type = "full aggregation")
wday_stratified <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 3 / 7,
  strata_cols = "weekday_ref_date"
)
no_wday <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 3
)
covid_data_summed <- covid_data |>
  group_by(reference_date, age_group) |>
  summarise(cases = sum(count)) |>
  ungroup()
weekday_stratified_data <- wday_stratified |>
  left_join(covid_data_summed |> filter(age_group == "00+"))
# Confirm that they are different (green is lower )
ggplot(weekday_stratified_data |> dplyr::filter(reference_date >= max(reference_date) - lubridate::days(40))) +
  geom_line(aes(x = reference_date, y = pred_count, group = draw),
    color = "darkgreen", size = 0.3, alpha = 0.5
  ) +
  geom_line(
    data = no_wday |>
      dplyr::filter(reference_date >= max(reference_date) - lubridate::days(40)),
    aes(x = reference_date, y = pred_count, group = draw),
    color = "blue", size = 0.1, alpha = 0.5
  ) +
  geom_point(aes(x = reference_date, y = cases))
#  Tests to make:
# - not the same average final value
# - not the same as the data

# Compare across multiple age groups with different pooling ----------------
multiple_ags_fp <- baselinenowcast(
  covid_data,
  max_delay = 40,
  draws = 100,
  strata_cols = c("age_group")
)
multiple_ags_full_ag <- baselinenowcast(
  covid_data,
  max_delay = 40,
  draws = 100,
  strata_cols = c("age_group"),
  strata_sharing = c("delay", "uncertainty")
)
multiple_ags_just_delay <- baselinenowcast(
  covid_data,
  max_delay = 40,
  draws = 100,
  strata_cols = c("age_group"),
  strata_sharing = c("delay")
)
multiple_ags_just_uq <- baselinenowcast(
  covid_data,
  max_delay = 40,
  draws = 100,
  strata_cols = c("age_group"),
  strata_sharing = c("uncertainty")
)
covid_data_summed <- covid_data |>
  group_by(reference_date, age_group) |>
  summarise(cases = sum(count)) |>
  ungroup()
multiple_ags_fp_data <- multiple_ags_fp |>
  left_join(covid_data_summed)
ggplot(multiple_ags_fp_data |> dplyr::filter(
  reference_date >= max(reference_date) - lubridate::days(40)
)) +
  geom_line(aes(x = reference_date, y = pred_count, group = draw),
    color = "blue", size = 0.1, alpha = 0.5
  ) +
  geom_line(
    data = multiple_ags_full_ag |> dplyr::filter(
      reference_date >= max(reference_date) - lubridate::days(40)
    ),
    aes(x = reference_date, y = pred_count, group = draw),
    color = "red", size = 0.1, alpha = 0.5
  ) +
  geom_line(
    data = multiple_ags_just_delay |> dplyr::filter(
      reference_date >= max(reference_date) - lubridate::days(40)
    ),
    aes(x = reference_date, y = pred_count, group = draw),
    color = "green", size = 0.1, alpha = 0.5
  ) +
  geom_line(
    data = multiple_ags_just_uq |> dplyr::filter(
      reference_date >= max(reference_date) - lubridate::days(40)
    ),
    aes(x = reference_date, y = pred_count, group = draw),
    color = "darkorange", size = 0.1, alpha = 0.5
  ) +
  geom_point(aes(x = reference_date, y = cases)) +
  facet_wrap(~age_group, nrow = 3, scales = "free_y")
# Tests to make:
# - final values are all different
# - upper and lower bounds are different






# These will fail
set.seed(123)
muliple_ags_forgot_strata <- baselinenowcast(
  covid_data,
  max_delay = 40,
  draws = 100
)
all_shared_dow <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 4,
  strata_cols = "weekday_ref_date",
  strata_sharing = c("delay", "uncertainty")
) |> mutate(type = "all_shared_dow")
set.seed(123)
delay_shared <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 4,
  strata_cols = "weekday_ref_date",
  strata_sharing = "delay"
) |>
  mutate(type = "delay shared")

set.seed(123)
uq_shared <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 4,
  strata_cols = "weekday_ref_date",
  strata_sharing = "uncertainty"
) |>
  mutate(type = "uncertainty shared")

combined_df <- bind_rows(
  all_shared_dow,
  full_ag,
  delay_shared,
  uq_shared
)
ggplot(combined_df) +
  geom_line(aes(
    x = reference_date, y = pred_counts, group = draw,
    color = type
  ))
