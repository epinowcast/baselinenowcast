# This will test:
# - The fully aggregated data.frame
# - The all shared day of week nowcat
# - The delay shared
# - The uncertainty shared
# - No sharing
covid_data <- germany_covid19_hosp[germany_covid19_hosp$report_date <= max(germany_covid19_hosp$reference_date) & # nolint
  germany_covid19_hosp$age_group %in% c("00+", "60-79", "80+"), ] # nolint
covid_data_single_strata <- dplyr::filter(
  covid_data,
  age_group == "00+"
)
covid_data_single_strata$weekday_ref_date <- lubridate::wday(covid_data_single_strata$reference_date, # nolint
  label = TRUE
)

set.seed(123)
full_ag <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 4
) |> mutate(type = "full aggregation")
set.seed(123)
all_shared_dow <- baselinenowcast(covid_data_single_strata,
  max_delay = 40,
  draws = 100,
  scale_factor = 4 / 7,
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
