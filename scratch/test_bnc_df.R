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

# Make a very small subset of an example
df1 <- covid_data_single_strata |>
  filter(
    reference_date >= "2022-08-01",
    report_date >= "2022-08-01",
    weekday_ref_date %in% c("Mon", "Tue")
  ) |>
  arrange(reference_date, report_date)
print(df1)

df1 |>
  ungroup() |>
  group_by(delay, weekday_ref_date) |>
  summarise(n_rows = n())
print(df1)

# In this example, we obviously have no set of overlapping reference date
# report date pairs. If we were to just naively add up the counts we would have
# 2 observations for a delay of 0 on Monday by 1 everywhere else which
# would result in a biased estimate of the delay on Mondays (upweighting 0
# delays). So what we want to do is somehow systematically only include the same
# number of delays for each strata. One way to do this with any strata that are
# not following the reference date is just to use the overlapping set of
# ref and report dates, but this isn't possible here because there are none.

# In this instance, we want to just get back the same input for aggregation.
test <- .combine_triangle_dfs(df1) |>
  arrange(reference_date, report_date)
expect_identical(test$count, df1$count)

# If we repeat this with multiple age groups, we want them to just be summed
# across the age groups?

df2 <- covid_data |>
  filter(
    reference_date >= "2022-08-01",
    report_date >= "2022-08-01",
    weekday_ref_date %in% c("Mon", "Tue")
  ) |>
  arrange(reference_date, report_date)

df2 |>
  ungroup() |>
  group_by(delay, weekday_ref_date, age_group) |>
  summarise(n_rows = n()) |>
  print()

test2 <- .combine_triangle_dfs(df2) |>
  arrange(reference_date, report_date)
expect_identical(test2$count[1], sum(df2$count[1:3]))

# What if we exclude a date only from one age group
df3 <- df2[-1, ]
df3 |>
  ungroup() |>
  group_by(delay, weekday_ref_date, age_group) |>
  summarise(n_rows = n()) |>
  print()

test3 <- .combine_triangle_dfs(df3) |>
  arrange(reference_date, report_date)
# Everything will be the same but the first row sums will be excluded
# because these are incomplete
expect_identical(test3$count, test2$count[2:nrow(test2)])



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
  scale_factor = 4 ,
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
