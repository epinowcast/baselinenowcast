# Convert a `baselinenowcast_df` object to a `forecast_point` object

This function converts a point
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
object as returned by
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
with `output_type = "point"` to a `forecast_point` object which can be
used for scoring with the
[scoringutils](https://epiforecasts.io/scoringutils/) package.

## Usage

``` r
# S3 method for class 'baselinenowcast_df'
as_forecast_point(
  data,
  latest_obs,
  observed = "count",
  model = "baselinenowcast",
  ...
)
```

## Arguments

- data:

  A
  [baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  object as returned by
  [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
  with `output_type = "point"`.

- latest_obs:

  A data.frame containing the truth to score against, with one row per
  (reference_date, strata) combination. Must contain a `reference_date`
  column and a column with observed counts named according to `observed`
  (default `"count"`). Additional columns shared with `data` (such as
  strata columns) are used as merge keys. `latest_obs` should hold the
  reported total at each reference date evaluated at the same
  `max_delay` horizon used for the nowcast (rolling truth), not the
  partial total available when the nowcast was run.

- observed:

  Character string giving the name of the column in `latest_obs` that
  holds the observed value. Defaults to `"count"` to match the input
  format of
  [`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md).

- model:

  Character string used as the value of the `model` column on the
  returned forecast object. Lets
  [`scoringutils::summarise_scores()`](https://epiforecasts.io/scoringutils/reference/summarise_scores.html)
  run with its default `by = "model"`. Defaults to `"baselinenowcast"`.
  Pass `NULL` to omit the column (e.g. when `data` already carries its
  own `model` column).

- ...:

  Additional arguments passed to
  [`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html).

## Value

A `forecast_point` object as returned by
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html).

## Details

The nowcast point estimates in `data` are merged with the latest
observations in `latest_obs` on `reference_date` and any other shared
columns and then passed to
[`scoringutils::as_forecast_point()`](https://epiforecasts.io/scoringutils/reference/as_forecast_point.html),
using `pred_count` as the predicted value and the column named by
`observed` as the observed value.

## See also

Main nowcasting interface functions
[`as_forecast_sample.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_sample.baselinenowcast_df.md),
[`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md),
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md),
[`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md),
[`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)

## Examples

``` r
if (FALSE) { # interactive() && requireNamespace("scoringutils", quietly = TRUE)
library(scoringutils)

max_delay <- 25
nowcast_date <- as.Date("2026-04-01")

full_tri <- as_reporting_triangle(syn_nssp_df) |>
  truncate_to_delay(max_delay = max_delay)

n_drop <- sum(as.Date(rownames(full_tri)) > nowcast_date)
rep_tri <- full_tri |>
  truncate_to_row(t = n_drop) |>
  apply_reporting_structure() |>
  tail(n = 40)

nowcast <- baselinenowcast(rep_tri, output_type = "point")

# Rolling truth from the same full triangle. `as_forecast_point()` scores
# only the right-truncated nowcast dates (the `nowcast` column); the
# inner-join merge restricts the truth to those dates.
truth_df <- as.data.frame(full_tri)
latest_obs <- aggregate(count ~ reference_date, data = truth_df, FUN = sum)

fp <- as_forecast_point(nowcast, latest_obs)
fp
scores <- score(fp)
scores
summarise_scores(scores)
}
```
