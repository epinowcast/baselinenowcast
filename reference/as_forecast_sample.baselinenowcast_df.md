# Convert a `baselinenowcast_df` object to a `forecast_sample` object

This function converts a
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
object as returned by
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
to a `forecast_sample` object which can be used for scoring with the
[scoringutils](https://epiforecasts.io/scoringutils/) package.

## Usage

``` r
# S3 method for class 'baselinenowcast_df'
as_forecast_sample(
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
  with `output_type = "samples"`.

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
  [`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).

## Value

A `forecast_sample` object as returned by
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html).

## Details

The nowcast samples in `data` are merged with the latest available
observations in `latest_obs` on `reference_date` and any other shared
columns (e.g. strata columns such as `age_group` or `location`). The
merged data is then passed to
[`scoringutils::as_forecast_sample()`](https://epiforecasts.io/scoringutils/reference/as_forecast_sample.html),
using `pred_count` as the predicted value, `draw` as the sample
identifier, and the column named by `observed` as the observed value.

## See also

Main nowcasting interface functions
[`as_forecast_point.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_point.baselinenowcast_df.md),
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

# Build the full reporting triangle once from the long-form data
full_tri <- as_reporting_triangle(syn_nssp_df) |>
  truncate_to_delay(max_delay = max_delay)

# As-of view for the nowcast: drop rows past `nowcast_date`, then apply the
# diagonal reporting structure so cells reported after it become NA
n_drop <- sum(as.Date(rownames(full_tri)) > nowcast_date)
rep_tri <- full_tri |>
  truncate_to_row(t = n_drop) |>
  apply_reporting_structure() |>
  tail(n = 40)

# Run a probabilistic nowcast
nowcast <- baselinenowcast(rep_tri, draws = 100)

# Truth: total reports observed within `max_delay` of each reference date,
# taken from the same `full_tri` (rolling truth). `as_forecast_sample()`
# scores only the right-truncated reference dates that were actually
# nowcast (the `nowcast` column of the nowcast); the inner-join merge then
# restricts the truth to those dates. The scoring vignette explains why we
# use a rolling rather than latest-vintage truth.
truth_df <- as.data.frame(full_tri)
latest_obs <- aggregate(count ~ reference_date, data = truth_df, FUN = sum)

# Convert and score
fs <- as_forecast_sample(nowcast, latest_obs)
fs
scores <- score(fs)
scores
summarise_scores(scores)
}
```
