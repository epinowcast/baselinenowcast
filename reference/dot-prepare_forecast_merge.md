# Shared validation and merge for forecast converters

Validates a
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
object, joins it with observations on `reference_date` plus any shared
strata columns, and warns or aborts on duplicate keys or missing
coverage. Used by
[`as_forecast_sample.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_sample.baselinenowcast_df.md)
and
[`as_forecast_point.baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/as_forecast_point.baselinenowcast_df.md).

## Usage

``` r
.prepare_forecast_merge(
  data,
  latest_obs,
  observed,
  model,
  required_output_type,
  target
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

- required_output_type:

  Either `"samples"` or `"point"`.

- target:

  Character label for the calling function used in error messages.

## Value

A merged data.frame ready for the relevant `scoringutils` converter.
