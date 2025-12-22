# Generate multiple draws of a nowcast combining observed and predicted values

Generate multiple draws of a nowcast combining observed and predicted
values

## Usage

``` r
sample_nowcasts(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params,
  draws = 1000,
  ...
)
```

## Arguments

- point_nowcast_matrix:

  Matrix of point nowcast predictions and observations, with rows
  representing the reference times and columns representing the delays.

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- uncertainty_params:

  Vector of uncertainty parameters ordered from horizon 1 to the maximum
  horizon. Note that these will be reversed internally to match the
  ordering of the `point_nowcast_matrix` (where a horizon of 1 is the
  last entry).

- draws:

  Integer indicating the number of draws of the predicted nowcast vector
  to generate. Default is `1000`.

- ...:

  Additional arguments passed to `sample_nowcast`.

## Value

Dataframe containing information for multiple draws with columns for the
reference date (`reference_date`), the predicted counts (`pred_count`),
and the draw number (`draw`). Returns predictions for all reference
dates in the input `reporting_triangle` (or fewer if using
`ref_time_aggregator`).

## See also

Probabilistic nowcast generation functions
[`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md),
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md),
[`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)

## Examples

``` r
# Generate point nowcast and uncertainty params from example data
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data_as_of) |>
  truncate_to_delay(max_delay = 5) |>
  tail(n = 10)
#> ℹ Using max_delay = 154 from data
#> ℹ Truncating from max_delay = 154 to 5.
point_nowcast_matrix <- estimate_and_apply_delay(rep_tri, n = 10)
reporting_triangle <- apply_reporting_structure(rep_tri)
uncertainty_params <- estimate_uncertainty_retro(
  rep_tri,
  n_history_delay = 8,
  n_retrospective_nowcasts = 2
)
nowcast_draws <- sample_nowcasts(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params,
  draws = 5
)
nowcast_draws
#>    pred_count reference_date draw
#> 1         472     2026-03-23    1
#> 2         368     2026-03-24    1
#> 3         534     2026-03-25    1
#> 4         364     2026-03-26    1
#> 5         459     2026-03-27    1
#> 6         409     2026-03-28    1
#> 7         673     2026-03-29    1
#> 8         469     2026-03-30    1
#> 9         362     2026-03-31    1
#> 10        451     2026-04-01    1
#> 11        472     2026-03-23    2
#> 12        368     2026-03-24    2
#> 13        534     2026-03-25    2
#> 14        364     2026-03-26    2
#> 15        459     2026-03-27    2
#> 16        417     2026-03-28    2
#> 17        602     2026-03-29    2
#> 18        476     2026-03-30    2
#> 19        435     2026-03-31    2
#> 20        478     2026-04-01    2
#> 21        472     2026-03-23    3
#> 22        368     2026-03-24    3
#> 23        534     2026-03-25    3
#> 24        364     2026-03-26    3
#> 25        459     2026-03-27    3
#> 26        405     2026-03-28    3
#> 27        604     2026-03-29    3
#> 28        486     2026-03-30    3
#> 29        487     2026-03-31    3
#> 30        462     2026-04-01    3
#> 31        472     2026-03-23    4
#> 32        368     2026-03-24    4
#> 33        534     2026-03-25    4
#> 34        364     2026-03-26    4
#> 35        459     2026-03-27    4
#> 36        403     2026-03-28    4
#> 37        661     2026-03-29    4
#> 38        503     2026-03-30    4
#> 39        386     2026-03-31    4
#> 40        410     2026-04-01    4
#> 41        472     2026-03-23    5
#> 42        368     2026-03-24    5
#> 43        534     2026-03-25    5
#> 44        364     2026-03-26    5
#> 45        459     2026-03-27    5
#> 46        411     2026-03-28    5
#> 47        641     2026-03-29    5
#> 48        520     2026-03-30    5
#> 49        393     2026-03-31    5
#> 50        355     2026-04-01    5
```
