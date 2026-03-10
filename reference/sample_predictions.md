# Get a dataframe of multiple draws of only the predicted elements of the nowcast vector

Get a dataframe of multiple draws of only the predicted elements of the
nowcast vector

## Usage

``` r
sample_predictions(
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

  Additional arguments passed to `sample_prediction`.

## Value

Dataframe containing the predicted point nowcast vectors indexed by
predicted count (`pred_count`), reference date (`reference_date`), and
the draw index (`draw`). Returns predictions for all reference dates in
the input `reporting_triangle` (or fewer if using
`ref_time_aggregator`).

## See also

Probabilistic nowcast generation functions
[`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md),
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md)

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
nowcast_pred_draws <- sample_predictions(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params,
  draws = 5
)
nowcast_pred_draws
#>    pred_count reference_date draw
#> 1           0     2026-03-23    1
#> 2           0     2026-03-24    1
#> 3           0     2026-03-25    1
#> 4           0     2026-03-26    1
#> 5           0     2026-03-27    1
#> 6          16     2026-03-28    1
#> 7          74     2026-03-29    1
#> 8          27     2026-03-30    1
#> 9          55     2026-03-31    1
#> 10        234     2026-04-01    1
#> 11          0     2026-03-23    2
#> 12          0     2026-03-24    2
#> 13          0     2026-03-25    2
#> 14          0     2026-03-26    2
#> 15          0     2026-03-27    2
#> 16         21     2026-03-28    2
#> 17         53     2026-03-29    2
#> 18         15     2026-03-30    2
#> 19         84     2026-03-31    2
#> 20        131     2026-04-01    2
#> 21          0     2026-03-23    3
#> 22          0     2026-03-24    3
#> 23          0     2026-03-25    3
#> 24          0     2026-03-26    3
#> 25          0     2026-03-27    3
#> 26         10     2026-03-28    3
#> 27         16     2026-03-29    3
#> 28         38     2026-03-30    3
#> 29        106     2026-03-31    3
#> 30        264     2026-04-01    3
#> 31          0     2026-03-23    4
#> 32          0     2026-03-24    4
#> 33          0     2026-03-25    4
#> 34          0     2026-03-26    4
#> 35          0     2026-03-27    4
#> 36         13     2026-03-28    4
#> 37         69     2026-03-29    4
#> 38         83     2026-03-30    4
#> 39         88     2026-03-31    4
#> 40        161     2026-04-01    4
#> 41          0     2026-03-23    5
#> 42          0     2026-03-24    5
#> 43          0     2026-03-25    5
#> 44          0     2026-03-26    5
#> 45          0     2026-03-27    5
#> 46         12     2026-03-28    5
#> 47         50     2026-03-29    5
#> 48         31     2026-03-30    5
#> 49         94     2026-03-31    5
#> 50        274     2026-04-01    5

# Get nowcast pred draws over rolling sum
if (requireNamespace("zoo", quietly = TRUE)) {
  nowcast_pred_draws_rolling_df <- sample_predictions(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params,
    draws = 5,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
  )
  nowcast_pred_draws_rolling_df
}
#>    pred_count reference_date draw
#> 1          NA     2026-03-23    1
#> 2           0     2026-03-24    1
#> 3           0     2026-03-25    1
#> 4           0     2026-03-26    1
#> 5           0     2026-03-27    1
#> 6          41     2026-03-28    1
#> 7         113     2026-03-29    1
#> 8          28     2026-03-30    1
#> 9         332     2026-03-31    1
#> 10        750     2026-04-01    1
#> 11         NA     2026-03-23    2
#> 12          0     2026-03-24    2
#> 13          0     2026-03-25    2
#> 14          0     2026-03-26    2
#> 15          0     2026-03-27    2
#> 16         26     2026-03-28    2
#> 17        171     2026-03-29    2
#> 18        210     2026-03-30    2
#> 19        298     2026-03-31    2
#> 20        715     2026-04-01    2
#> 21         NA     2026-03-23    3
#> 22          0     2026-03-24    3
#> 23          0     2026-03-25    3
#> 24          0     2026-03-26    3
#> 25          0     2026-03-27    3
#> 26         17     2026-03-28    3
#> 27         70     2026-03-29    3
#> 28        189     2026-03-30    3
#> 29        202     2026-03-31    3
#> 30        658     2026-04-01    3
#> 31         NA     2026-03-23    4
#> 32          0     2026-03-24    4
#> 33          0     2026-03-25    4
#> 34          0     2026-03-26    4
#> 35          0     2026-03-27    4
#> 36         30     2026-03-28    4
#> 37        148     2026-03-29    4
#> 38         78     2026-03-30    4
#> 39        252     2026-03-31    4
#> 40        363     2026-04-01    4
#> 41         NA     2026-03-23    5
#> 42          0     2026-03-24    5
#> 43          0     2026-03-25    5
#> 44          0     2026-03-26    5
#> 45          0     2026-03-27    5
#> 46         25     2026-03-28    5
#> 47        137     2026-03-29    5
#> 48        160     2026-03-30    5
#> 49        282     2026-03-31    5
#> 50        524     2026-04-01    5
```
