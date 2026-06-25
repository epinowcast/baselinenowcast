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
#> 6          10     2026-03-28    1
#> 7          81     2026-03-29    1
#> 8          49     2026-03-30    1
#> 9         101     2026-03-31    1
#> 10        144     2026-04-01    1
#> 11          0     2026-03-23    2
#> 12          0     2026-03-24    2
#> 13          0     2026-03-25    2
#> 14          0     2026-03-26    2
#> 15          0     2026-03-27    2
#> 16          9     2026-03-28    2
#> 17         66     2026-03-29    2
#> 18         47     2026-03-30    2
#> 19        102     2026-03-31    2
#> 20        197     2026-04-01    2
#> 21          0     2026-03-23    3
#> 22          0     2026-03-24    3
#> 23          0     2026-03-25    3
#> 24          0     2026-03-26    3
#> 25          0     2026-03-27    3
#> 26          9     2026-03-28    3
#> 27         70     2026-03-29    3
#> 28         71     2026-03-30    3
#> 29         91     2026-03-31    3
#> 30        151     2026-04-01    3
#> 31          0     2026-03-23    4
#> 32          0     2026-03-24    4
#> 33          0     2026-03-25    4
#> 34          0     2026-03-26    4
#> 35          0     2026-03-27    4
#> 36         14     2026-03-28    4
#> 37         53     2026-03-29    4
#> 38         68     2026-03-30    4
#> 39         87     2026-03-31    4
#> 40        279     2026-04-01    4
#> 41          0     2026-03-23    5
#> 42          0     2026-03-24    5
#> 43          0     2026-03-25    5
#> 44          0     2026-03-26    5
#> 45          0     2026-03-27    5
#> 46          7     2026-03-28    5
#> 47         44     2026-03-29    5
#> 48         57     2026-03-30    5
#> 49         29     2026-03-31    5
#> 50        265     2026-04-01    5

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
#> 6           7     2026-03-28    1
#> 7          86     2026-03-29    1
#> 8         255     2026-03-30    1
#> 9         102     2026-03-31    1
#> 10        440     2026-04-01    1
#> 11         NA     2026-03-23    2
#> 12          0     2026-03-24    2
#> 13          0     2026-03-25    2
#> 14          0     2026-03-26    2
#> 15          0     2026-03-27    2
#> 16         29     2026-03-28    2
#> 17        111     2026-03-29    2
#> 18         63     2026-03-30    2
#> 19        274     2026-03-31    2
#> 20        180     2026-04-01    2
#> 21         NA     2026-03-23    3
#> 22          0     2026-03-24    3
#> 23          0     2026-03-25    3
#> 24          0     2026-03-26    3
#> 25          0     2026-03-27    3
#> 26         28     2026-03-28    3
#> 27        114     2026-03-29    3
#> 28         45     2026-03-30    3
#> 29        107     2026-03-31    3
#> 30        327     2026-04-01    3
#> 31         NA     2026-03-23    4
#> 32          0     2026-03-24    4
#> 33          0     2026-03-25    4
#> 34          0     2026-03-26    4
#> 35          0     2026-03-27    4
#> 36         32     2026-03-28    4
#> 37        165     2026-03-29    4
#> 38        337     2026-03-30    4
#> 39        185     2026-03-31    4
#> 40        668     2026-04-01    4
#> 41         NA     2026-03-23    5
#> 42          0     2026-03-24    5
#> 43          0     2026-03-25    5
#> 44          0     2026-03-26    5
#> 45          0     2026-03-27    5
#> 46         28     2026-03-28    5
#> 47        184     2026-03-29    5
#> 48         43     2026-03-30    5
#> 49        328     2026-03-31    5
#> 50        593     2026-04-01    5
```
