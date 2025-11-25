# Generate a single draw of a nowcast combining observed and predicted values

Generate a single draw of a nowcast combining observed and predicted
values

## Usage

``` r
sample_nowcast(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params,
  uncertainty_sampler = sample_nb,
  ref_time_aggregator = identity,
  delay_aggregator = function(x) rowSums(x, na.rm = TRUE)
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

- uncertainty_sampler:

  Function that ingests a vector or matrix of predictions and a vector
  of uncertainty parameters and generates draws from the observation
  model. Default is `sample_nb` which expects arguments `pred` for the
  vector of predictions and uncertainty parameters for the corresponding
  vector of uncertainty parameters, and draws from a negative binomial
  for each element of the vector.

- ref_time_aggregator:

  Function that operates along the rows (reference times) of the
  retrospective point nowcast matrix before it has been aggregated
  across columns (delays). Default is `identity` which does not
  aggregate across reference times.

- delay_aggregator:

  Function that operates along the columns (delays) of the retrospective
  point nowcast matrix after it has been aggregated across reference
  times. Default is `function(x) rowSums(x, na.rm = TRUE)`.

## Value

Vector of predicted counts at each reference date based on combining the
observed counts and the predicted counts for the unobserved elements.
Returns values for all reference dates in the input `reporting_triangle`
(or fewer if using `ref_time_aggregator`).

## See also

Probabilistic nowcast generation functions
[`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md),
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
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
reporting_triangle <- construct_triangle(rep_tri)
uncertainty_params <- estimate_uncertainty_retro(
  rep_tri,
  n_history_delay = 8,
  n_retrospective_nowcasts = 2
)
nowcast_draw <- sample_nowcast(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params
)
nowcast_draw
#>            [,1]
#>             472
#>             368
#>             534
#>             364
#>             459
#> 2026-03-28  402
#> 2026-03-29  620
#> 2026-03-30  412
#> 2026-03-31  457
#> 2026-04-01  422
```
