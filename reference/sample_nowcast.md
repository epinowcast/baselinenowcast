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

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

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

Vector of predicted counts at each reference time based on combining the
observed counts and the predicted counts for the unobserved elements.

## See also

Probabilistic nowcast generation functions
[`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md),
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md),
[`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)

## Examples

``` r
point_nowcast_matrix <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, 16.8,
    80, 40, 21.2, 19.5,
    70, 34.5, 15.4, 9.1
  ),
  nrow = 5,
  byrow = TRUE
)
reporting_triangle <- construct_triangle(point_nowcast_matrix)
disp <- c(0.8, 12.4, 9.1)
nowcast_draw <- sample_nowcast(
  point_nowcast_matrix,
  reporting_triangle,
  disp
)
nowcast_draw
#>      [,1]
#> [1,]  165
#> [2,]  200
#> [3,]  188
#> [4,]  162
#> [5,]  271
```
