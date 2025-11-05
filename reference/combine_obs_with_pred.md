# Combine observed data with a single prediction draw

Internally it sums observed counts from the reporting triangle by
reference time and adds them to the predicted counts to form a single
draw of the nowcast for the final counts by reference time.

## Usage

``` r
combine_obs_with_pred(
  predicted_counts,
  reporting_triangle,
  ref_time_aggregator = identity,
  delay_aggregator = function(x) rowSums(x, na.rm = TRUE)
)
```

## Arguments

- predicted_counts:

  Vector of predicted counts at each reference time. Note that if using
  a reference time or delay aggregator function, this is assumed to have
  already been aggregated.

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

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

A vector of predicted counts at each reference time

## See also

Probabilistic nowcast generation functions
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md),
[`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)

## Examples

``` r
pred_counts <- c(10, 20, 30, 40)
reporting_matrix <- matrix(
  c(
    7, 9, 4, 3,
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12
  ),
  nrow = 4,
  byrow = TRUE
)
reporting_triangle <- construct_triangle(reporting_matrix)
combine_obs_with_pred(pred_counts, reporting_triangle)
#> [1] 33 26 41 49
# Another example with rolling sum
if (requireNamespace("zoo", quietly = TRUE)) {
  combine_obs_with_pred(pred_counts,
    reporting_triangle,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
  )
}
#> Warning: longer object length is not a multiple of shorter object length
#> [1] 36 34 44 66
```
