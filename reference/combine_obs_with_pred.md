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

  Vector of predicted counts at each reference date. Note that if using
  a reference time or delay aggregator function, this is assumed to have
  already been aggregated.

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

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

A vector of predicted counts at each reference date, for all reference
dates in the input `reporting_triangle` (or fewer if using
`ref_time_aggregator`)

## See also

Probabilistic nowcast generation functions
[`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md),
[`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md),
[`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)

## Examples

``` r
# Use example data
reporting_triangle <- apply_reporting_structure(example_reporting_triangle)
pred_counts <- c(10, 20, 30, 40)
combine_obs_with_pred(pred_counts, reporting_triangle)
#> Warning: longer object length is not a multiple of shorter object length
#> 2024-01-01 2024-01-02 2024-01-03 2024-01-04 2024-01-05 
#>        175        190        165        150        105 

# Example with rolling sum
if (requireNamespace("zoo", quietly = TRUE)) {
  combine_obs_with_pred(pred_counts,
    reporting_triangle,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
  )
}
#> [1] 335 305 230 245
```
