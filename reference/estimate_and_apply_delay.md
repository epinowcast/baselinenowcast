# Estimate and apply delay from a reporting triangle

Estimate and apply delay from a reporting triangle

## Usage

``` r
estimate_and_apply_delay(reporting_triangle, validate = TRUE, ...)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

- ...:

  Additional arguments passed to the `estimate_delay` function

## Value

`pt_nowcast_matrix` A `reporting_triangle` object of point nowcasts with
the same structure as the input

## See also

High-level workflow wrapper functions
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = example_reporting_triangle
)
pt_nowcast_matrix
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-05
#> Max delay: 3
#> Structure: 0
#> 
#>              0        1        2        3
#> 2024-01-01  80 50.00000 25.00000 10.00000
#> 2024-01-02 100 50.00000 20.00000 10.97165
#> 2024-01-03  90 45.00000 21.72707 10.11533
#> 2024-01-04 110 59.32834 27.24413 12.68600
#> 2024-01-05  95 51.27278 23.53877 10.95949
```
