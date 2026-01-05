# Estimate and apply delay from a reporting triangle

This function generates a point nowcast by estimating a delay
distribution from the reporting triangle and applying it to complete the
triangle. If a delay distribution is specified, this will be used to
generate the nowcast, otherwise, a delay distribution will be estimated
from the `reporting_triangle`.

## Usage

``` r
estimate_and_apply_delay(
  reporting_triangle,
  n = nrow(reporting_triangle),
  delay_pmf = NULL,
  validate = TRUE
)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- n:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay. The default is to use the whole reporting
  triangle, so `nrow(reporting_triangle)`.

- delay_pmf:

  Vector of delays assumed to be indexed starting at the first delay
  column in `reporting_triangle`. Default is `NULL`, which will estimate
  a delay from the `reporting_triangle`.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`pt_nowcast_matrix` A `reporting_triangle` object of point nowcasts with
the same structure as the input

## See also

High-level workflow wrapper functions
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md),
[`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
# Estimate and apply delay using default parameters
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = example_reporting_triangle
)
pt_nowcast_matrix
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-07
#> Max delay: 3
#> Structure: 0
#> 
#>              0        1        2        3
#> 2024-01-01 100 55.00000 30.00000 12.00000
#> 2024-01-02  70 40.00000 24.00000  8.00000
#> 2024-01-03  80 50.00000 25.00000 10.00000
#> 2024-01-04 100 50.00000 20.00000 10.76326
#> 2024-01-05  90 45.00000 24.56001 10.10250
#> 2024-01-06 110 60.26455 30.96586 12.73987
#> 2024-01-07  95 52.08273 26.75486 11.00615

# Use downward correction example with specific rows for delay estimation
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = example_downward_corr_rt,
  n = 5
)
pt_nowcast_matrix
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-08
#> Max delay: 3
#> Structure: 0
#> 
#>              0       1         2        3
#> 2024-01-01 100 60.0000 -20.00000 10.00000
#> 2024-01-02 120 70.0000 -25.00000 15.00000
#> 2024-01-03 110 65.0000 -22.00000 12.00000
#> 2024-01-04 130 75.0000 -28.00000 18.00000
#> 2024-01-05 115 68.0000 -24.00000 14.00000
#> 2024-01-06 125 72.0000 -26.00000 16.29400
#> 2024-01-07 105 62.0000 -22.25953 13.79309
#> 2024-01-08  95 55.5951 -20.07221 12.43903

# Provide a pre-computed delay PMF
delay_pmf <- estimate_delay(
  reporting_triangle = example_reporting_triangle
)
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = example_reporting_triangle,
  delay_pmf = delay_pmf
)
pt_nowcast_matrix
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-07
#> Max delay: 3
#> Structure: 0
#> 
#>              0        1        2        3
#> 2024-01-01 100 55.00000 30.00000 12.00000
#> 2024-01-02  70 40.00000 24.00000  8.00000
#> 2024-01-03  80 50.00000 25.00000 10.00000
#> 2024-01-04 100 50.00000 20.00000 10.76326
#> 2024-01-05  90 45.00000 24.56001 10.10250
#> 2024-01-06 110 60.26455 30.96586 12.73987
#> 2024-01-07  95 52.08273 26.75486 11.00615
```
