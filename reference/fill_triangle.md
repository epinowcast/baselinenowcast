# Generate point nowcast

This function ingests a reporting triangle matrix and optionally, a
delay distribution, and returns a completed reporting square which
represents the point nowcast. If a delay distribution is specified, this
will be used to generate the nowcast, otherwise, a delay distribution
will be estimated from the `reporting_triangle`.

## Usage

``` r
fill_triangle(
  reporting_triangle,
  n = nrow(reporting_triangle),
  delay_pmf = NULL,
  preprocess = preprocess_negative_values,
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

- preprocess:

  Function to apply to the truncated triangle before estimation, or NULL
  to skip preprocessing. Default is
  [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md),
  which handles negative values by redistributing them to earlier
  delays. Set to NULL if you want to preserve negative PMF entries
  (e.g., when working with downward corrections where negative
  probabilities reflect systematic adjustments). Custom preprocess
  functions must accept a `validate` parameter (defaults to TRUE) to
  enable validation optimisation in internal function chains.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`point_nowcast_matrix` Matrix of the same number of rows and columns as
the `reporting_triangle` but with the missing values filled in as point
estimates.

## See also

Point nowcast generation functions
[`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md),
[`fill_triangles()`](https://baselinenowcast.epinowcast.org/reference/fill_triangles.md)

## Examples

``` r
# Fill triangle using default delay estimation
point_nowcast_matrix <- fill_triangle(
  reporting_triangle = example_reporting_triangle
)
point_nowcast_matrix
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
