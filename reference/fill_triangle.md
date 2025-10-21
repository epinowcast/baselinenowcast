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
  max_delay = ncol(reporting_triangle) - 1,
  n = nrow(reporting_triangle),
  delay_pmf = NULL
)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

- n:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay. The default is to use the whole reporting
  triangle, so `nrow(reporting_triangle)`.

- delay_pmf:

  Vector of delays assumed to be indexed starting at the first delay
  column in `reporting_triangle`. Default is `NULL`, which will estimate
  a delay from the `reporting_triangle`.

## Value

`point_nowcast_matrix` Matrix of the same number of rows and columns as
the `reporting_triangle` but with the missing values filled in as point
estimates.

## Examples

``` r
triangle <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)
point_nowcast_matrix <- fill_triangle(
  reporting_triangle = triangle
)
point_nowcast_matrix
#>      [,1]     [,2]     [,3]     [,4]
#> [1,]   80 50.00000 25.00000 10.00000
#> [2,]  100 50.00000 30.00000 20.00000
#> [3,]   90 45.00000 25.00000 14.33572
#> [4,]   80 40.00000 23.17697 12.82918
#> [5,]   70 37.26249 20.72155 11.46862
```
