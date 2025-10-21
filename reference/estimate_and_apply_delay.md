# Estimate and apply delay from a reporting triangle

Estimate and apply delay from a reporting triangle

## Usage

``` r
estimate_and_apply_delay(
  reporting_triangle,
  max_delay = ncol(reporting_triangle) - 1,
  ...
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

- ...:

  Additional arguments passed to the `estimate_delay` function

## Value

`pt_nowcast_matrix` Matrix of point nowcasts

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
pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = triangle,
  max_delay = 3,
  n = 4
)
pt_nowcast_matrix
#>      [,1]     [,2]     [,3]     [,4]
#> [1,]   80 50.00000 25.00000 10.00000
#> [2,]  100 50.00000 30.00000 20.00000
#> [3,]   90 45.00000 25.00000 17.78889
#> [4,]   80 40.00000 23.20529 15.92281
#> [5,]   70 35.24853 20.35851 13.96745
```
