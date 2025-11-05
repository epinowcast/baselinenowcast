# Apply the delay to generate a point nowcast

Generate a point estimate of a completed reporting square (or rectangle)
from a reporting triangle that we want to complete with a nowcast and a
delay PMF. Each element is computed by taking the product of the
expected number of total cases assigned to a reference time \$t\$ and
the proportion of those cases reported on delay \$d\$. The formula to
obtain the expected number of total cases as a function of the reporting
delay and previous observations was derived elsewhere. This code was
adapted from code written (under an MIT license) by the Karlsruhe
Institute of Technology RESPINOW German Hospitalization Nowcasting Hub.
Modified from:
https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55
\#nolint

## Usage

``` r
apply_delay(reporting_triangle, delay_pmf)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle to be nowcasted, with rows
  representing the time points of reference and columns representing the
  delays

- delay_pmf:

  Vector of delays assumed to be indexed starting at the first delay
  column in `reporting_triangle`.

## Value

`point_nowcast_matrix` Matrix of the same number of rows and columns as
the `rep_mat_to_nowcast` but with the missing values filled in as point
estimates

## See also

Point nowcast generation functions
[`fill_triangle()`](https://baselinenowcast.epinowcast.org/reference/fill_triangle.md),
[`fill_triangles()`](https://baselinenowcast.epinowcast.org/reference/fill_triangles.md)

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
delay_pmf <- estimate_delay(
  reporting_triangle = triangle,
  max_delay = 3,
  n = 4
)
point_nowcast_matrix <- apply_delay(
  reporting_triangle = triangle,
  delay_pmf = delay_pmf
)
print(point_nowcast_matrix)
#>      [,1]     [,2]     [,3]     [,4]
#> [1,]   80 50.00000 25.00000 10.00000
#> [2,]  100 50.00000 30.00000 20.00000
#> [3,]   90 45.00000 25.00000 17.78889
#> [4,]   80 40.00000 23.20529 15.92281
#> [5,]   70 35.24853 20.35851 13.96745
```
