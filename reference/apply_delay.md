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
apply_delay(reporting_triangle, delay_pmf, validate = TRUE)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle to be nowcasted, with rows
  representing the time points of reference and columns representing the
  delays

- delay_pmf:

  Vector of delays assumed to be indexed starting at the first delay
  column in `reporting_triangle`.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`point_nowcast_matrix` Matrix of the same number of rows and columns as
the `rep_mat_to_nowcast` but with the missing values filled in as point
estimates

## Examples

``` r
# Example 1: Standard usage with example dataset
delay_pmf <- estimate_delay(example_reporting_triangle)
point_nowcast_matrix <- apply_delay(
  reporting_triangle = example_reporting_triangle,
  delay_pmf = delay_pmf
)
print(point_nowcast_matrix)
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

# Example 2: Using delay PMF with negative entries from downward corrections
delay_pmf_negative <- c(0.7, 0.4, -0.15, 0.05)
nowcast_with_corrections <- apply_delay(
  reporting_triangle = example_downward_corr_rt,
  delay_pmf = delay_pmf_negative
)
# The nowcast includes negative predictions at delay 2,
# correctly reflecting expected downward corrections
print(nowcast_with_corrections)
#> Reporting Triangle
#> ℹ The reporting triangle does not contain any missing values.
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-08
#> Max delay: 3
#> Structure: 0
#> 
#>              0        1         2         3
#> 2024-01-01 100 60.00000 -20.00000 10.000000
#> 2024-01-02 120 70.00000 -25.00000 15.000000
#> 2024-01-03 110 65.00000 -22.00000 12.000000
#> 2024-01-04 130 75.00000 -28.00000 18.000000
#> 2024-01-05 115 68.00000 -24.00000 14.000000
#> 2024-01-06 125 72.00000 -26.00000  9.002632
#> 2024-01-07 105 62.00000 -22.75909  7.594258
#> 2024-01-08  95 54.45714 -20.36688  6.796856
```
