# Get maximum delay from reporting_triangle

Get maximum delay from reporting_triangle

## Usage

``` r
get_max_delay(x, non_zero = FALSE)
```

## Arguments

- x:

  A reporting_triangle object

- non_zero:

  Logical. If TRUE, returns the maximum delay where at least one
  observation is non-zero. Useful for identifying the actual extent of
  the delay distribution. Default FALSE.

## Value

Maximum delay (integer), or -1 if all zero when non_zero = TRUE

## See also

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`as_reporting_triangle.triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.triangle.md),
[`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md),
[`get_delays_from_dates()`](https://baselinenowcast.epinowcast.org/reference/get_delays_from_dates.md),
[`get_delays_unit()`](https://baselinenowcast.epinowcast.org/reference/get_delays_unit.md),
[`get_mean_delay()`](https://baselinenowcast.epinowcast.org/reference/get_mean_delay.md),
[`get_quantile_delay()`](https://baselinenowcast.epinowcast.org/reference/get_quantile_delay.md),
[`get_reference_dates()`](https://baselinenowcast.epinowcast.org/reference/get_reference_dates.md),
[`get_report_dates()`](https://baselinenowcast.epinowcast.org/reference/get_report_dates.md),
[`get_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/get_reporting_structure.md),
[`head.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/head.reporting_triangle.md),
[`is_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/is_reporting_triangle.md),
[`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/new_reporting_triangle.md),
[`print.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/print.reporting_triangle.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md),
[`summary.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/summary.reporting_triangle.md),
[`tail.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/tail.reporting_triangle.md),
[`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_delay.md),
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
# Get maximum delay from triangle structure
max_delay <- get_max_delay(example_reporting_triangle)
max_delay
#> [1] 3

# Get maximum delay with non-zero observations
max_delay_nz <- get_max_delay(example_reporting_triangle, non_zero = TRUE)
max_delay_nz
#> [1] 3
```
