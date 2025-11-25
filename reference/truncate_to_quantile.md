# Truncate reporting_triangle to quantile-based maximum delay

Automatically determines an appropriate maximum delay based on when a
specified proportion of cases have been reported (CDF cutoff). This is
useful for reducing computational burden when most cases are reported
within a shorter delay window.

## Usage

``` r
truncate_to_quantile(x, p = 0.99)
```

## Arguments

- x:

  A reporting_triangle object

- p:

  Numeric value between 0 and 1 indicating the quantile cutoff. For
  example, p = 0.99 truncates to the delay at which 99% of cases have
  been reported. Default is 0.99.

## Value

A reporting_triangle object truncated to the maximum quantile delay, or
the original object if no truncation is needed

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
[`get_max_delay()`](https://baselinenowcast.epinowcast.org/reference/get_max_delay.md),
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
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
# Create triangle, max_delay is automatically computed
rep_tri <- suppressMessages(as_reporting_triangle(data = data_as_of_df))

# Check the maximum delay in the triangle
ncol(rep_tri)
#> [1] 155

# Truncate to 99th percentile of reporting
rep_tri_trunc <- truncate_to_quantile(rep_tri, p = 0.99)
#> ℹ Truncating to 136 based on 99% quantile.
#> ℹ Truncating from max_delay = 154 to 136.
ncol(rep_tri_trunc)
#> [1] 137

# More aggressive truncation
rep_tri_trunc90 <- truncate_to_quantile(rep_tri, p = 0.90)
#> ℹ Truncating to 62 based on 90% quantile.
#> ℹ Truncating from max_delay = 154 to 62.
ncol(rep_tri_trunc90)
#> [1] 63
```
