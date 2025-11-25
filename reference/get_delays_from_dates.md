# Compute delays between report dates and reference dates

Computes delays between report dates and reference dates using the
specified time unit. This is the inverse operation of
[`get_report_dates()`](https://baselinenowcast.epinowcast.org/reference/get_report_dates.md).

## Usage

``` r
get_delays_from_dates(report_dates, reference_dates, delays_unit)
```

## Arguments

- report_dates:

  Date vector of report dates.

- reference_dates:

  Date vector of reference dates.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. Default is
  `"days"`.

## Value

Numeric vector of delays.

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
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
# Compute delays in days
ref_date <- as.Date("2024-01-01")
report_date <- as.Date("2024-01-08")
get_delays_from_dates(report_date, ref_date, "days") # 7
#> [1] 7

# Compute delays in weeks
report_date_weeks <- as.Date("2024-01-15")
get_delays_from_dates(report_date_weeks, ref_date, "weeks") # 2
#> [1] 2
```
