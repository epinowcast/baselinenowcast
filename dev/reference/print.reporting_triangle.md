# Print a reporting_triangle object

Print a reporting_triangle object

## Usage

``` r
# S3 method for class 'reporting_triangle'
print(x, n_rows = 10, n_cols = 10, ...)
```

## Arguments

- x:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/dev/reference/reporting_triangle-class.md)
  object to print.

- n_rows:

  Maximum number of rows to display. If the triangle has more rows, only
  the last `n_rows` rows are shown. Default is 10. Set to NULL to
  display all rows.

- n_cols:

  Maximum number of columns to display. If the triangle has more
  columns, only the first `n_cols` columns are shown. Default is 10. Set
  to NULL to display all columns.

- ...:

  Additional arguments passed to print methods.

## Value

Invisibly returns the object.

## See also

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reporting_triangle.matrix.md),
[`as_reporting_triangle.tbl_pubdate()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reporting_triangle.tbl_pubdate.md),
[`as_reporting_triangle.triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reporting_triangle.triangle.md),
[`as_reviser_vintages()`](https://baselinenowcast.epinowcast.org/dev/reference/as_reviser_vintages.md),
[`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/assert_reporting_triangle.md),
[`get_delays_from_dates()`](https://baselinenowcast.epinowcast.org/dev/reference/get_delays_from_dates.md),
[`get_delays_unit()`](https://baselinenowcast.epinowcast.org/dev/reference/get_delays_unit.md),
[`get_max_delay()`](https://baselinenowcast.epinowcast.org/dev/reference/get_max_delay.md),
[`get_mean_delay()`](https://baselinenowcast.epinowcast.org/dev/reference/get_mean_delay.md),
[`get_quantile_delay()`](https://baselinenowcast.epinowcast.org/dev/reference/get_quantile_delay.md),
[`get_reference_dates()`](https://baselinenowcast.epinowcast.org/dev/reference/get_reference_dates.md),
[`get_report_dates()`](https://baselinenowcast.epinowcast.org/dev/reference/get_report_dates.md),
[`get_reporting_structure()`](https://baselinenowcast.epinowcast.org/dev/reference/get_reporting_structure.md),
[`head.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/head.reporting_triangle.md),
[`is_reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/is_reporting_triangle.md),
[`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/new_reporting_triangle.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/dev/reference/reporting_triangle-class.md),
[`summary.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/summary.reporting_triangle.md),
[`tail.reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/tail.reporting_triangle.md),
[`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/dev/reference/truncate_to_delay.md),
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/dev/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/dev/reference/validate_reporting_triangle.md)
