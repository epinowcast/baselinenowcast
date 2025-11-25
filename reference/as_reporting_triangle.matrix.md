# Create a `reporting_triangle` from a matrix

This method takes a matrix in the format of a reporting triangle, with
rows as reference dates and columns as delays and elements as incident
case counts and creates a
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object. See
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
for creating from data frames.

## Usage

``` r
# S3 method for class 'matrix'
as_reporting_triangle(data, delays_unit = "days", reference_dates = NULL, ...)
```

## Arguments

- data:

  Matrix of a reporting triangle where rows are reference times, columns
  are delays, and entries are the incident counts. The number of columns
  determines the maximum delay.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. Default is
  `"days"`.

- reference_dates:

  Vector of Date objects or character strings indicating the reference
  dates corresponding to each row of the reporting triangle matrix
  (`data`). If NULL (default), dummy dates starting from 1900-01-01 are
  generated with spacing determined by `delays_unit`.

- ...:

  Additional arguments not used.

## Value

A
[`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object

## See also

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
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
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
rep_tri_mat <- matrix(
  c(
    1, 3, 5, 7, 9,
    4, 7, 8, 0, NA,
    9, 10, 0, NA, NA,
    3, 0, NA, NA, NA,
    6, NA, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)

reference_dates <- seq(
  from = as.Date("2025-01-01"),
  to = as.Date("2025-01-05"),
  by = "day"
)

# max_delay is inferred from matrix dimensions (4 in this case)
rep_tri <- as_reporting_triangle(
  data = rep_tri_mat,
  reference_dates = reference_dates
)
rep_tri
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2025-01-01 to 2025-01-05
#> Max delay: 4
#> Structure: 1
#> 
#>            0  1  2  3  4
#> 2025-01-01 1  3  5  7  9
#> 2025-01-02 4  7  8  0 NA
#> 2025-01-03 9 10  0 NA NA
#> 2025-01-04 3  0 NA NA NA
#> 2025-01-05 6 NA NA NA NA
```
