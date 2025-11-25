# Convert ChainLadder triangle to reporting_triangle format

This S3 method converts a ChainLadder triangle object to a
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object, enabling use of baselinenowcast's nowcasting methods.

## Usage

``` r
# S3 method for class 'triangle'
as_reporting_triangle(data, delays_unit = "days", reference_dates = NULL, ...)
```

## Arguments

- data:

  A ChainLadder triangle object (class "triangle").

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. Default is
  `"days"`.

- reference_dates:

  Vector of dates corresponding to the rows of the triangle. If not
  provided, will attempt to coerce row names to dates. If row names
  cannot be coerced to dates and this is not provided, an error will be
  raised.

- ...:

  Additional arguments passed to
  [`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md).

## Value

A
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object. See
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
for details on the structure.

## Details

This method converts a ChainLadder triangle back to baselinenowcast's
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
format. If `reference_dates` is not provided, the function will attempt
to extract dates from the triangle's row names.

The ChainLadder package must be installed to use this function.

The conversion uses
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
internally after extracting the matrix from the ChainLadder triangle
object.

## See also

- [`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md)
  for converting to ChainLadder

- [`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
  for the underlying method

- [`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
  for creating from data frames

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
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
# Create a reporting triangle
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data = data_as_of_df)
#> ℹ Using max_delay = 154 from data

# Convert to ChainLadder triangle
cl_triangle <- as_ChainLadder_triangle(rep_tri)

# Convert back to reporting_triangle (seamless round-trip)
# max_delay is inferred from the ChainLadder triangle dimensions
rep_tri_2 <- as_reporting_triangle(data = cl_triangle)
print(rep_tri_2)
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2025-10-25 to 2026-04-01
#> Max delay: 154
#> Structure: 1
#> 
#> Showing last 10 of 159 rows
#> Showing first 10 of 155 columns
#> 
#>              0   1  2  3  4  5  6  7  8  9
#> 2026-03-23 210 131 34 50 35 12  1 25 20  6
#> 2026-03-24 221  96 22 10 13  6  0  5  9 NA
#> 2026-03-25 291 129 17 26 42 29 23 25 NA NA
#> 2026-03-26 179  96 22 50  9  8 18 NA NA NA
#> 2026-03-27 284  40 41 54 28 12 NA NA NA NA
#> 2026-03-28 217  78 46 14 39 NA NA NA NA NA
#> 2026-03-29 336 161 62 13 NA NA NA NA NA NA
#> 2026-03-30 296  53 55 NA NA NA NA NA NA NA
#> 2026-03-31 210 108 NA NA NA NA NA NA NA NA
#> 2026-04-01 236  NA NA NA NA NA NA NA NA NA
#> 
#> Use print(x, n_rows = NULL, n_cols = NULL) to see all data
```
