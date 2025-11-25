# Summarize a reporting_triangle object

Summarize a reporting_triangle object

## Usage

``` r
# S3 method for class 'reporting_triangle'
summary(object, ...)
```

## Arguments

- object:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object to summarize.

- ...:

  Additional arguments not used.

## Value

Invisibly returns the object.

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
[`tail.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/tail.reporting_triangle.md),
[`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_delay.md),
[`truncate_to_quantile()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_quantile.md),
[`validate_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/validate_reporting_triangle.md)

## Examples

``` r
# Display summary statistics
summary(example_reporting_triangle)
#> Reporting Triangle Summary
#> Dimensions: 5 x 4
#> Reference period: 2024-01-01 to 2024-01-05
#> Max delay: 3 days
#> Structure: 1, 0, 1, 1
#> Most recent complete date: 2024-01-01 (165 cases)
#> Dates requiring nowcast: 4 (complete: 1)
#> Rows with negatives: 0
#> Zeros: 0 (0% of non-NA values)
#> Zeros per row summary:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>       0       0       0       0       0       0 
#> 
#> Mean delay summary (complete rows):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.7879  0.7879  0.7879  0.7879  0.7879  0.7879 
#> 
#> 99% quantile delay (complete rows):
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>       3       3       3       3       3       3 
```
