# Convert reporting_triangle to reviser vintages format

This function converts a
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object to a vintages object in the wide format used by the
[reviser](https://CRAN.R-project.org/package=reviser) package. `reviser`
provides tools for analysing data revisions and vintages, including
state space nowcasting models and methods for assessing when releases
become stable. Converting to reviser format enables use of those methods
alongside baselinenowcast's nowcasting functionality.

## Usage

``` r
as_reviser_vintages(x, ...)
```

## Arguments

- x:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object to convert.

- ...:

  Additional arguments passed to
  [`reviser::vintages_wide()`](https://docs.ropensci.org/reviser/reference/vintages_wide.html).

## Value

A tibble of class `tbl_pubdate` with a `time` column for the reference
dates and one column per publication date containing the cumulative
reported value as known on that publication date.

## Details

A `reporting_triangle` stores *incremental* counts at each delay column,
while a reviser vintages object stores the *cumulative* reported value
at each publication date. The conversion takes the row-wise cumulative
sum of the reporting triangle and maps each (reference date, delay) cell
to a publication date `reference_date + delay` in the triangle's delay
unit. The long form is then pivoted to wide format using
[`reviser::vintages_wide()`](https://docs.ropensci.org/reviser/reference/vintages_wide.html).

`NA` values in the triangle propagate through the cumulative sum, so any
unobserved cells in the original triangle remain `NA` in the vintages
output.

To convert back to a
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object, use
[`as_reporting_triangle.tbl_pubdate()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.tbl_pubdate.md).

## See also

- [`as_reporting_triangle.tbl_pubdate()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.tbl_pubdate.md)
  for converting back

- [reviser package documentation](https://github.com/cynkra/reviser)

Reporting triangle construction and validation
[`[.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/sub-.reporting_triangle.md),
`[<-.reporting_triangle()`,
[`as.data.frame.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.data.frame.reporting_triangle.md),
[`as.matrix.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as.matrix.reporting_triangle.md),
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`as_reporting_triangle.tbl_pubdate()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.tbl_pubdate.md),
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
# Create a reporting triangle from synthetic NSSP data
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data = data_as_of_df)
#> ℹ Using max_delay = 154 from data

# Convert to reviser vintages (wide) format
vintages <- as_reviser_vintages(rep_tri)
print(vintages)
#> # Vintages data (publication date format):
#> # Time periods:                            159
#> # Vintages:                                159
#>    time       `2025-10-25` `2025-10-26` `2025-10-27` `2025-10-28`
#>    <date>            <dbl>        <dbl>        <dbl>        <dbl>
#>  1 2025-10-25          194          248          274          287
#>  2 2025-10-26           NA          198          251          280
#>  3 2025-10-27           NA           NA          167          225
#>  4 2025-10-28           NA           NA           NA          145
#>  5 2025-10-29           NA           NA           NA           NA
#>  6 2025-10-30           NA           NA           NA           NA
#>  7 2025-10-31           NA           NA           NA           NA
#>  8 2025-11-01           NA           NA           NA           NA
#>  9 2025-11-02           NA           NA           NA           NA
#> 10 2025-11-03           NA           NA           NA           NA
#> # ℹ 149 more rows
#> # ℹ 155 more variables: `2025-10-29` <dbl>, `2025-10-30` <dbl>,
#> #   `2025-10-31` <dbl>, `2025-11-01` <dbl>, `2025-11-02` <dbl>,
#> #   `2025-11-03` <dbl>, `2025-11-04` <dbl>, `2025-11-05` <dbl>,
#> #   `2025-11-06` <dbl>, `2025-11-07` <dbl>, `2025-11-08` <dbl>,
#> #   `2025-11-09` <dbl>, `2025-11-10` <dbl>, `2025-11-11` <dbl>,
#> #   `2025-11-12` <dbl>, `2025-11-13` <dbl>, `2025-11-14` <dbl>, …
```
