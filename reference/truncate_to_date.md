# Truncate reporting triangle to a reference date

Drops rows whose reference date is later than the cutoff
`reference_date`, returning the reporting triangle as it would have
looked if observed up to and including that date. This is a date-based
wrapper around
[`truncate_to_row()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_row.md)
that removes the need to compute the number of rows to drop manually.

## Usage

``` r
truncate_to_date(reporting_triangle, reference_date, validate = TRUE)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- reference_date:

  A `Date` of length 1 giving the reference cutoff. Rows with reference
  dates greater than this value are dropped. Reports after this date are
  not removed.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

A `reporting_triangle` object containing only rows with reference dates
less than or equal to `reference_date`. The class and metadata are
preserved.

## See also

Retrospective data generation functions
[`apply_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structure.md),
[`apply_reporting_structures()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structures.md),
[`truncate_to_row()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_row.md),
[`truncate_to_rows()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_rows.md)

## Examples

``` r
ref_dates <- get_reference_dates(example_reporting_triangle)
cutoff <- ref_dates[length(ref_dates) - 1]
truncate_to_date(example_reporting_triangle, reference_date = cutoff)
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-06
#> Max delay: 3
#> Structure: 1
#> 
#>              0  1  2  3
#> 2024-01-01 100 55 30 12
#> 2024-01-02  70 40 24  8
#> 2024-01-03  80 50 25 10
#> 2024-01-04 100 50 20 NA
#> 2024-01-05  90 45 NA NA
#> 2024-01-06 110 NA NA NA
```
