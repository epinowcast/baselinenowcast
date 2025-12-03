# Generate truncated reporting triangles

This function ingests a reporting triangle/matrix and the number of
truncated reporting triangles we want to create, `n`, and iteratively
truncates the reporting triangle, working from the latest reference time
(bottom) to the older reference times (top) for `n` snapshots.

## Usage

``` r
truncate_triangles(
  reporting_triangle,
  n = nrow(reporting_triangle) - sum(is.na(rowSums(reporting_triangle))) - 1,
  validate = TRUE
)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- n:

  Integer indicating the number of retrospective truncated triangles to
  be generated, always starting from the most recent reference time.
  Default is to generate truncated matrices for each row up until there
  are insufficient rows to generate nowcasts from, where the minimum
  requirement is one more than the number of horizon rows (rows
  containing NAs).

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`trunc_rep_tri_list` List of `n` truncated `reporting_triangle` objects
with as many rows as available given the truncation, and the same number
of columns as the input `reporting_triangle`.

## See also

Retrospective data generation functions
[`apply_reporting_structure()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structure.md),
[`apply_reporting_structures()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structures.md),
[`truncate_triangle()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangle.md)

## Examples

``` r
# Generate multiple truncated triangles
truncated_rts <- truncate_triangles(example_reporting_triangle, n = 2)
truncated_rts[1:2]
#> [[1]]
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-04
#> Max delay: 3
#> Structure: 1
#> 
#>              0  1  2  3
#> 2024-01-01  80 50 25 10
#> 2024-01-02 100 50 20 NA
#> 2024-01-03  90 45 NA NA
#> 2024-01-04 110 NA NA NA
#> 
#> [[2]]
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-03
#> Max delay: 3
#> Structure: 2
#> 
#>              0  1  2  3
#> 2024-01-01  80 50 25 10
#> 2024-01-02 100 50 20 NA
#> 2024-01-03  90 45 NA NA
#> 
```
