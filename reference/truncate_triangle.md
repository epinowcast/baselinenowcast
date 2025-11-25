# Get a single truncated triangle

This function takes in a reporting triangle and an integer `t` and
generates a truncated reporting triangle, removing the last `t`
observations.

## Usage

``` r
truncate_triangle(reporting_triangle, t, validate = TRUE)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- t:

  Integer indicating the number of timepoints to truncate off the bottom
  of the original reporting triangle.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`trunc_rep_tri` A `reporting_triangle` object with `t` fewer rows than
the input. The class and metadata are preserved with updated reference
dates.

## See also

Retrospective data generation functions
[`construct_triangle()`](https://baselinenowcast.epinowcast.org/reference/construct_triangle.md),
[`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md),
[`truncate_triangles()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangles.md)

## Examples

``` r
# Generate single truncated triangle
trunc_rep_tri <- truncate_triangle(example_reporting_triangle, t = 1)
trunc_rep_tri
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
```
