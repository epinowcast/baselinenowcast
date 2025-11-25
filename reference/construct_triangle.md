# Generate a single retrospective reporting triangle

This function generates a single reporting triangle by removing the
bottom right observations from a truncated reporting triangle matrix. It
is the singular version of
[`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md).

## Usage

``` r
construct_triangle(
  truncated_reporting_triangle,
  structure = 1,
  validate = TRUE
)
```

## Arguments

- truncated_reporting_triangle:

  A single truncated reporting_triangle object. May or may not contain
  NAs.

- structure:

  Integer or vector specifying the reporting structure. If integer,
  divides columns evenly by that integer (with last possibly truncated).
  If vector, the sum must not be greater than or equal to the number of
  columns. Default is 1 (standard triangular structure).

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

A single retrospective reporting triangle matrix with NAs in the
appropriate positions.

## See also

Retrospective data generation functions
[`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md),
[`truncate_triangle()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangle.md),
[`truncate_triangles()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangles.md)

## Examples

``` r
# Standard triangular structure (default)
rep_tri <- construct_triangle(example_reporting_triangle)
rep_tri
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-05
#> Max delay: 3
#> Structure: 1, 0, 1, 1
#> 
#>              0  1  2  3
#> 2024-01-01  80 50 25 10
#> 2024-01-02 100 50 20 NA
#> 2024-01-03  90 45 NA NA
#> 2024-01-04 110 NA NA NA
#> 2024-01-05  95 NA NA NA

# Ragged structure with 2 columns per delay period
rep_ragged <- construct_triangle(example_reporting_triangle, 2)
rep_ragged
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-05
#> Max delay: 3
#> Structure: 1, 0, 1, 1
#> 
#>              0  1  2  3
#> 2024-01-01  80 50 25 10
#> 2024-01-02 100 50 20 NA
#> 2024-01-03  90 45 NA NA
#> 2024-01-04 110 NA NA NA
#> 2024-01-05  95 NA NA NA

# Custom structure with explicit column counts
rep_custom <- construct_triangle(example_reporting_triangle, c(1, 2))
rep_custom
#> Reporting Triangle
#> Delays unit: days
#> Reference dates: 2024-01-01 to 2024-01-05
#> Max delay: 3
#> Structure: 1, 0, 1, 1
#> 
#>              0  1  2  3
#> 2024-01-01  80 50 25 10
#> 2024-01-02 100 50 20 NA
#> 2024-01-03  90 45 NA NA
#> 2024-01-04 110 NA NA NA
#> 2024-01-05  95 NA NA NA
```
