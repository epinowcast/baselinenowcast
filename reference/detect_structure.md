# Detect the structure of a reporting triangle

This function takes as input a reporting triangle matrix and returns an
integer or vector specifying the reporting structure, which will tell
[`construct_triangle`](https://baselinenowcast.epinowcast.org/reference/construct_triangle.md)
how to create new reporting triangles of the same reporting pattern.

## Usage

``` r
detect_structure(reporting_triangle)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

## Value

Integer or vector specifying the reporting structure. If integer,
divides columns evenly by that integer (with last possibly truncated).
If vector, the sum must not be greater than or equal to the number of
columns. Default is 1 (standard triangular structure). If there are no
NAs, will return 0.

## See also

Reporting triangle construction and validation
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md),
[`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/new_reporting_triangle.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)

## Examples

``` r
ragged_triangle <- matrix(
  c(
    1, 3, 5, 7, 9, 7,
    4, 5, 9, 4, NA, NA,
    1, 6, NA, NA, NA, NA,
    3, NA, NA, NA, NA, NA
  ),
  nrow = 4,
  byrow = TRUE
)
detected_structure <- detect_structure(ragged_triangle)
detected_structure
#> [1] 1 1 2
```
