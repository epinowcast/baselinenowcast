# Generate a single retrospective reporting triangle

This function generates a single reporting triangle by removing the
bottom right observations from a truncated reporting triangle matrix. It
is the singular version of
[`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md).

## Usage

``` r
construct_triangle(truncated_reporting_triangle, structure = 1)
```

## Arguments

- truncated_reporting_triangle:

  A single truncated reporting triangle. May or may not contain NAs.

- structure:

  Integer or vector specifying the reporting structure. If integer,
  divides columns evenly by that integer (with last possibly truncated).
  If vector, the sum must not be greater than or equal to the number of
  columns. Default is 1 (standard triangular structure).

## Value

A single retrospective reporting triangle matrix with NAs in the
appropriate positions.

## Examples

``` r
triangle <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, 10,
    82, 42, 6, NA,
    70, 90, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
)

# Standard triangular structure (default)
rep_tri <- construct_triangle(triangle)
rep_tri
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   NA
#> [6,]   82   42   NA   NA
#> [7,]   70   NA   NA   NA

# Ragged structure with 2 columns per delay period
rep_ragged <- construct_triangle(triangle, 2)
rep_ragged
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   10
#> [6,]   82   42    6   NA
#> [7,]   70   90   NA   NA

# Custom structure with explicit column counts
rep_custom <- construct_triangle(triangle, c(1, 2))
rep_custom
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   10
#> [6,]   82   42    6   NA
#> [7,]   70   NA   NA   NA
```
