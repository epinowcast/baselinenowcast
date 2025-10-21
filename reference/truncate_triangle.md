# Get a single truncated triangle

This function takes in a integer `t` and a reporting triangle and
generates a truncated reporting triangle, remove the last `t`
observations.

## Usage

``` r
truncate_triangle(t, reporting_triangle)
```

## Arguments

- t:

  Integer indicating the number of timepoints to truncate off the bottom
  of the original reporting triangle.

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

## Value

`trunc_rep_tri` Matrix with `t` fewer rows than `reporting_triangle`.

## Examples

``` r
# example code
triangle <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, NA,
    82, 42, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
)

trunc_rep_tri <- truncate_triangle(t = 1, reporting_triangle = triangle)
trunc_rep_tri
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   NA
#> [6,]   82   42   NA   NA
```
