# Generate truncated reporting triangles

This function ingests a reporting triangle/matrix and the number of
truncated reporting triangles we want to create, `n`, and iteratively
truncates the reporting triangle, working from the latest reference time
(bottom) to the older reference times (top) for `n` snapshots.

## Usage

``` r
truncate_triangles(
  reporting_triangle,
  n = nrow(reporting_triangle) - sum(is.na(rowSums(reporting_triangle))) - 1
)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- n:

  Integer indicating the number of retrospective truncated triangles to
  be generated, always starting from the most recent reference time.
  Default is to generate truncated matrices for each row up until there
  are insufficient rows to generate nowcasts from, where the minimum
  requirement is one more than the number of horizon rows (rows
  containing NAs).

## Value

`trunc_rep_tri_list` List of `n` truncated reporting triangle matrices
with as many rows as available given the truncation, and the same number
of columns as `reporting_triangle`.

## Examples

``` r
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

truncated_rts <- truncate_triangles(triangle, n = 2)
truncated_rts[1:2]
#> [[1]]
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   NA
#> [6,]   82   42   NA   NA
#> 
#> [[2]]
#>      [,1] [,2] [,3] [,4]
#> [1,]   65   46   21    7
#> [2,]   70   40   20    5
#> [3,]   80   50   10   10
#> [4,]  100   40   31   20
#> [5,]   95   45   21   NA
#> 
```
