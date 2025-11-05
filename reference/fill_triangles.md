# Generate retrospective nowcasts

This function ingests a list of incomplete reporting triangles and
generates a list of point nowcast matrices, based on the delay estimated
in each triangle or the corresponding delay passed in. It uses the
specified `n` number of reference times to estimate the delay in each
retrospective reporting triangle.

## Usage

``` r
fill_triangles(
  retro_reporting_triangles,
  max_delay = min(sapply(retro_reporting_triangles, ncol)) - 1,
  n = min(sapply(retro_reporting_triangles, nrow)),
  delay_pmf = NULL
)
```

## Arguments

- retro_reporting_triangles:

  List of `n` truncated reporting triangle matrices with as many rows as
  available given the truncation.

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

- n:

  Integer indicating the number of reference times (number of rows) to
  use to estimate the delay distribution for each reporting triangle.
  Default is the minimum of the number of rows of all the matrices in
  the `list_of_rts`.

- delay_pmf:

  Vector or list of vectors of delays assumed to be indexed starting at
  the first delay column in each of the matrices in
  `retro_reporting_triangles`. If a list, must of the same length as
  `retro_reporting_triangles`, with elements aligning. Default is `NULL`

## Value

`point_nowcast_matrices` List of the same number of elements as the
input `retro_reporting_triangles`but with each reporting triangle filled
in based on the delay estimated in that reporting triangle.

## See also

Point nowcast generation functions
[`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md),
[`fill_triangle()`](https://baselinenowcast.epinowcast.org/reference/fill_triangle.md)

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

trunc_rts <- truncate_triangles(triangle)
retro_rts <- construct_triangles(trunc_rts)
retro_pt_nowcast_mat_list <- fill_triangles(retro_rts)
retro_pt_nowcast_mat_list[1:3]
#> [[1]]
#>      [,1]     [,2]     [,3]     [,4]
#> [1,]   65 46.00000 21.00000  7.00000
#> [2,]   70 40.00000 20.00000  5.00000
#> [3,]   80 50.00000 10.00000 10.00000
#> [4,]  100 40.00000 31.00000 12.21905
#> [5,]   95 45.00000 21.28807 11.52534
#> [6,]   82 40.47865 18.62742 10.08377
#> 
#> [[2]]
#>      [,1]     [,2]    [,3]     [,4]
#> [1,]   65 46.00000 21.0000 7.000000
#> [2,]   70 40.00000 20.0000 5.000000
#> [3,]   80 50.00000 10.0000 5.386040
#> [4,]  100 40.00000 17.5180 6.059809
#> [5,]   95 49.62717 18.0964 6.260023
#> 
#> [[3]]
#>      [,1]   [,2]     [,3]      [,4]
#> [1,]   65 46.000 21.00000  7.000000
#> [2,]   70 40.000 20.00000  6.896610
#> [3,]   80 50.000 24.15456  8.177534
#> [4,]  100 63.578 30.38396 10.288532
#> 
```
