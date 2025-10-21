# Generate multiple draws of a nowcast combining observed and predicted values

Generate multiple draws of a nowcast combining observed and predicted
values

## Usage

``` r
sample_nowcasts(
  point_nowcast_matrix,
  reporting_triangle,
  uncertainty_params,
  draws = 1000,
  ...
)
```

## Arguments

- point_nowcast_matrix:

  Matrix of point nowcast predictions and observations, with rows
  representing the reference times and columns representing the delays.

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- uncertainty_params:

  Vector of uncertainty parameters ordered from horizon 1 to the maximum
  horizon. Note that these will be reversed internally to match the
  ordering of the `point_nowcast_matrix` (where a horizon of 1 is the
  last entry).

- draws:

  Integer indicating the number of draws of the predicted nowcast vector
  to generate. Default is `1000`.

- ...:

  Additional arguments passed to `sample_nowcast`.

## Value

Dataframe containing information for multiple draws with columns for the
reference time (`time`), the predicted counts (`pred_count`), and the
draw number (`draw`).

## Examples

``` r
point_nowcast_matrix <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, 16.8,
    80, 40, 21.2, 19.5,
    70, 34.5, 15.4, 9.1
  ),
  nrow = 5,
  byrow = TRUE
)
reporting_triangle <- construct_triangle(point_nowcast_matrix)
disp <- c(0.8, 12.4, 9.1)
nowcast_draws <- sample_nowcasts(
  point_nowcast_matrix,
  reporting_triangle,
  disp,
  draws = 5
)
nowcast_draws
#>    pred_count time draw
#> 1         165    1    1
#> 2         200    2    1
#> 3         174    3    1
#> 4         158    4    1
#> 5          70    5    1
#> 6         165    1    2
#> 7         200    2    2
#> 8         180    3    2
#> 9         164    4    2
#> 10        257    5    2
#> 11        165    1    3
#> 12        200    2    3
#> 13        180    3    3
#> 14        170    4    3
#> 15        230    5    3
#> 16        165    1    4
#> 17        200    2    4
#> 18        168    3    4
#> 19        173    4    4
#> 20        106    5    4
#> 21        165    1    5
#> 22        200    2    5
#> 23        171    3    5
#> 24        150    4    5
#> 25         71    5    5
```
