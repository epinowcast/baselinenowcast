# Extract from one matrix only elements that are missing in another

Extract from one matrix only elements that are missing in another

## Usage

``` r
.extract_predictions(point_nowcast_matrix, reporting_triangle)
```

## Arguments

- point_nowcast_matrix:

  Matrix of point nowcast predictions and observations, with rows
  representing the reference times and columns representing the delays.

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

## Value

Matrix containing the elements from `point_nowcast_matrix` for only the
elements that are missing in `reporting_triangle`
