# Validate the inputs to `estimate_and_apply_uncertainty()` to ensure that the reporting triangle, point nowcast matrix, and specified maximum delay are correct.

Validate the inputs to
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)
to ensure that the reporting triangle, point nowcast matrix, and
specified maximum delay are correct.

## Usage

``` r
.validate_multiple_inputs(point_nowcast_matrix, reporting_triangle)
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

NULL, invisibly
