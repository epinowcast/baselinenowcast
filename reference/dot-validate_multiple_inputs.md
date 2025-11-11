# Validate the inputs to `estimate_and_apply_uncertainty()` to ensure that the reporting triangle and point nowcast matrix are compatible.

Validate the inputs to
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)
to ensure that the reporting triangle and point nowcast matrix are
compatible.

## Usage

``` r
.validate_multiple_inputs(point_nowcast_matrix, reporting_triangle)
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

## Value

NULL invisibly
