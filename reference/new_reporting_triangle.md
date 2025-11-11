# Class constructor for `reporting_triangle` objects

Class constructor for `reporting_triangle` objects

## Usage

``` r
new_reporting_triangle(
  reporting_triangle_matrix,
  reference_dates,
  structure,
  max_delay,
  delays_unit,
  strata = NULL
)
```

## Arguments

- reporting_triangle_matrix:

  Matrix of reporting triangle

- reference_dates:

  Vector of character strings indicating the reference dates
  corresponding to each row of the reporting triangle matrix (`data`).

- structure:

  Integer or vector specifying the reporting structure. If integer,
  divides columns evenly by that integer (with last possibly truncated).
  If vector, the sum must not be greater than or equal to the number of
  columns. Default is 1 (standard triangular structure).

- max_delay:

  Integer indicating the maximum delay.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. For the matrix
  method, this is simply passed as an item in the `reporting_triangle`
  object which will later be used to create a nowcast data.frame. For
  the data.frame method, this is used to compute the delay in terms of
  the specified unit, and to expand the combinations of reference dates
  and delays to the complete set of combinations in the reporting
  triangle. Default is `"days"`.

- strata:

  Character string indicating the strata. Default is NULL.

## Value

An object of class
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)

## See also

Reporting triangle construction and validation
[`as_ChainLadder_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_ChainLadder_triangle.md),
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`as_reporting_triangle.triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.triangle.md),
[`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md),
[`detect_structure()`](https://baselinenowcast.epinowcast.org/reference/detect_structure.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
