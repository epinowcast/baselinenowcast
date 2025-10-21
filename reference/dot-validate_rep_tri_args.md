# Validate each item in the reporting triangle

Validate each item in the reporting triangle

## Usage

``` r
.validate_rep_tri_args(
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
