# Validate constructor inputs for reporting triangle

Validate constructor inputs for reporting triangle

## Usage

``` r
.validate_rep_tri_args(reporting_triangle_matrix, reference_dates, delays_unit)
```

## Arguments

- reporting_triangle_matrix:

  Matrix of reporting triangle

- reference_dates:

  Vector of character strings indicating the reference dates
  corresponding to each row of the reporting triangle matrix (`data`).

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. For the matrix
  method, this is simply passed as an item in the `reporting_triangle`
  object which will later be used to create a nowcast data.frame. For
  the data.frame method, this is used to compute the delay in terms of
  the specified unit, and to expand the combinations of reference dates
  and delays to the complete set of combinations in the reporting
  triangle. Default is `"days"`.

## Value

NULL invisibly
