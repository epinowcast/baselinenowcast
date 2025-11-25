# Validate reporting_triangle constructor arguments

Internal helper to validate the arguments passed to
new_reporting_triangle.

## Usage

``` r
.validate_rep_tri_args(reporting_triangle_matrix, reference_dates, delays_unit)
```

## Arguments

- reporting_triangle_matrix:

  Matrix of reporting triangle data.

- reference_dates:

  Date vector of reference dates.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. Default is
  `"days"`.
