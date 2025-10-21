# Validate the reporting triangle data.frame

Checks for duplicate reference date report dates, missing columns,
report dates beyond the final reference date, and missing combinations
of delays and reports.

## Usage

``` r
.validate_rep_tri_df(data, delays_unit)
```

## Arguments

- data:

  Data.frame in long tidy form with reference dates, report dates, and
  case counts, used to create a `reporting_triangle` object.

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

NULL, invisibly
