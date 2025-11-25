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
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. Default is
  `"days"`.

## Value

NULL, invisibly
