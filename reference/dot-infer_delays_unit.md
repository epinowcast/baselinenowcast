# Infer `delays_unit` from `report_date - reference_date` gaps

Looks at the realised delays (the gaps between each report date and its
reference date, in days) and returns the smallest unit that divides
every gap evenly. This correctly handles cases where reference dates are
weekly but delays are daily (or vice versa).

## Usage

``` r
.infer_delays_unit(report_dates, reference_dates)
```

## Arguments

- report_dates:

  Date vector of report dates.

- reference_dates:

  Date vector of reference dates (same length as `report_dates`).

## Value

A character string, one of `"days"` or `"weeks"`.
