# Compute complete row statistics

Compute complete row statistics

## Usage

``` r
.compute_complete_row_stats(object, mat, ref_dates)
```

## Arguments

- object:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object.

- mat:

  Matrix representation of the triangle.

- ref_dates:

  Vector of reference dates.

## Value

List with most_recent_complete_idx, most_recent_complete_date,
most_recent_complete_count, mean_delays, and complete_rows (or NULL if
no complete rows).
