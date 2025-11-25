# Update reporting_triangle with new matrix data

Internal helper to create a new reporting_triangle from modified matrix
data while preserving the original object's metadata (reference dates,
delays_unit). This simplifies the pattern of converting to matrix,
operating on it, then restoring the reporting_triangle class and
attributes.

## Usage

``` r
.update_triangle_matrix(reporting_triangle, new_matrix, reference_dates = NULL)
```

## Arguments

- reporting_triangle:

  The original
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object.

- new_matrix:

  The modified matrix data.

- reference_dates:

  Optional. Reference dates for the new matrix. If NULL, extracts from
  the original reporting_triangle object.

## Value

A new
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object with the updated matrix data.
