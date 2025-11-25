# Compute quantile delay statistics for complete rows

Compute quantile delay statistics for complete rows

## Usage

``` r
.compute_quantile_delay_stats(object, complete_rows, p = 0.99)
```

## Arguments

- object:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object.

- complete_rows:

  Indices of complete rows.

- p:

  Quantile probability (default 0.99).

## Value

Vector of quantile delays for complete rows, or NULL if none.
