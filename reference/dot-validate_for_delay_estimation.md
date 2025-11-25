# Validate reporting_triangle for delay estimation Domain-specific checks to ensure the reporting triangle is suitable for delay estimation in [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md).

Validate reporting_triangle for delay estimation Domain-specific checks
to ensure the reporting triangle is suitable for delay estimation in
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md).

## Usage

``` r
.validate_for_delay_estimation(triangle, n = nrow(triangle))
```

## Arguments

- triangle:

  Matrix of values with rows indicating the time points and columns
  indicating the delays.

- n:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay. The default is to use the whole reporting
  triangle, so `nrow(reporting_triangle)`.

## Value

NULL, invisibly
