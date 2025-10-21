# Validate triangle Various checks to make sure that the reporting triangle passed in to [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md) is formatted properly.

Validate triangle Various checks to make sure that the reporting
triangle passed in to
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
is formatted properly.

## Usage

``` r
.validate_triangle(
  triangle,
  max_delay = ncol(triangle) - 1,
  n = nrow(triangle)
)
```

## Arguments

- triangle:

  Matrix of values with rows indicating the time points and columns
  indicating the delays.

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

- n:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay. The default is to use the whole reporting
  triangle, so `nrow(reporting_triangle)`.

## Value

NULL, invisibly
