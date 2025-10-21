# Check that the maximum delay is not too large, error if it is

Check that the maximum delay is not too large, error if it is

## Usage

``` r
.validate_max_delay(triangle, max_delay)
```

## Arguments

- triangle:

  Matrix of values with rows indicating the time points and columns
  indicating the delays.

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

## Value

NULL invisibly
