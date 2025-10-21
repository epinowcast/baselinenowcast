# Check that the reporting triangle contains the correct number of columns for the specified maximum delay

Check that the reporting triangle contains the correct number of columns
for the specified maximum delay

## Usage

``` r
.check_to_filter_to_max_delay(triangle, max_delay)
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

reporting_triangle
