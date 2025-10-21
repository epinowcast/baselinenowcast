# Validate triangle to nowcast and delay PMF together Various checks to make sure that the reporting triangle and the delay PMF passed in to [`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md) are formatted properly and compatible.

Validate triangle to nowcast and delay PMF together Various checks to
make sure that the reporting triangle and the delay PMF passed in to
[`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md)
are formatted properly and compatible.

## Usage

``` r
.validate_delay_and_triangle(triangle, delay_pmf)
```

## Arguments

- triangle:

  Matrix of values with rows indicating the time points and columns
  indicating the delays.

- delay_pmf:

  Vector of length of the number of delays indicating the probability of
  a case being reported on a given delay.

## Value

NULL, invisibly
