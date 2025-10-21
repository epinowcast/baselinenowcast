# Create a `reporting_triangle` object

Create a `reporting_triangle` object

## Usage

``` r
as_reporting_triangle(
  data,
  max_delay,
  strata = NULL,
  delays_unit = "days",
  ...
)
```

## Arguments

- data:

  Data to be nowcasted.

- max_delay:

  Integer indicating the maximum delay.

- strata:

  Character string indicating the strata. Default is NULL.

- delays_unit:

  Character string specifying the temporal granularity of the delays.
  Options are `"days"`, `"weeks"`, `"months"`, `"years"`. For the matrix
  method, this is simply passed as an item in the `reporting_triangle`
  object which will later be used to create a nowcast data.frame. For
  the data.frame method, this is used to compute the delay in terms of
  the specified unit, and to expand the combinations of reference dates
  and delays to the complete set of combinations in the reporting
  triangle. Default is `"days"`.

- ...:

  Additional arguments passed to methods.

## Value

A
[`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
object

## See also

Other reporting_triangle:
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md),
[`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
