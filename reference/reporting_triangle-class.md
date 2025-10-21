# Reporting Triangle Object

A `reporting_triangle` object contains the data and metadata needed for
nowcasting.

## Structure

A `reporting_triangle` is a list with the following components:

- reporting_triangle_matrix:

  Matrix with which rows are reference times and columns are delays and
  entries are incident cases at each reference time and delay.

- reference_dates:

  Vector of the same length as the rows of the matrix indicating the
  dates corresponding to the reference times in the rows of the
  reporting triangle.

- structure:

  Vector indicating the "structure" of the reporting triangle, see
  [`construct_triangle`](https://baselinenowcast.epinowcast.org/reference/construct_triangle.md)
  for more details.

- max_delay:

  Integer indicating the maximum delay.

- delays_unit:

  Character string indicating the unit of the delays. Valid options are
  "days", "weeks", "months", "years".

- strata:

  Character string indicating the strata.

See the corresponding
[`as_reporting_triangle.matrix`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
and
[`as_reporting_triangle.data.frame`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
functions for more details on the required input formats to generate the
object.

## See also

Other reporting_triangle:
[`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md),
[`as_reporting_triangle.data.frame()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md),
[`as_reporting_triangle.matrix()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
