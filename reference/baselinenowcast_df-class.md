# Nowcast Data.frame Object

A `baselinenowcast_df` object which contains point or probabilistic
nowcasts alongside reference dates and any additional metadata, in tidy
data format. Nowcasts are presented aggregated across delays, by
reference date.

## Value

A `baselinenowcast_df` object. This is a data.frame subclass containing
nowcast results. See the Structure section for details on the required
columns.

## Structure

A `baselinenowcast_df` is a data.frame with the following columns:

- reference_date:

  Dates corresponding to the reference times of the nowcast.

- pred_count:

  Numeric indicating the estimated total counts aggregated across delays
  at each reference date.

- draw:

  Integer indexing the sample from the probabilistic nowcast
  distribution. If `output_type = "point"`, this will be set to 1.

- output_type:

  Character string indicating whether the `pred_count` represents a
  probabilistic draw from the observation model indicated by `"samples"`
  or whether the `pred_count` is a point estimate indicated by
  `"point"`.

See the corresponding
[reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
and
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
function for more details on the required inputs to generate the object.

## See also

Main nowcasting interface functions
[`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md),
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md),
[`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)
