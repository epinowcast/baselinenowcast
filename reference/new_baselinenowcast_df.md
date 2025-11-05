# Combine data from a nowcast dataframe, strata, and reference dates

Combines data from a nowcast dataframe, a named list of the strata
associated with the nowcast dataframe, and a vector of reference dates
corresponding to the time column in the `baselinenowcast_df`

## Usage

``` r
new_baselinenowcast_df(baselinenowcast_df, reference_dates, output_type)
```

## Arguments

- baselinenowcast_df:

  Data.frame containing information for multiple draws with columns for
  the reference time (`time`), the predicted counts (`pred_count`), and
  the draw number (`draw`).

- reference_dates:

  Vector of reference dates corresponding to the reference times in the
  `baselinenowcast_df`.

- output_type:

  Character string indicating whether the output should be samples
  (`"samples"`)from the estimate with full uncertainty or whether to
  return the point estimate (`"point"`). Default is `"samples"`.

## Value

An object of class
[`baselinenowcast_df`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)

## See also

Main nowcasting interface functions
[`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md),
[`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
