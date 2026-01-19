# Generate a nowcast

This function ingests data to be nowcasted and generates a a
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
which contains a probabilistic or point estimate of the final case
counts at each reference date in the `data`. See
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
for details on the input requirements.

## Usage

``` r
baselinenowcast(
  data,
  scale_factor = 3,
  prop_delay = 0.5,
  output_type = c("samples", "point"),
  draws = 1000,
  uncertainty_model = fit_by_horizon,
  uncertainty_sampler = sample_nb,
  ...
)
```

## Arguments

- data:

  Data to be nowcasted

- scale_factor:

  Numeric value indicating the multiplicative factor on the maximum
  delay to be used for estimation of delay and uncertainty. Default is
  `3`.

- prop_delay:

  Numeric value \<1 indicating what proportion of all reference times in
  the reporting triangle to be used for delay estimation. Default is
  `0.5`.

- output_type:

  Character string indicating whether the output should be samples
  (`"samples"`) from the estimate with full uncertainty or whether to
  return the point estimate (`"point"`). Default is `"samples"`. If
  `"point"`estimates are specified, the minimum number of reference
  times needed is the number needed for delay estimation, otherwise, if
  `"samples"` are specified, at least 2 additional reference times are
  required for uncertainty estimation.

- draws:

  Integer indicating the number of probabilistic draws to include if
  `output_type` is `"samples"`. Default is 1000.

- uncertainty_model:

  Function that ingests a matrix of observations and a matrix of
  predictions and returns a vector that can be used to apply uncertainty
  using the same error model. Default is `fit_by_horizon` with arguments
  of `obs` matrix of observations and `pred` the matrix of predictions
  that fits each column (horizon) to a negative binomial observation
  model by default. The user can specify a different fitting model by
  replacing the `fit_model` argument in `fit_by_horizon`.

- uncertainty_sampler:

  Function that ingests a vector or matrix of predictions and a vector
  of uncertainty parameters and generates draws from the observation
  model. Default is `sample_nb` which expects arguments `pred` for the
  vector of predictions and uncertainty parameters for the corresponding
  vector of uncertainty parameters, and draws from a negative binomial
  for each element of the vector.

- ...:

  Additional arguments passed to methods.

## Value

Data.frame of class
[baselinenowcast_df](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)

## See also

Main nowcasting interface functions
[`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md),
[`baselinenowcast.data.frame()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.data.frame.md),
[`baselinenowcast.reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md),
[`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md),
[`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)
