# Estimate uncertainty parameters

This function ingests a list of point nowcast matrices and a
corresponding list of truncated reporting matrices and uses both to
estimate a vector of uncertainty parameters from the observations and
estimates at each horizon, starting at 0 up until the max delay number
of horizons.

## Usage

``` r
estimate_uncertainty(
  point_nowcast_matrices,
  truncated_reporting_triangles,
  retro_reporting_triangles,
  n = length(point_nowcast_matrices),
  uncertainty_model = fit_by_horizon,
  ref_time_aggregator = identity,
  delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
  validate = TRUE
)
```

## Arguments

- point_nowcast_matrices:

  List of point nowcast matrices where rows represent reference time
  points and columns represent delays.

- truncated_reporting_triangles:

  List of truncated reporting matrices, containing all observations as
  of the latest reference time. Elements of list are paired with
  elements of `point_nowcast_matrices`.

- retro_reporting_triangles:

  List of `n` truncated reporting triangle matrices with as many rows as
  available given the truncation.

- n:

  Integer indicating the number of reporting matrices to use to estimate
  the uncertainty parameters.

- uncertainty_model:

  Function that ingests a matrix of observations and a matrix of
  predictions and returns a vector that can be used to apply uncertainty
  using the same error model. Default is `fit_by_horizon` with arguments
  of `obs` matrix of observations and `pred` the matrix of predictions
  that fits each column (horizon) to a negative binomial observation
  model by default. The user can specify a different fitting model by
  replacing the `fit_model` argument in `fit_by_horizon`.

- ref_time_aggregator:

  Function that operates along the rows (reference times) of the
  retrospective point nowcast matrix before it has been aggregated
  across columns (delays). Default is `identity` which does not
  aggregate across reference times.

- delay_aggregator:

  Function that operates along the columns (delays) of the retrospective
  point nowcast matrix after it has been aggregated across reference
  times. Default is `function(x) rowSums(x, na.rm = TRUE)`.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

`uncertainty_params` Vector of length of the number of horizons, with
each element representing the estimate of the uncertainty parameter for
each horizon. The specific parameter type depends on the chosen error
model.

## See also

Observation error estimation functions
[`fit_by_horizon()`](https://baselinenowcast.epinowcast.org/reference/fit_by_horizon.md),
[`fit_nb()`](https://baselinenowcast.epinowcast.org/reference/fit_nb.md)

## Examples

``` r
# Use example data to create reporting triangle
data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data = data_as_of_df)
#> ℹ Using max_delay = 154 from data

# Create retrospective nowcasts
trunc_rts <- truncate_to_rows(rep_tri, n = 2)
retro_rts <- apply_reporting_structures(trunc_rts)
retro_nowcasts <- fill_triangles(retro_rts)

# Estimate dispersion parameters using default negative binomial model
disp_params <- estimate_uncertainty(
  point_nowcast_matrices = retro_nowcasts,
  truncated_reporting_triangles = trunc_rts,
  retro_reporting_triangles = retro_rts
)
disp_params
#>   [1]  11.3024846   6.5723896   2.5952630   2.7925662  37.2124366
#>   [6] 999.9999442  12.4500844 999.9999014   6.8669997   7.6539534
#>  [11]   1.0521862   0.4650880   2.7053717   0.5904918   1.7773856
#>  [16]   3.1155077   0.8435241   0.2312235   2.2117861   0.2150130
#>  [21]   0.2651624   0.8098215   1.7313731 999.9998982   1.1099185
#>  [26]   0.2699157   0.1253986   0.1192670   0.1000784   0.8390552
#>  [31]   0.1000784   0.1000784   0.2373649   0.1045634   0.1000784
#>  [36]   0.2949140   0.1000784   0.1000784   0.1000784   0.2251535
#>  [41]   0.1275446   0.1000784   0.1388573   0.1000784   0.1000784
#>  [46]   0.1000784 999.9998975   1.1995052   0.1000784   0.1000784
#>  [51]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [56]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [61]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [66]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [71]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [76]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [81]   0.1000784   0.1000784   0.1000784   0.1000784 999.9999287
#>  [86]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [91]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [96]   0.1000784   0.1000784   0.1000784 999.9999287 999.9999287
#> [101]   0.1000784   0.1000784   0.1000784   0.1000784 999.9999287
#> [106]   0.1000784   0.1000784   0.1000784 999.9999287 999.9999287
#> [111] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [116] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [121] 999.9999287 999.9999287   0.1000784   0.1000784 999.9999287
#> [126] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [131] 999.9999287 999.9999287   0.1000784   0.1000784   0.1000784
#> [136]   0.1000784 999.9999287 999.9999287 999.9999287 999.9999287
#> [141] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [146] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [151] 999.9999287 999.9999287 999.9999287 999.9999287

# Estimate dispersion parameters from rolling sum on the reference times
if (requireNamespace("zoo", quietly = TRUE)) {
  disp_params_agg <- estimate_uncertainty(
    point_nowcast_matrices = retro_nowcasts,
    truncated_reporting_triangles = trunc_rts,
    retro_reporting_triangles = retro_rts,
    ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
  )
  disp_params_agg
}
#>   [1]  67.2814366  46.1937528   6.5234536  14.6418994 999.9999091
#>   [6] 999.9999063 999.9999064 999.9999042 133.1830143  85.2348341
#>  [11]   4.3347357   2.6104948   4.9707196   1.1615881  40.5117886
#>  [16]   8.3077858   2.4123936   8.1272501   2.3392506   3.1127881
#>  [21]   6.8129843   2.0275206   1.4125405 999.9999393   1.0159982
#>  [26]   0.8015210   0.7529198   0.6777585   0.3368692  19.0399805
#>  [31]   0.3067500   0.1000784   0.4453096   0.1012936   0.5085173
#>  [36]   1.3786030   0.1000784   0.1000784   0.1000784   0.1882146
#>  [41]   0.8623161   0.2242091   0.6499149   0.1000784   0.7170622
#>  [46]   0.1000784 999.9998995   0.4818577   0.1000784   0.1000784
#>  [51]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [56]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [61]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [66]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [71]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [76]   0.1000784   0.1000784 999.9998993   0.1000784   0.1000784
#>  [81]   0.1000784   0.1000784   0.1000784   0.1000784 999.9999287
#>  [86]   0.1000784 999.9998997   0.1000784   0.1000784   0.1000784
#>  [91]   0.1000784   0.1000784   0.1000784   0.1000784   0.1000784
#>  [96]   0.1000784   0.1000784   0.1000784 999.9999287 999.9999287
#> [101]   0.1000784   0.1000784   0.1000784   0.1000784 999.9999287
#> [106]   0.1000784   0.1000784   0.1000784 999.9999287 999.9999287
#> [111] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [116] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [121] 999.9999287 999.9999287   0.1000784   0.1000784 999.9999287
#> [126] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [131] 999.9999287 999.9999287   0.1000784   0.1000784   0.1000784
#> [136]   0.1000784 999.9999287 999.9999287 999.9999287 999.9999287
#> [141] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [146] 999.9999287 999.9999287 999.9999287 999.9999287 999.9999287
#> [151] 999.9999287 999.9999287 999.9999287 999.9999287
```
