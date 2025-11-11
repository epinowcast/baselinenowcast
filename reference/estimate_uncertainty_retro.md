# Estimate uncertainty parameters using retrospective nowcasts

Estimates uncertainty parameters for nowcasting by creating a series of
retrospective datasets from the input reporting triangle, generating
point nowcasts for those datasets, and calibrating uncertainty
parameters based on retrospective nowcast performance.

This function chains the retrospective nowcasting workflow:

1.  [`truncate_triangles()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangles.md) -
    Create retrospective snapshots

2.  [`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md) -
    Generate retrospective reporting triangles

3.  [`fill_triangles()`](https://baselinenowcast.epinowcast.org/reference/fill_triangles.md) -
    Generate point nowcasts

4.  [`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md) -
    Estimate uncertainty parameters

For full probabilistic nowcasts (uncertainty estimation + sampling), use
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md).

For more control over individual steps (e.g., custom matrix preparation,
alternative aggregation), use the low-level functions directly.

## Usage

``` r
estimate_uncertainty_retro(
  reporting_triangle,
  n_history_delay,
  n_retrospective_nowcasts,
  max_delay = ncol(reporting_triangle) - 1,
  delay_pmf = NULL,
  preprocess = preprocess_negative_values,
  ...
)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- n_history_delay:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay.

- n_retrospective_nowcasts:

  Integer indicating the number of retrospective nowcast times to use
  for uncertainty estimation.

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

- delay_pmf:

  Vector or list of vectors of delays assumed to be indexed starting at
  the first delay column in each of the matrices in
  `retro_reporting_triangles`. If a list, must of the same length as
  `retro_reporting_triangles`, with elements aligning. Default is `NULL`

- preprocess:

  Function to apply to the truncated triangle before estimation, or NULL
  to skip preprocessing. Default is
  [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md),
  which handles negative values by redistributing them to earlier
  delays. Set to NULL if you want to preserve negative PMF entries
  (e.g., when working with downward corrections where negative
  probabilities reflect systematic adjustments).

- ...:

  Additional arguments passed to
  [`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md).

## Value

A numeric vector of uncertainty parameters with length equal to one less
than the number of columns in the reporting triangle, with each element
representing the estimate of the uncertainty parameter for each horizon.
Returns NULL if insufficient data is available for estimation.

## See also

High-level workflow wrapper functions
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md),
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)

## Examples

``` r
# Create example reporting triangle
triangle <- matrix(
  c(
    65, 46, 21, 7,
    70, 40, 20, 5,
    80, 50, 10, 10,
    100, 40, 31, 20,
    95, 45, 21, NA,
    82, 42, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 7,
  byrow = TRUE
)

# Estimate uncertainty parameters
uncertainty_params <- estimate_uncertainty_retro(
  triangle,
  n_history_delay = 5,
  n_retrospective_nowcasts = 2
)
uncertainty_params
#> [1] 999.999936   5.078299   3.034222

# Estimate with custom parameters
uncertainty_params_custom <- estimate_uncertainty_retro(
  triangle,
  n_history_delay = 4,
  n_retrospective_nowcasts = 2,
  max_delay = 3
)
uncertainty_params_custom
#> [1] 999.999908   2.985022   4.850320
```
