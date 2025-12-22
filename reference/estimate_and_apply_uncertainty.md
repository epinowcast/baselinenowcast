# Estimate and apply uncertainty to a point nowcast matrix

Generates probabilistic nowcasts by estimating uncertainty parameters
from retrospective nowcasts and applying them to a point nowcast matrix.

This function combines:

1.  [`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md) -
    Estimates uncertainty parameters using retrospective nowcasts

2.  [`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md) -
    Applies uncertainty to generate draws

To obtain estimates of uncertainty parameters, use
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md).
For full control over individual steps (e.g., custom matrix preparation,
alternative aggregation), use the low-level functions
([`truncate_to_rows()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_rows.md),
[`apply_reporting_structures()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structures.md),
[`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.md),
[`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md))
directly.

## Usage

``` r
estimate_and_apply_uncertainty(
  point_nowcast_matrix,
  reporting_triangle,
  n_history_delay,
  n_retrospective_nowcasts,
  structure = get_reporting_structure(reporting_triangle),
  draws = 1000,
  delay_pmf = NULL,
  uncertainty_model = fit_by_horizon,
  uncertainty_sampler = sample_nb,
  validate = TRUE,
  ...
)
```

## Arguments

- point_nowcast_matrix:

  Matrix of point nowcast predictions and observations, with rows
  representing the reference times and columns representing the delays.

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

- n_history_delay:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay.

- n_retrospective_nowcasts:

  Integer indicating the number of retrospective nowcast times to use
  for uncertainty estimation.

- structure:

  Integer or vector specifying the reporting structure. If integer,
  divides columns evenly by that integer (with last possibly truncated).
  If vector, the sum must not be greater than or equal to the number of
  columns. Default is 1 (standard triangular structure).

- draws:

  Integer indicating the number of draws of the predicted nowcast vector
  to generate. Default is `1000`.

- delay_pmf:

  Vector or list of vectors of delays assumed to be indexed starting at
  the first delay column in each of the matrices in
  `retro_reporting_triangles`. If a list, must be of the same length as
  `retro_reporting_triangles`, with elements aligning. Default is
  `NULL`.

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

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

- ...:

  Additional arguments to
  [`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md)
  and
  [`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md).

## Value

`nowcast_draws_df` Dataframe containing draws of combined observations
and probabilistic predictions at each reference time.

## See also

High-level workflow wrapper functions
[`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md),
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md),
[`estimate_and_apply_delays()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delays.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
# Use package data truncated to appropriate size
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
triangle <- as_reporting_triangle(data_as_of) |>
  truncate_to_delay(max_delay = 25)
#> ℹ Using max_delay = 154 from data
#> ℹ Truncating from max_delay = 154 to 25.

pt_nowcast_matrix <- estimate_and_apply_delay(
  reporting_triangle = triangle,
  n = 75
)
# Use 75 reference times for delay estimation and 40 for uncertainty
nowcast_draws_df <- estimate_and_apply_uncertainty(
  pt_nowcast_matrix,
  triangle,
  n_history_delay = 75,
  n_retrospective_nowcasts = 40,
  draws = 100
)
head(nowcast_draws_df)
#>   pred_count reference_date draw
#> 1        382     2025-10-25    1
#> 2        432     2025-10-26    1
#> 3        351     2025-10-27    1
#> 4        361     2025-10-28    1
#> 5        426     2025-10-29    1
#> 6        362     2025-10-30    1
```
