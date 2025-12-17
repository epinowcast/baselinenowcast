# Estimate uncertainty parameters using retrospective nowcasts

Estimates uncertainty parameters for nowcasting by creating a series of
retrospective datasets from the input reporting triangle, generating
point nowcasts for those datasets, and calibrating uncertainty
parameters based on retrospective nowcast performance.

This function chains the retrospective nowcasting workflow:

1.  [`truncate_to_rows()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_rows.md) -
    Create retrospective snapshots

2.  [`apply_reporting_structures()`](https://baselinenowcast.epinowcast.org/reference/apply_reporting_structures.md) -
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
  structure = get_reporting_structure(reporting_triangle),
  delay_pmf = NULL,
  validate = TRUE,
  ...
)
```

## Arguments

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

- delay_pmf:

  Vector or list of vectors of delays assumed to be indexed starting at
  the first delay column in each of the matrices in
  `retro_reporting_triangles`. If a list, must of the same length as
  `retro_reporting_triangles`, with elements aligning. Default is `NULL`

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

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
# Create a reporting triangle from syn_nssp_df
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data_as_of) |>
  truncate_to_delay(max_delay = 25)
#> ℹ Using max_delay = 154 from data
#> ℹ Truncating from max_delay = 154 to 25.

uncertainty_params <- estimate_uncertainty_retro(
  rep_tri,
  n_history_delay = 30,
  n_retrospective_nowcasts = 10
)
uncertainty_params
#>  [1] 15.5332295 10.0728853  4.0528122  4.4307525  5.2296859  4.5942518
#>  [7]  5.4795967  5.3292982  2.6502624  2.2989398  2.0225177  1.6874664
#> [13]  1.4401393  1.4909153  2.1489536  2.6053864  3.6601337  5.1271216
#> [19]  7.0165474  2.1741529  4.5010526  3.6969221  1.1146483  0.4782235
#> [25]  0.3418897
```
