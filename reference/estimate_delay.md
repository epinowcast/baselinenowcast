# Estimate a delay distribution from a reporting triangle

Provides an estimate of the reporting delay as a function of the delay,
based on the reporting triangle and the number of reference date
observations to be used in the estimation. This point estimate of the
delay is computed empirically, using an iterative algorithm starting
from the most recent observations. Use
[`truncate_to_delay()`](https://baselinenowcast.epinowcast.org/reference/truncate_to_delay.md)
if you want to limit the maximum delay before estimation. This code was
adapted from code written (under an MIT license) by the Karlsruhe
Institute of Technology RESPINOW German Hospitalization Nowcasting Hub.
Modified from:
https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55
\#nolint

## Usage

``` r
estimate_delay(
  reporting_triangle,
  n = nrow(reporting_triangle),
  validate = TRUE
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

- n:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay. The default is to use the whole reporting
  triangle, so `nrow(reporting_triangle)`.

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

Vector indexed at 0 of length `ncol(reporting_triangle)` with columns
indicating the point estimate of the empirical probability mass on each
delay.

## See also

Delay distribution estimation functions
[`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md)

## Examples

``` r
# Example 1: Standard usage
delay_pmf <- estimate_delay(
  reporting_triangle = example_reporting_triangle
)
delay_pmf
#>          0          1          2          3 
#> 0.51499404 0.28090584 0.14457631 0.05952381 

# Example 2: Using data with downward corrections (negatives preserved)
# Low-level functions process triangles as-is without preprocessing
delay_pmf_negative <- estimate_delay(
  reporting_triangle = example_downward_corr_rt,
  n = 5
)
delay_pmf_negative
#>           0           1           2           3 
#>  0.66544955  0.38806216 -0.14046823  0.08695652 

# Example 3: Preprocess explicitly before estimation if needed
preprocessed_triangle <- preprocess_negative_values(example_downward_corr_rt)
#> ℹ Negative values detected in reporting triangle and will be corrected
delay_pmf_preprocessed <- estimate_delay(
  reporting_triangle = preprocessed_triangle,
  n = 5
)
delay_pmf_preprocessed
#>          0          1          2          3 
#> 0.64346536 0.26957812 0.00000000 0.08695652 
```
