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
  preprocess = preprocess_negative_values,
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

- preprocess:

  Function to apply to the truncated triangle before estimation, or NULL
  to skip preprocessing. Default is
  [`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md),
  which handles negative values by redistributing them to earlier
  delays. Set to NULL if you want to preserve negative PMF entries
  (e.g., when working with downward corrections where negative
  probabilities reflect systematic adjustments). Custom preprocess
  functions must accept a `validate` parameter (defaults to TRUE) to
  enable validation optimisation in internal function chains.

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
# Example 1: Standard usage with default preprocessing
delay_pmf <- estimate_delay(
  reporting_triangle = example_reporting_triangle
)
delay_pmf
#>          0          1          2          3 
#> 0.52654815 0.28277586 0.13006993 0.06060606 

# Example 2: Using data with downward corrections without preprocessing
# This preserves negative PMF entries reflecting systematic corrections
triangle_ex2 <- example_downward_corr_rt
delay_pmf_negative <- estimate_delay(
  reporting_triangle = triangle_ex2,
  n = 5,
  preprocess = NULL
)
delay_pmf_negative
#>           0           1           2           3 
#>  0.66544955  0.38806216 -0.14046823  0.08695652 
# Note: PMF may contain negative values and not sum to 1
sum(delay_pmf_negative)
#> [1] 1
```
