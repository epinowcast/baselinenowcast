# Estimate a delay distribution from a reporting triangle

Provides an estimate of the reporting delay as a function of the delay,
based on the reporting triangle and the specified maximum delay and
number of reference date observations to be used in the estimation. This
point estimate of the delay is computed empirically, using an iterative
algorithm starting from the most recent observations. This code was
adapted from code written (under an MIT license) by the Karlsruhe
Institute of Technology RESPINOW German Hospitalization Nowcasting Hub.
Modified from:
https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55
\#nolint

## Usage

``` r
estimate_delay(
  reporting_triangle,
  max_delay = ncol(reporting_triangle) - 1,
  n = nrow(reporting_triangle),
  preprocess = preprocess_negative_values
)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

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
  probabilities reflect systematic adjustments).

## Value

Vector indexed at 0 of length `max_delay + 1` with columns indicating
the point estimate of the empirical probability mass on each delay.

## See also

Delay distribution estimation functions
[`preprocess_negative_values()`](https://baselinenowcast.epinowcast.org/reference/preprocess_negative_values.md)

## Examples

``` r
# Example 1: Standard usage with default preprocessing
triangle <- matrix(
  c(
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 5,
  byrow = TRUE
)
delay_pmf <- estimate_delay(
  reporting_triangle = triangle,
  max_delay = 3,
  n = 4
)
delay_pmf
#> [1] 0.5029412 0.2514706 0.1455882 0.1000000

# Example 2: Using data with downward corrections without preprocessing
# This preserves negative PMF entries reflecting systematic corrections
delay_pmf_negative <- estimate_delay(
  reporting_triangle = example_downward_corr_mat,
  max_delay = 3,
  n = 5,
  preprocess = NULL
)
delay_pmf_negative
#> [1]  0.66544955  0.38806216 -0.14046823  0.08695652
# Note: PMF may contain negative values and not sum to 1
sum(delay_pmf_negative)
#> [1] 1
```
