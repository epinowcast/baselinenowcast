# Fit a negative binomial to a vector of observations and expectations

Takes in a vector of observations and a vector of expectations and
performs a MLE estimator to estimate the dispersion parameter of a
negative binomial. This code was adapted from code written (under an MIT
license) by the Karlsruhe Institute of Technology RESPINOW German
Hospitalization Nowcasting Hub. Modified from:
https://github.com/KITmetricslab/RESPINOW-Hub/blob/7fab4dce7b559c3076ab643cf22048cb5fb84cc2/code/baseline/functions.R#L404
\#nolint

## Usage

``` r
fit_nb(x, mu)
```

## Arguments

- x:

  Vector of observed values.

- mu:

  Vector of expected values.

## Value

the maximum likelihood estimate of the dispersion

## Examples

``` r
obs <- c(4, 8, 10)
pred <- c(3.1, 7.2, 11)
disp <- fit_nb(obs, pred)
disp
#> [1] 999.9999
```
