# Sample from negative binomial model given a set of predictions

Sample from negative binomial model given a set of predictions

## Usage

``` r
sample_nb(pred, uncertainty_params)
```

## Arguments

- pred:

  Vector of predictions.

- uncertainty_params:

  Vector of uncertainty parameters.

## Value

`sampled_pred` Object of the same dimensions as `pred` representing a
single draw from the negative binomial distribution with the specified
`uncertainty params`.

## Examples

``` r
pred <- c(3.2, 4.6)
sampled_preds <- sample_nb(pred,
  uncertainty_params = c(50, 100)
)
sampled_preds
#> [1] 1 6
```
