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

## See also

Probabilistic nowcast generation functions
[`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md),
[`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md),
[`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md),
[`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md),
[`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)

## Examples

``` r
pred <- c(3.2, 4.6)
sampled_preds <- sample_nb(pred,
  uncertainty_params = c(50, 100)
)
sampled_preds
#> [1] 7 4
```
