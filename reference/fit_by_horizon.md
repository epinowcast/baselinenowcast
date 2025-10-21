# Helper function that fits its each column of the matrix (horizon) to an observation model.

Helper function that fits its each column of the matrix (horizon) to an
observation model.

## Usage

``` r
fit_by_horizon(obs, pred, fit_model = fit_nb)
```

## Arguments

- obs:

  Matrix or vector of observations.

- pred:

  Matrix or vector of predictions.

- fit_model:

  Function that ingests observations and expectations and returns
  uncertainty parameters, default is `fit_nb`.

## Value

Vector of uncertainty parameters of the same length as the number of
columns in the `obs` matrix.

## Examples

``` r
obs <- matrix(
  c(
    5, 6, 2,
    1, 4, 2,
    8, 4, 2
  ),
  nrow = 3,
  byrow = TRUE
)
pred <- matrix(
  c(
    4.2, 5.2, 1.8,
    0.7, 3.5, 3.4,
    7.3, 4.1, 1.2
  ),
  nrow = 3,
  byrow = TRUE
)
disp <- fit_by_horizon(obs = obs, pred = pred)
disp
#> [1] 999.9999 999.9999 999.9999
```
