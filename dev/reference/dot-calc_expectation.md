# Calculate the updated rows of the expected nowcasted triangle

Calculate the updated rows of the expected nowcasted triangle

## Usage

``` r
.calc_expectation(index, expectation, delay_prob, delay_cdf_prev, n_rows)
```

## Arguments

- index:

  Integer indicating the delay index

- expectation:

  Matrix of the incomplete reporting matrix

- delay_prob:

  Scalar probability for the current delay

- delay_cdf_prev:

  Scalar cumulative probability up to previous delay

- n_rows:

  Number of rows in the expectation matrix

## Value

Matrix with another set of entries corresponding to the updated values
for the specified rows and column
