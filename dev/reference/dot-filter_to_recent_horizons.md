# Filter to recent horizons

Filter to recent horizons

## Usage

``` r
.filter_to_recent_horizons(mat, n_possible_horizons)
```

## Arguments

- mat:

  Matrix containing all the rows

- n_possible_horizons:

  Number of rows we want starting from the final row

## Value

`bottom_matrix` Matrix containing the last `n_possible_horizons` rows of
the matrix.
