# Check if matrix has valid NA pattern

Alternative pure-matrix implementation of the same NA-pattern check as
`.check_na_pattern()`. Kept for its tests, which exercise this algorithm
directly.

## Usage

``` r
.check_na_bottom_right(mat)
```

## Arguments

- mat:

  Matrix

## Value

Boolean indicating whether the matrix only contains NAs in the bottom
right (TRUE if only in bottom right, FALSE if elsewhere).
