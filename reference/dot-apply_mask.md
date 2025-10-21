# Apply mask to extract the elements of the matrix that are both true

Apply mask to extract the elements of the matrix that are both true

## Usage

``` r
.apply_mask(mat, indices_1, indices_2)
```

## Arguments

- mat:

  Matrix containing elements for extraction.

- indices_1:

  Matrix of booleans of the same dimensions of `mat`.

- indices_2:

  Matrix of booleans of the same dimensions of `mat`

## Value

Matrix of same dimensions of `mat` with the overlapping `TRUE` elements
only.
