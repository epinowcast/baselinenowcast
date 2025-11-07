# Preprocess negative values in the reporting triangle

Takes in a reporting triangle and returns a matrix in the same format as
the input triangle, but with negative values of reporting handled by
redistributing them to earlier delays (from longer delay to shorter).
This is useful when dealing with reporting corrections that can result
in negative incremental counts.

When negative values are detected, they are set to zero and the negative
amount is subtracted from the count at the next earlier delay (moving
from right to left in each row). This process continues until either the
negative value is fully absorbed or the first delay is reached.

This code was adapted from code written (under an MIT license) by the
Karlsruhe Institute of Technology RESPINOW German Hospitalization
Nowcasting Hub. Modified from
https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R
\#nolint

## Usage

``` r
preprocess_negative_values(triangle)
```

## Arguments

- triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays.

## Value

Matrix of positive integers with negative values of reporting handled
via redistribution to earlier delays.

## Details

Use this function when:

- Your data contains reporting corrections that result in negative
  counts

- You want to preserve the total count while handling negatives

- You need a delay distribution that sums to 1 or a CDF that is weakly
  increasing

Set `preprocess = NULL` in
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
when:

- Your data naturally has negative PMF entries (e.g., from differencing)

- You want to preserve the original structure including negatives

- You are working with corrections that should be reflected as negative
  probabilities

## See also

Delay distribution estimation functions
[`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)

## Examples

``` r
# Triangle with negative values from corrections
triangle_with_neg <- matrix(c(
  10, 5, -2, 3,
  8, -3, 4, 2,
  1, 6, 3, -1
), nrow = 3, byrow = TRUE)

# Preprocess to handle negatives
preprocessed <- preprocess_negative_values(triangle_with_neg)
#> ℹ Negative values detected in reporting triangle and will be corrected
preprocessed
#>      [,1] [,2] [,3] [,4]
#> [1,]   10    3    0    3
#> [2,]    5    0    4    2
#> [3,]    1    6    2    0
```
