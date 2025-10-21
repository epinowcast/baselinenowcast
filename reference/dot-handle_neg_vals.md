# Handle negative values in the reporting triangle Takes in a reporting triangle and returns a matrix in the same format as the input triangle, but with negative values of reporting handled via passing them to the subsequent days (from longer delay to shorter). This code was adapted from code written (under an MIT license) by the Karlsruhe Institute of Technology RESPINOW German Hospitalization Nowcasting Hub. Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R \#nolint

Handle negative values in the reporting triangle Takes in a reporting
triangle and returns a matrix in the same format as the input triangle,
but with negative values of reporting handled via passing them to the
subsequent days (from longer delay to shorter). This code was adapted
from code written (under an MIT license) by the Karlsruhe Institute of
Technology RESPINOW German Hospitalization Nowcasting Hub. Modified from
https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R
\#nolint

## Usage

``` r
.handle_neg_vals(triangle)
```

## Arguments

- triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays.

## Value

Matrix of positive integers with negative values of reporting handled
via passing them to the subsequent days delay.
