# Validate the uncertainty parameters if they are passed in

Validate the uncertainty parameters if they are passed in

## Usage

``` r
.validate_uncertainty(triangle, uncertainty_params)
```

## Arguments

- triangle:

  Matrix of values with rows indicating the time points and columns
  indicating the delays.

- uncertainty_params:

  Vector of uncertainty parameters ordered from horizon 1 to the maximum
  horizon. Note that these will be reversed internally to match the
  ordering of the `point_nowcast_matrix` (where a horizon of 1 is the
  last entry).

## Value

NULL invisibly
