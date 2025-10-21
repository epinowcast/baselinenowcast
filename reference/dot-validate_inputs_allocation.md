# Helper function to validate allocation parameters

Helper function to validate allocation parameters

## Usage

``` r
.validate_inputs_allocation(scale_factor, prop_delay, n_min_retro_nowcasts)
```

## Arguments

- scale_factor:

  Numeric value indicating the multiplicative factor on the maximum
  delay to be used for estimation of delay and uncertainty. Default is
  `3`.

- prop_delay:

  Numeric value \<1 indicating what proportion of all reference times in
  the reporting triangle to be used for delay estimation. Default is
  `0.5`.

- n_min_retro_nowcasts:

  Integer indicating the minimum number of reference times needed for
  uncertainty estimation. Default is `2`.

## Value

NULL invisibly
