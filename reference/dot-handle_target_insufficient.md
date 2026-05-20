# Helper for when the target is less than the required minimum

Helper for when the target is less than the required minimum

## Usage

``` r
.handle_target_insufficient(
  n_target,
  n_required,
  n_min_delay,
  n_min_retro_nowcasts,
  scale_factor,
  max_delay
)
```

## Arguments

- n_target:

  Integer indicating the target number of reference times.

- n_required:

  Integer indicating the number need for both delay and uncertainty

- n_min_delay:

  Integer indicating the number needed for delay estimation.

- n_min_retro_nowcasts:

  Integer indicating the number needed for uncertainty estimation.

- scale_factor:

  Numeric value indicating the multiplicative factor on the maximum
  delay to be used for estimation of delay and uncertainty. Default is
  `3`.

- max_delay:

  Integer indicating the maximum delay in the reporting triangle, used
  together with `scale_factor` to derive the target number of reference
  times.

## Value

NULL invisibly
