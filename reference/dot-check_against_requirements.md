# Check target size against number of reference times available and the number required

Check target size against number of reference times available and the
number required

## Usage

``` r
.check_against_requirements(
  n_ref_times,
  n_required,
  n_target,
  n_min_delay,
  n_min_retro_nowcasts,
  scale_factor,
  max_delay
)
```

## Arguments

- n_ref_times:

  Integer indicating the number of reference times available

- n_required:

  Integer indicating the number need for both delay and uncertainty

- n_target:

  Integer indicating the target number of reference times.

- n_min_delay:

  Integer indicating the number needed for delay estimation.

- n_min_retro_nowcasts:

  Integer indicating the number needed for uncertainty estimation.

- scale_factor:

  Numeric value indicating the multiplicative factor on the maximum
  delay to be used for estimation of delay and uncertainty. Default is
  `3`.

## Value

`n_used` Integer indicating how many reference times will be used
