# Assign number of reference times to delay and uncertainty from the sizes

Assign number of reference times to delay and uncertainty from the sizes

## Usage

``` r
.assign_allocation_from_ns(
  n_used,
  n_target,
  n_min_delay,
  n_min_retro_nowcasts,
  n_required,
  prop_delay
)
```

## Arguments

- n_used:

  Integer indicating number reference times that will be used.

- n_target:

  Integer indicating the target number of reference times.

- n_min_delay:

  Integer indicating the number needed for delay estimation.

- n_min_retro_nowcasts:

  Integer indicating the number needed for uncertainty estimation.

- n_required:

  Integer indicating the number need for both delay and uncertainty

- prop_delay:

  Numeric value \<1 indicating what proportion of all reference times in
  the reporting triangle to be used for delay estimation.

## Value

List of number of reference times to use for delay and uncertainty
