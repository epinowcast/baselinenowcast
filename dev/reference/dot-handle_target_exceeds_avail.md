# Helper for when target exceeds available reference times

Helper for when target exceeds available reference times

## Usage

``` r
.handle_target_exceeds_avail(
  n_ref_times,
  n_required,
  n_target,
  n_min_delay,
  n_min_retro_nowcasts
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

## Value

number of reference times to use or NULL, invisibly
