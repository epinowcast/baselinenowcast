# Validate the specified number of reference times meets the minimum requirements

Validate the specified number of reference times meets the minimum
requirements

## Usage

``` r
.validate_inputs_uncertainty(
  n_ref_times,
  n_min_delay,
  n_history_delay,
  n_retrospective_nowcasts,
  n_min_retro_nowcasts = 2
)
```

## Arguments

- n_ref_times:

  Integer indicating the number of reference times available.

- n_min_delay:

  Integer indicating minimum number of reference times needed for delay
  estimation.

- n_history_delay:

  Integer indicating the number of reference times (observations) to be
  used in the estimate of the reporting delay, always starting from the
  most recent reporting delay.

- n_retrospective_nowcasts:

  Integer indicating the number of retrospective nowcast times to use
  for uncertainty estimation.

- n_min_retro_nowcasts:

  Integer indicating the minimum number of reference times needed for
  uncertainty estimation. Default is `2`.

## Value

NULL, invisibly
