# Calculate the number of retrospective nowcast times that can be used after aggregating

Calculate the number of retrospective nowcast times that can be used
after aggregating

## Usage

``` r
.calc_n_retro_nowcast_times(
  list_of_obs,
  n_possible_horizons,
  ref_time_aggregator = identity
)
```

## Arguments

- list_of_obs:

  List of matrices of truncated reporting triangles

- n_possible_horizons:

  Integer indicating the number of horizons in the retrospective
  reporting triangle.

- ref_time_aggregator:

  Function that operates along the rows (reference times) of the
  retrospective point nowcast matrix before it has been aggregated
  across columns (delays). Default is `identity` which does not
  aggregate across reference times.

## Value

`n_iters` Integer indicating the number of iterations, or number of
retrospective nowcast times, that have sufficient data once aggregated
to be used to generate a retrospective point nowcast.
