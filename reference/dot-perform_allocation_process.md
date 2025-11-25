# Perform the allocation process

Perform the allocation process

## Usage

``` r
.perform_allocation_process(
  reporting_triangle,
  max_delay,
  scale_factor,
  prop_delay,
  n_min_retro_nowcasts
)
```

## Arguments

- reporting_triangle:

  A
  [reporting_triangle](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  object with rows representing reference times and columns representing
  delays. Can be a reporting matrix or incomplete reporting matrix. Can
  also be a ragged reporting triangle, where multiple columns are
  reported for the same row (e.g., weekly reporting of daily data).

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

list of reference time allocations
