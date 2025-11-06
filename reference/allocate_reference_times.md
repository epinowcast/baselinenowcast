# Allocate training volume based on combination of defaults and user-specified values for training volume for delay and uncertainty estimation.

Given the reporting triangle, the maximum delay, and optionally the
user-specified scale factor on the max delay to be used as total
reference times and the proportion of those reference times to be used
for delay estimation, allocate reference times to the number used for
delay estimation and the number used as retrospective nowcasts for
uncertainty estimation.

This function implements an algorithm which:

- if the specified number of reference times (`scale_factor` x
  `max delay`) is less than or equal to the number of reference times
  available in the reporting triangle, split reference times between
  delay and uncertainty according to `prop_delay`, ensuring that the
  minimum requirements for delay and uncertainty estimation are met.

- if the specified number of reference times is greater than the number
  of reference times available in the reporting triangle, use all the
  reference times available and satisfy the minimum requirement for
  delay estimation and then split the remainder according to the
  specified `prop_delay`, ensuring that the minimum reference times for
  delay and uncertainty estimation are fulfilled.

- the function errors if the minimum requirements for delay and
  uncertainty estimation are not possible from the number of reference
  times in the reporting triangle.

## Usage

``` r
allocate_reference_times(
  reporting_triangle,
  max_delay = ncol(reporting_triangle) - 1,
  scale_factor = 3,
  prop_delay = 0.5,
  n_min_retro_nowcasts = 2
)
```

## Arguments

- reporting_triangle:

  Matrix of the reporting triangle, with rows representing the time
  points of reference and columns representing the delays. Can be a
  reporting matrix or incomplete reporting matrix. Can also be a ragged
  reporting triangle, where multiple columns are reported for the same
  row. (e.g. weekly reporting of daily data).

- max_delay:

  Integer indicating the maximum delay to estimate, in units of the
  delay. The default is to use the whole reporting triangle,
  `ncol(reporting_triangle) -1`.

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

list of n_history_delay and n_retrospective_nowcasts

## See also

High-level workflow wrapper functions
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
triangle <- matrix(
  c(
    100, 50, 30, 20,
    40, 10, 20, 5,
    80, 50, 25, 10,
    100, 50, 30, 20,
    40, 10, 20, 5,
    80, 50, 25, 10,
    100, 50, 30, 20,
    90, 45, 25, NA,
    80, 40, NA, NA,
    70, NA, NA, NA
  ),
  nrow = 10,
  byrow = TRUE
)
# Use the defaults
ref_time_allocation_default <- allocate_reference_times(triangle)
#> ℹ 0.5 reference times were specified for delay estimation but 0.444 of reference times used for delay estimation.
#> ℹ `prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.
ref_time_allocation_default
#> $n_history_delay
#> [1] 4
#> 
#> $n_retrospective_nowcasts
#> [1] 5
#> 
# Modify to use less volume and redistribute
ref_time_allocation_alt <- allocate_reference_times(
  reporting_triangle = triangle,
  scale_factor = 2,
  prop_delay = 0.6
)
#> ℹ 0.6 reference times were specified for delay estimation but 0.667 of reference times used for delay estimation.
#> ℹ This is due to the minumim requirement for the number of reference times needed for delay estimation (4).
ref_time_allocation_alt
#> $n_history_delay
#> [1] 4
#> 
#> $n_retrospective_nowcasts
#> [1] 2
#> 
```
