# Allocate training volume based on combination of defaults and user-specified values for training volume for delay and uncertainty estimation.

Given the reporting triangle and optionally the user-specified scale
factor on the max delay to be used as total reference times and the
proportion of those reference times to be used for delay estimation,
allocate reference times to the number used for delay estimation and the
number used as retrospective nowcasts for uncertainty estimation.

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
  scale_factor = 3,
  prop_delay = 0.5,
  n_min_retro_nowcasts = 2,
  validate = TRUE
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

- validate:

  Logical. If TRUE (default), validates the object. Set to FALSE only
  when called from functions that already validated.

## Value

list of n_history_delay and n_retrospective_nowcasts

## See also

High-level workflow wrapper functions
[`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md),
[`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md),
[`estimate_uncertainty_retro()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty_retro.md)

## Examples

``` r
# Create a reporting triangle from package data
data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
rep_tri <- as_reporting_triangle(data_as_of) |>
  truncate_to_delay(max_delay = 25)
#> ℹ Using max_delay = 154 from data
#> ℹ Truncating from max_delay = 154 to 25.

# Use the defaults (scale_factor = 3, prop_delay = 0.5)
ref_time_allocation_default <- allocate_reference_times(rep_tri)
#> ℹ 0.5 reference times were specified for delay estimation but 0.493 of reference times used for delay estimation.
#> ℹ `prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.
ref_time_allocation_default
#> $n_history_delay
#> [1] 37
#> 
#> $n_retrospective_nowcasts
#> [1] 38
#> 

# Modify to use less volume and redistribute
ref_time_allocation_alt <- allocate_reference_times(
  reporting_triangle = rep_tri,
  scale_factor = 2,
  prop_delay = 0.6
)
ref_time_allocation_alt
#> $n_history_delay
#> [1] 30
#> 
#> $n_retrospective_nowcasts
#> [1] 20
#> 
```
