# Package index

## All functions

- [`allocate_reference_times()`](https://baselinenowcast.epinowcast.org/reference/allocate_reference_times.md)
  : Allocate training volume based on combination of defaults and
  user-specified values for training volume for delay and uncertainty
  estimation.

- [`apply_delay()`](https://baselinenowcast.epinowcast.org/reference/apply_delay.md)
  : Apply the delay to generate a point nowcast

- [`as_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.md)
  :

  Create a `reporting_triangle` object

- [`as_reporting_triangle(`*`<data.frame>`*`)`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.data.frame.md)
  :

  Create a `reporting_triangle` object from a data.frame

- [`as_reporting_triangle(`*`<matrix>`*`)`](https://baselinenowcast.epinowcast.org/reference/as_reporting_triangle.matrix.md)
  :

  Create a `reporting_triangle` from a matrix

- [`assert_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/assert_baselinenowcast_df.md)
  :

  Assert validity of `baselinenowcast_df` objects

- [`assert_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/assert_reporting_triangle.md)
  :

  Assert validity of `reporting_triangle` objects

- [`baselinenowcast()`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.md)
  : Generate a nowcast

- [`baselinenowcast(`*`<reporting_triangle>`*`)`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast.reporting_triangle.md)
  : Create a dataframe of nowcast results from a single reporting
  triangle

- [`baselinenowcast_df-class`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  [`baselinenowcast_df`](https://baselinenowcast.epinowcast.org/reference/baselinenowcast_df-class.md)
  : Nowcast Data.frame Object

- [`combine_obs_with_pred()`](https://baselinenowcast.epinowcast.org/reference/combine_obs_with_pred.md)
  : Combine observed data with a single prediction draw

- [`construct_triangle()`](https://baselinenowcast.epinowcast.org/reference/construct_triangle.md)
  : Generate a single retrospective reporting triangle

- [`construct_triangles()`](https://baselinenowcast.epinowcast.org/reference/construct_triangles.md)
  : Generate retrospective reporting triangles

- [`detect_structure()`](https://baselinenowcast.epinowcast.org/reference/detect_structure.md)
  : Detect the structure of a reporting triangle

- [`.apply_mask()`](https://baselinenowcast.epinowcast.org/reference/dot-apply_mask.md)
  : Apply mask to extract the elements of the matrix that are both true

- [`.assign_allocation_from_ns()`](https://baselinenowcast.epinowcast.org/reference/dot-assign_allocation_from_ns.md)
  : Assign number of reference times to delay and uncertainty from the
  sizes

- [`.calc_n_retro_nowcast_times()`](https://baselinenowcast.epinowcast.org/reference/dot-calc_n_retro_nowcast_times.md)
  : Calculate the number of retrospective nowcast times that can be used
  after aggregating

- [`.calculate_ns()`](https://baselinenowcast.epinowcast.org/reference/dot-calculate_ns.md)
  : Helper function to calculate various size requirements

- [`.check_against_requirements()`](https://baselinenowcast.epinowcast.org/reference/dot-check_against_requirements.md)
  : Check target size against number of reference times available and
  the number required

- [`.check_lhs_not_only_zeros()`](https://baselinenowcast.epinowcast.org/reference/dot-check_lhs_not_only_zeros.md)
  : Check if there are non-zero-values on the LHS of NAs

- [`.check_obs_and_pred()`](https://baselinenowcast.epinowcast.org/reference/dot-check_obs_and_pred.md)
  : Check observations and predictions are compatible

- [`.check_to_filter_to_max_delay()`](https://baselinenowcast.epinowcast.org/reference/dot-check_to_filter_to_max_delay.md)
  : Check that the reporting triangle contains the correct number of
  columns for the specified maximum delay

- [`.filter_to_recent_horizons()`](https://baselinenowcast.epinowcast.org/reference/dot-filter_to_recent_horizons.md)
  : Filter to recent horizons

- [`.handle_target_exceeds_avail()`](https://baselinenowcast.epinowcast.org/reference/dot-handle_target_exceeds_avail.md)
  : Helper for when target exceeds available reference times

- [`.perform_allocation_process()`](https://baselinenowcast.epinowcast.org/reference/dot-perform_allocation_process.md)
  : Perform the allocation process

- [`.safelydoesit()`](https://baselinenowcast.epinowcast.org/reference/dot-safelydoesit.md)
  : Safe iterator

- [`.validate_delay()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_delay.md)
  : Validate the delay PMF if it is passed in

- [`.validate_inputs_allocation()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_inputs_allocation.md)
  : Helper function to validate allocation parameters

- [`.validate_inputs_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_inputs_uncertainty.md)
  : Validate the specified number of reference times meets the minimum
  requirements

- [`.validate_max_delay()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_max_delay.md)
  : Check that the maximum delay is not too large, error if it is

- [`.validate_rep_tri_args()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_rep_tri_args.md)
  : Validate each item in the reporting triangle

- [`.validate_rep_tri_df()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_rep_tri_df.md)
  : Validate the reporting triangle data.frame

- [`.validate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/dot-validate_uncertainty.md)
  : Validate the uncertainty parameters if they are passed in

- [`estimate_and_apply_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_delay.md)
  : Estimate and apply delay from a reporting triangle

- [`estimate_and_apply_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_and_apply_uncertainty.md)
  : Estimate and apply uncertainty to a point nowcast matrix

- [`estimate_delay()`](https://baselinenowcast.epinowcast.org/reference/estimate_delay.md)
  : Estimate a delay distribution from a reporting triangle

- [`estimate_uncertainty()`](https://baselinenowcast.epinowcast.org/reference/estimate_uncertainty.md)
  : Estimate uncertainty parameters

- [`fill_triangle()`](https://baselinenowcast.epinowcast.org/reference/fill_triangle.md)
  : Generate point nowcast

- [`fill_triangles()`](https://baselinenowcast.epinowcast.org/reference/fill_triangles.md)
  : Generate retrospective nowcasts

- [`fit_by_horizon()`](https://baselinenowcast.epinowcast.org/reference/fit_by_horizon.md)
  : Helper function that fits its each column of the matrix (horizon) to
  an observation model.

- [`fit_nb()`](https://baselinenowcast.epinowcast.org/reference/fit_nb.md)
  : Fit a negative binomial to a vector of observations and expectations

- [`new_baselinenowcast_df()`](https://baselinenowcast.epinowcast.org/reference/new_baselinenowcast_df.md)
  : Combine data from a nowcast dataframe, strata, and reference dates

- [`new_reporting_triangle()`](https://baselinenowcast.epinowcast.org/reference/new_reporting_triangle.md)
  :

  Class constructor for `reporting_triangle` objects

- [`reporting_triangle-class`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  [`reporting_triangle`](https://baselinenowcast.epinowcast.org/reference/reporting_triangle-class.md)
  : Reporting Triangle Object

- [`sample_nb()`](https://baselinenowcast.epinowcast.org/reference/sample_nb.md)
  : Sample from negative binomial model given a set of predictions

- [`sample_nowcast()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcast.md)
  : Generate a single draw of a nowcast combining observed and predicted
  values

- [`sample_nowcasts()`](https://baselinenowcast.epinowcast.org/reference/sample_nowcasts.md)
  : Generate multiple draws of a nowcast combining observed and
  predicted values

- [`sample_prediction()`](https://baselinenowcast.epinowcast.org/reference/sample_prediction.md)
  : Get a draw of only the predicted elements of the nowcast vector

- [`sample_predictions()`](https://baselinenowcast.epinowcast.org/reference/sample_predictions.md)
  : Get a dataframe of multiple draws of only the predicted elements of
  the nowcast vector

- [`syn_nssp_df`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_df.md)
  : A synthetic dataset containing the number of incident cases indexed
  by reference date and report date. While data of this form could be
  from any source, this data is meant to represent the output of
  pre-processing the syn_nssp_line_list dataset, which is a synthetic
  patient-level line list data from the United State's National
  Syndromic Surveillance System (NSSP).

- [`syn_nssp_line_list`](https://baselinenowcast.epinowcast.org/reference/syn_nssp_line_list.md)
  : A synthetic dataset resembling line-list (each row is a patient)
  data from the United States' National Syndromic Surveillance System
  (NSSP) accessed via the Essence platform. All entries are synthetic,
  formatted to look as close to the real raw data as possible.

- [`truncate_triangle()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangle.md)
  : Get a single truncated triangle

- [`truncate_triangles()`](https://baselinenowcast.epinowcast.org/reference/truncate_triangles.md)
  : Generate truncated reporting triangles
