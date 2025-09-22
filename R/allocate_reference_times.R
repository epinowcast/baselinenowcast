#' Allocate training volume based on combination of defaults and user-specified
#'   values for training volume for delay and uncertainty estimation.
#' @description Given the reporting triangle, the maximum delay, and
#'    optionally the user-specified scale factor on the max delay to be used as
#'    total reference times and the proportion of those reference times to
#'    be used for delay estimation, allocate reference times to the number
#'    used for delay estimation and the number used as retrospective nowcasts
#'    for uncertainty estimation.
#'
#'    This function implements an algorithm which:
#'    - by default uses 3 x the maximum delay, split evenly between delay and
#'    uncertainty estimation if there are sufficient reference times in the
#'    reporting triangle for this.
#'    - if the specified number of reference times
#'    (`scale_factor` x `max delay`) is greater than the number of reference
#'    times available in the reporting triangle, use all the reference times
#'    available and satisfy the minimum
#'    requirement for delay estimation and then split the remainder according to
#'    the specified `prop_delay`, ensuring that the minimum reference times
#'    for delay and uncertainty estimation are fulfilled.
#'    - the function errors if the minimum requirements for delay and
#'    uncertainty estimation are not possible from the number of reference times
#'    in the reporting triangle.
#'
#' @inheritParams estimate_delay
#' @param scale_factor Numeric value indicating the multiplicative factor on
#'     the maximum delay to be used for estimation of delay and uncertainty.
#'      Default is `3`.
#' @param prop_delay Numeric value <1 indicating what proportion of all
#'     reference times in the reporting triangle to be used for delay
#'     estimation. Default is `0.5`.
#' @param n_min_retro_nowcasts Integer indicating the minimum number of
#'     reference times needed for uncertainty estimation. Default is `2`.
#' @importFrom cli cli_abort cli_warn
#' @importFrom checkmate assert_integerish
#' @export
#' @returns list of n_history_delay and n_retrospective_nowcasts
#' @examples
#' triangle <- matrix(
#'   c(
#'     100, 50, 30, 20,
#'     40, 10, 20, 5,
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     40, 10, 20, 5,
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 10,
#'   byrow = TRUE
#' )
#' # Use the defaults
#' ref_time_allocation_default <- allocate_reference_times(triangle)
#' ref_time_allocation_default
#' # Modify to use less volume and redistribute
#' ref_time_allocation_alt <- allocate_reference_times(
#'   reporting_triangle = triangle,
#'   scale_factor = 2,
#'   prop_delay = 0.6
#' )
#' ref_time_allocation_alt
allocate_reference_times <- function(reporting_triangle,
                                     max_delay = ncol(reporting_triangle) - 1,
                                     scale_factor = 3,
                                     prop_delay = 0.5,
                                     n_min_retro_nowcasts = 2) {
  # Checks of inputs
  .validate_triangle(reporting_triangle, max_delay)
  .validate_allocation_params(
    scale_factor, prop_delay,
    n_min_retro_nowcasts
  )

  ns <- .perform_allocation_process(
    reporting_triangle,
    max_delay,
    scale_factor,
    prop_delay,
    n_min_retro_nowcasts
  )

  return(ns)
}

#' Perform the allocation process
#'
#' @inheritParams allocate_reference_times
#'
#' @return list of reference time allocations
.perform_allocation_process <- function(reporting_triangle,
                                        max_delay,
                                        scale_factor,
                                        prop_delay,
                                        n_min_retro_nowcasts) {
  sizes <- .calculate_ns(
    reporting_triangle,
    max_delay,
    scale_factor,
    n_min_retro_nowcasts
  )
  n_ref_times <- sizes$ref_times
  n_required <- sizes$required
  n_min_delay <- sizes$min_delay
  n_target <- sizes$target

  # Check specifications against targets and requirements
  n_used <- .check_against_requirements(
    n_ref_times,
    n_required,
    n_target,
    n_min_delay,
    n_min_retro_nowcasts,
    scale_factor,
    max_delay
  )

  # If size being used equals or exceeds the target, simply split according to prop delay #nolint
  ns <- .assign_allocation_from_ns(
    n_used = n_used,
    n_target = n_target,
    n_min_delay = n_min_delay,
    n_min_retro_nowcasts = n_min_retro_nowcasts,
    n_required = n_required,
    prop_delay = prop_delay
  )

  n_retrospective_nowcasts <- ns$n_retrospective_nowcasts
  n_history_delay <- ns$n_history_delay

  # Handle proportion delay warning and messages
  .handle_output_msgs(
    n_history_delay,
    n_retrospective_nowcasts,
    n_used,
    prop_delay
  )

  return(list(
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts
  ))
}

#' Handle output messages after allocation has occurred
#'
#' @inheritParams allocate_reference_times
#' @param prop_delay Numeric value <1 indicating what proportion of all
#'   reference times in the reporting triangle to be used for delay
#'  estimation.
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams estimate_and_apply_uncertainty
#' @importFrom cli cli_alert_info cli_warn
#' @return NULL, invisibly
.handle_output_msgs <- function(n_history_delay,
                                n_retrospective_nowcasts,
                                n_used,
                                prop_delay) {
  prop_delay_used <- n_history_delay / n_used

  if (prop_delay_used != prop_delay) {
    cli_warn(
      message = c(
        "`prop_delay` specified is not equivalent to `prop_delay` used.",
        "i" = "{prop_delay} reference times were specified for delay estimation but due to minimum requirements had to be reallocated.", # nolint
        "x" = "{round(prop_delay_used,3)} of reference times used for delay estimation." # nolint
      )
    )
  }

  cli_alert_info(text = "Using {n_history_delay} reference times for delay estimation.") # nolint
  cli_alert_info(text = "Using {n_retrospective_nowcasts} reference times as retrospective nowcast times for uncertainty estimation.") # nolint
  return(NULL)
}

#' Helper function to validate allocation parameters
#'
#'
#' @inheritParams allocate_reference_times
#' @importFrom checkmate assert_scalar assert_numeric assert_integerish
#'
#' @returns NULL invisibly
.validate_allocation_params <- function(scale_factor,
                                        prop_delay,
                                        n_min_retro_nowcasts) {
  assert_integerish(n_min_retro_nowcasts, lower = 0)
  assert_scalar(prop_delay)
  assert_numeric(prop_delay, lower = 0, upper = 1)
  assert_scalar(scale_factor)
  assert_numeric(scale_factor, lower = 0)
  return(NULL)
}

#' Helper function to calculate various size requirements
#' @inheritParams allocate_reference_times
#'
#' @returns list of the integer sizes
.calculate_ns <- function(reporting_triangle,
                          max_delay,
                          scale_factor,
                          n_min_retro_nowcasts) {
  ref_times <- nrow(reporting_triangle)
  min_delay <- sum(is.na(rowSums(reporting_triangle))) + 1
  required <- min_delay + n_min_retro_nowcasts
  target <- round(scale_factor * max_delay, 0)

  sizes <- list(
    ref_times = ref_times,
    min_delay = min_delay,
    required = required,
    target = target
  )
  return(sizes)
}

#' Check target size against number of reference times available and the number
#'   required
#'
#' @param n_ref_times Integer indicating the number of reference times
#'    available
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns `n_used` Integer indicating how many reference times will be
#'    used
.check_against_requirements <- function(n_ref_times,
                                        n_required,
                                        n_target,
                                        n_min_delay,
                                        n_min_retro_nowcasts,
                                        scale_factor,
                                        max_delay) {
  # Early return for simple case
  if (n_target <= n_ref_times && n_target >= n_required) {
    return(n_target)
  }

  # Handle target exceeds available reference times
  if (n_target > n_ref_times) {
    return(.handle_target_exceeds_avail(
      n_ref_times, n_required, n_target,
      n_min_delay, n_min_retro_nowcasts
    ))
  }

  # Handle target less than required
  return(.handle_target_insufficient(
    n_target, n_required, n_min_delay,
    n_min_retro_nowcasts, scale_factor, max_delay
  ))
}

#' Helper for when target exceeds available reference times
#'
#' @inheritParams .check_against_requirements
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns number of reference times to use or NULL, invisibly
.handle_target_exceeds_avail <- function(n_ref_times,
                                         n_required,
                                         n_target,
                                         n_min_delay,
                                         n_min_retro_nowcasts) {
  if (n_ref_times >= n_required) {
    cli_warn(message = c(
      "Insufficient reference times in reporting triangle for the specified `scale_factor`.", # nolint
      "i" = "{n_ref_times} reference times available and {n_target} are specified.", # nolint
      "x" = "All {n_ref_times} reference times will be used." # nolint
    ))
    return(n_ref_times)
  }

  cli_abort(message = c(
    "Insufficient reference times in reporting triangle for the both delay and uncertainty estimation.", # nolint
    "i" = "{n_ref_times} reference times available and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint
    "x" = "Probabilistic nowcasts cannot be generated." # nolint
  ))
  return(NULL)
}

#'
#' @inheritParams .check_against_requirements
#' @inheritParams .assign_allocation_from_ns
#' @inheritParams allocate_reference_times
#'
#' @returns NULL invisibly
.handle_target_insufficient <- function(n_target,
                                        n_required,
                                        n_min_delay,
                                        n_min_retro_nowcasts,
                                        scale_factor,
                                        max_delay) {
  cli_abort(message = c(
    "Insufficient reference times specified by `scale_factor` for the both delay and uncertainty estimation.", # nolint
    "i" = "{scale_factor*max_delay} reference times specified and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint,
    "x" = "Probabilistic nowcasts cannot be generated." # nolint
  ))
  return(NULL)
}

#' Assign number of reference times to delay and uncertainty from the sizes
#'
#' @param n_used Integer indicating number reference times that will be used.
#' @param n_target Integer indicating the target number of reference times.
#' @param n_min_delay Integer indicating the number needed for delay
#'    estimation.
#' @param n_min_retro_nowcasts Integer indicating the number needed for
#'    uncertainty estimation.
#' @param n_required Integer indicating the number need for both delay and
#'    uncertainty
#' @param prop_delay Numeric value <1 indicating what proportion of all
#'     reference times in the reporting triangle to be used for delay
#'     estimation.
#'
#' @returns List of number of reference times to use for delay and uncertainty
.assign_allocation_from_ns <- function(n_used,
                                       n_target,
                                       n_min_delay,
                                       n_min_retro_nowcasts,
                                       n_required,
                                       prop_delay) {
  # This is the "standard case", with checks to ensure that minimums are hit
  if (n_used >= n_target) {
    n_history_delay <- max(floor(n_used * prop_delay), n_min_delay)
    n_retrospective_nowcasts <- n_used - n_history_delay
    # If the size used is less than the target size, we will assign the
    # remainder after hitting the delay requirement according to prop_delay
  } else if (n_used >= n_required & n_used < n_target) { # nolint
    # Allocate to n_history_delay and then split the remainder ensuring n_retropsective nowcasts has enough #nolint
    n_remaining_ref_times <- n_used - n_min_delay
    n_retrospective_nowcasts <- ceiling(n_remaining_ref_times * (1 - prop_delay)) # nolint
    n_history_delay <- n_used - n_retrospective_nowcasts
  }

  # Special case: check to make sure minimum
  # requirements for uncertainty were hit
  if (n_retrospective_nowcasts < n_min_retro_nowcasts) {
    n_retrospective_nowcasts <- n_min_retro_nowcasts
    n_history_delay <- n_used - n_retrospective_nowcasts
  }

  return(list(
    n_retrospective_nowcasts = n_retrospective_nowcasts,
    n_history_delay = n_history_delay
  ))
}
