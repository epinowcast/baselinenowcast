#' Allocate training volume based on combination of defaults and user-specified
#'   values for training volume for delay and uncertainty estimation.
#' @description Given the reporting triangle and optionally the user-specified
#'    scale factor on the max delay to be used as total reference times and the
#'    proportion of those reference times to be used for delay estimation,
#'    allocate reference times to the number used for delay estimation and the
#'    number used as retrospective nowcasts for uncertainty estimation.
#'
#'    This function implements an algorithm which:
#'
#'    - if the specified number of reference times
#'    (`scale_factor` x `max delay`) is less than or equal to the number of
#'    reference times available in the reporting triangle, split reference times
#'    between delay and uncertainty according to `prop_delay`, ensuring that the
#'    minimum requirements for delay and uncertainty estimation are met.
#'    - if the specified number of reference times is greater than the
#'    number of reference times available in the reporting triangle,
#'    use all the reference times available and satisfy the minimum
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
#' @returns list of n_history_delay and n_retrospective_nowcasts
#' @family workflow_wrappers
#' @export
#' @examples
#' # Create a reporting triangle from package data
#' data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data_as_of) |>
#'   truncate_to_delay(max_delay = 25)
#'
#' # Use the defaults (scale_factor = 3, prop_delay = 0.5)
#' ref_time_allocation_default <- allocate_reference_times(rep_tri)
#' ref_time_allocation_default
#'
#' # Modify to use less volume and redistribute
#' ref_time_allocation_alt <- allocate_reference_times(
#'   reporting_triangle = rep_tri,
#'   scale_factor = 2,
#'   prop_delay = 0.6
#' )
#' ref_time_allocation_alt
allocate_reference_times <- function(reporting_triangle,
                                     scale_factor = 3,
                                     prop_delay = 0.5,
                                     n_min_retro_nowcasts = 2) {
  # Checks of inputs
  assert_reporting_triangle(reporting_triangle)
  max_delay <- get_max_delay(reporting_triangle)
  .validate_inputs_allocation(
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
#' @keywords internal
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

  return(list(
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts
  ))
}


#' Helper function to calculate various size requirements
#' @inheritParams allocate_reference_times
#'
#' @returns list of the integer sizes
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
.handle_target_exceeds_avail <- function(n_ref_times,
                                         n_required,
                                         n_target,
                                         n_min_delay,
                                         n_min_retro_nowcasts) {
  if (n_ref_times >= n_required) {
    cli_warn(message = c(
      "{n_ref_times} reference times available and {n_target} are specified.", # nolint
      "i" = "All {n_ref_times} reference times will be used." # nolint
    ))
    return(n_ref_times)
  }

  cli_abort(message = c(
    "{n_ref_times} reference times available and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint
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
    "{scale_factor*max_delay} reference times specified and {n_required} are needed, {n_min_delay} for delay estimation and {n_min_retro_nowcasts} for uncertainty estimation.", # nolint,
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
#' @keywords internal
.assign_allocation_from_ns <- function(n_used,
                                       n_target,
                                       n_min_delay,
                                       n_min_retro_nowcasts,
                                       n_required,
                                       prop_delay) {
  # This is the "standard case", with checks to ensure that minimums are hit
  flag_req_delay <- FALSE
  flag_req_uncertainty <- FALSE
  if (n_used >= n_target) {
    n_history_delay <- max(c(floor(n_used * prop_delay), n_min_delay))
    flag_req_delay <- which.max(c(floor(n_used * prop_delay), n_min_delay)) == 2
    n_retrospective_nowcasts <- n_used - n_history_delay
    # If the size used is less than the target size, we will assign the
    # remainder after hitting the delay requirement according to prop_delay
  } else if (n_used >= n_required & n_used < n_target) { # nolint
    # Allocate to n_history_delay and then split the remainder ensuring n_retropsective nowcasts has enough #nolint
    n_remaining_ref_times <- n_used - n_min_delay
    n_retrospective_nowcasts <- ceiling(n_remaining_ref_times * (1 - prop_delay)) # nolint
    n_history_delay <- n_used - n_retrospective_nowcasts
  }

  # Special case as a final check to make sure minimum
  # requirements for uncertainty were hit
  if (n_retrospective_nowcasts < n_min_retro_nowcasts) {
    n_retrospective_nowcasts <- n_min_retro_nowcasts
    flag_req_uncertainty <- TRUE
    n_history_delay <- n_used - n_retrospective_nowcasts
  }

  prop_delay_used <- n_history_delay / n_used

  if (round(prop_delay_used, 3) != round(prop_delay, 3)) {
    cli_alert_info(
      text =
        "{prop_delay} reference times were specified for delay estimation but {round(prop_delay_used,3)} of reference times used for delay estimation." # nolint
    )
    if (flag_req_uncertainty) {
      cli_alert_info("This is due to the minumim requirement for the number of retrospective nowcasts for uncertainty estimation ({n_min_retro_nowcasts}).") # nolint
    } else if (flag_req_delay) {
      cli_alert_info("This is due to the minumim requirement for the number of reference times needed for delay estimation ({n_min_delay}).") # nolint
    } else {
      cli_alert_info("`prop_delay` not identical to the proportion of reference times used for delay estimation due to rounding.") # nolint
    }
  }

  return(list(
    n_retrospective_nowcasts = n_retrospective_nowcasts,
    n_history_delay = n_history_delay
  ))
}
