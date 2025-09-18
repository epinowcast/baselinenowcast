#' Allocate training volume based on combination of defaults and user-specified
#'   values for training volume for delay and uncertainty estimation.
#' @description Given the reporting triangle, the maximum delay, and
#'    optionally the user-specified scale factor on the max delay to be used as
#'    total reference times and the proportion of those reference times to
#'    be used for delay estimation, allocate reference times accordingly
#'
#' @inheritParams estimate_delay
#' @param scale_factor Numeric value indicating the multiplicative factor on
#'     the maximum delay to be used for estimation of delay and uncertainty.
#'      Default is `3`.
#' @param prop_delay Numeric value <1 indicating what proportion of all
#'     reference times in the reporting triangle to be used for delay
#'     estimation. Default is `0.5`.
#' @param size_min_retro_nowcasts Integer indicating the minimum number of
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
                                     size_min_retro_nowcasts = 2) {
  # Checks of inputs
  .validate_triangle(reporting_triangle, max_delay)
  .validate_allocation_params(
    scale_factor, prop_delay,
    size_min_retro_nowcasts
  )
  sizes <- .calculate_sizes(
    reporting_triangle, max_delay, scale_factor,
    size_min_retro_nowcasts
  )
  n_ref_times <- sizes$n_ref_times
  size_required <- sizes$size_required
  size_min_delay <- sizes$size_min_delay
  size_target <- sizes$size_target

  # Check for scale factor being too high.
  if (size_target > n_ref_times && n_ref_times >= size_required) {
    cli_warn(message = c(
      "Insufficient reference times in reporting triangle for the specified `scale_factor`.", # nolint
      "i" = "{n_ref_times} reference times available and {size_target} are specified.", # nolint
      "x" = "All {n_ref_times} reference times will be used." # nolint
    ))
    size_used <- n_ref_times
  } else if (size_target > n_ref_times && n_ref_times < size_required) {
    cli_abort(message = c(
      "Insufficient reference times in reporting triangle for the both delay and uncertainty estimation.", # nolint
      "i" = "{n_ref_times} reference times available and {size_required} are needed, {size_min_delay} for delay estimation and {size_min_retro_nowcasts} for uncertainty estimation.", # nolint
      "x" = "Probabilistic nowcasts cannot be generated. " # nolint
    ))
  } else if (size_target < n_ref_times && size_target < size_required) {
    cli_abort(message = c(
      "Insufficient reference times specified by `scale_factor` for the both delay and uncertainty estimation.", # nolint
      "i" = "{scale_factor*max_delay} reference times specified and {size_required} are needed, {size_min_delay} for delay estimation and {size_min_retro_nowcasts} for uncertainty estimation.", # nolint
      "x" = "Probabilistic nowcasts cannot be generated. " # nolint
    ))
  } else {
    size_used <- size_target
  }


  # If size being used equals or exceeds the target, simply split according to prop delay #nolint
  if (size_used >= size_target) {
    n_history_delay <- max(floor(size_used * prop_delay), size_min_delay)
    n_retrospective_nowcasts <- size_used - n_history_delay
    if (n_retrospective_nowcasts < size_min_retro_nowcasts) {
      n_retrospective_nowcasts <- size_min_retro_nowcasts
      n_history_delay <- size_used - n_retrospective_nowcasts
    }
    # If less than the target size, we will assign the remainder after hitting
    # the delay requirement according to prop_delay
  } else if (size_used >= size_required & size_used < size_target) { # nolint
    # Allocate to n_history_delay and then split the remainder ensuring n_retropsective nowcasts has enough #nolint
    n_remaining_ref_times <- size_used - size_min_delay
    n_retrospective_nowcasts <- max(
      ceiling(n_remaining_ref_times * (1 - prop_delay)),
      size_min_retro_nowcasts
    )
    n_history_delay <- size_used - n_retrospective_nowcasts
  }

  prop_delay_used <- n_history_delay / size_used

  if (prop_delay_used != prop_delay) {
    cli_warn(
      message = c(
        "`prop_delay` specified is not equivalent to `prop_delay` used.",
        "i" = "{prop_delay} reference times were specified for delay estimation but due to minimum requirements had to be reallocated.", # nolint
        "x" = "{round(prop_delay_used,3)} of reference times used for delay estimation." # nolint
      )
    )
  }

  message(sprintf("Using %d reference times for delay estimation.", n_history_delay)) # nolint
  message(sprintf("Using %d reference times as retrospective nowcast times for uncertainty estimation.", n_retrospective_nowcasts)) # nolint

  return(list(
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts
  ))
}

#' Helper function to validate allocation parameters
#'
#'
#' @inheritParams allocate_reference_times
#'
#' @returns NULL invisibly
.validate_allocation_params <- function(scale_factor,
                                        prop_delay,
                                        size_min_retro_nowcasts) {
  assert_integerish(size_min_retro_nowcasts, lower = 0)

  if (!is.numeric(scale_factor) || length(scale_factor) != 1 ||
    !is.finite(scale_factor) || scale_factor <= 0) {
    cli_abort("`scale_factor` must be a finite positive scalar.")
  }

  if (!is.numeric(prop_delay) || length(prop_delay) != 1 ||
    !is.finite(prop_delay) || prop_delay <= 0 || prop_delay >= 1) {
    cli_abort("`prop_delay` must be in (0, 1).")
  }
  return(NULL)
}

#' Helper function to calculate various size requirements
#' @inheritParams allocate_reference_times
#'
#' @returns list of the integer sizes
.calculate_sizes <- function(reporting_triangle,
                             max_delay,
                             scale_factor,
                             size_min_retro_nowcasts) {
  n_ref_times <- nrow(reporting_triangle)
  size_min_delay <- sum(is.na(rowSums(reporting_triangle))) + 1
  size_required <- size_min_delay + size_min_retro_nowcasts
  size_target <- scale_factor * max_delay

  sizes <- list(
    n_ref_times = n_ref_times,
    size_min_delay = size_min_delay,
    size_required = size_required,
    size_target = size_target
  )
  return(sizes)
}
