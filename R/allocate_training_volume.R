#' Allocate training volume based on combination of defaults and user-specified
#'   values for training volume for delay and uncertainty estimation.
#' @description Given the number of references times, the maximum delay, and
#'    optionally the user-specified number of reference times used for delay
#'    (`n_history_delay`) and/or the number of reference times used as
#'    retrospective nowcasts for uncertainty estimation
#'    (`n_retrospective_nowcasts`), allocated the training volume accordingly
#'    or return the allocations specified if feasible.
#'
#' @param n_ref_times Integer indicating the number of reference times (rows)
#'    in the reporting triangle.
#' @inheritParams estimate_and_apply_uncertainty
#' @param n_retrospective_nowcasts
#'
#' @returns list of n_history_delay and n_retrospective_nowcasts
.allocate_training_volume <- function(n_ref_times,
                                      max_delay,
                                      n_history_delay = NULL,
                                      n_retrospective_nowcasts = NULL) {
  if (!is.null(n_history_delay) && !is.null(n_retrospective_nowcasts) &&
    (n_ref_times < n_history_delay + n_retrospective_nowcasts)) {
    cli_abort(message = c(
      "Insufficient reference times in reporting triangle for specified training volume.", # nolint
      "i" = "{n_history_delay + n_retrospective_nowcasts} reference times are specified for delay and uncertainty estimation.", # nolint
      "x" = "Only {n_ref_times} reference times are available in the reporting triangle." # nolint
    ))
  }

  # Logic for how to handle if one is passed in but not the other
  if (is.null(n_history_delay) && !is.null(n_retrospective_nowcasts)) {
    n_history_delay <- max(
      max_delay + 1,
      min(3 * max_delay, n_ref_times) - n_retrospective_nowcasts
    ) # nolint
    # Check to make sure this doesn't exceed n_ref times
    if (n_ref_times < n_history_delay + n_retrospective_nowcasts) {
      cli_abort(message = c(
        "i" = "{n_history_delay} reference times are required for delay estimation, and this plus {n_retrospective_nowcasts} retrospective nowcasts is more than the available {n_ref_times} reference times." # nolint
      ))
    }
  }

  if (is.null(n_retrospective_nowcasts) && !is.null(n_history_delay)) {
    n_retrospective_nowcasts <- min(3 * max_delay, n_ref_times) - n_history_delay # nolint
  }


  if (is.null(n_history_delay) && is.null(n_retrospective_nowcasts)) {
    if (n_ref_times >= 3 * max_delay) {
      n_history_delay <- floor(1.5 * max_delay)
      n_retrospective_nowcasts <- 3 * max_delay - n_history_delay
    } else if (n_ref_times >= max_delay + 3 &&
      n_ref_times <= 3 * max_delay) {
      # Allocate to n_history_delay and then split
      n_remaining_ref_times <- n_ref_times - max_delay - 1
      n_retrospective_nowcasts <- max(2, ceiling(n_remaining_ref_times / 2))
      n_history_delay <- n_ref_times - n_retrospective_nowcasts
    } else {
      cli_abort(message = c(
        "Insufficient reference times in reporting triangle for delay and uncertainty estimation.", # nolint
        "i" = "{max_delay + 1} reference times are required for delay estimation and 2 reference times are required as retrospective nowcasts for uncertainty estimation.", # nolint
        "x" = "Only {n_ref_times} of the {max_delay + 3} required reference times are available in the reporting triangle." # nolint
      ))
    }
  }

  if (n_retrospective_nowcasts < 2) {
    cli_abort(
      message = c(
        "Insufficient reference times for uncertainty estimation.", # nolint
        "i" = "There must be at least 2 reference times used as retrospective nowcast times for uncertainty estimation." # nolint
      )
    )
  }

  if (n_history_delay < max_delay + 1) {
    cli_abort(
      message = c(
        "User-specified `n_history_delay` is insufficient for delay estimation.", # nolint
        "i" = "At least {max_delay + 1} reference times are needed for delay estimation.", # nolint
        "x" = "The specified {n_history_delay} reference times for delay estimation is insufficient" # nolint
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
