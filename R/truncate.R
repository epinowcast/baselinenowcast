#' Truncate reporting_triangle to quantile-based maximum delay
#'
#' Automatically determines an appropriate maximum delay based on when a
#' specified proportion of cases have been reported (CDF cutoff). This is useful
#' for reducing computational burden when most cases are reported within a
#' shorter delay window.
#'
#' @param x A reporting_triangle object
#' @param p Numeric value between 0 and 1 indicating the quantile cutoff.
#'   For example, p = 0.99 truncates to the delay at which 99% of cases have
#'   been reported. Default is 0.99.
#' @return A reporting_triangle object truncated to the maximum quantile delay,
#'   or the original object if no truncation is needed
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom cli cli_alert_info
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' # Create triangle, max_delay is automatically computed
#' rep_tri <- suppressMessages(as_reporting_triangle(data = data_as_of_df))
#'
#' # Check the maximum delay in the triangle
#' ncol(rep_tri)
#'
#' # Truncate to 99th percentile of reporting
#' rep_tri_trunc <- truncate_to_quantile(rep_tri, p = 0.99)
#' ncol(rep_tri_trunc)
#'
#' # More aggressive truncation
#' rep_tri_trunc90 <- truncate_to_quantile(rep_tri, p = 0.90)
#' ncol(rep_tri_trunc90)
truncate_to_quantile <- function(x, p = 0.99) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  assert_numeric(p, lower = 0, upper = 1, len = 1)

  # Get quantile delays for each reference date
  quantile_delays <- get_quantile_delay(x, p = p)

  # Find maximum delay needed across all reference dates
  # Suppress warning when all values are NA (handled below)
  max_delay_needed <- suppressWarnings(max(quantile_delays, na.rm = TRUE))

  # If max_delay_needed is invalid (NA, -Inf, or -1), return original
  if (is.na(max_delay_needed) || is.infinite(max_delay_needed) ||
    max_delay_needed < 0) {
    cli_alert_info(
      text = "Could not determine quantile delay, returning original triangle."
    )
    return(x)
  }

  # Ensure max_delay is at least 1 (for validation requirement)
  max_delay_needed <- max(1L, as.integer(max_delay_needed))

  current_max_delay <- get_max_delay(x)

  # Only truncate if we can reduce the max_delay
  if (max_delay_needed < current_max_delay) {
    cli_alert_info(
      text = "Truncating to {max_delay_needed} based on {p * 100}% quantile."
    )
    return(truncate_to_delay(x, max_delay = max_delay_needed))
  } else {
    cli_alert_info(
      text = "No truncation needed: {p * 100}% of cases reported within existing max_delay = {current_max_delay}." # nolint
    )
    return(x)
  }
}

#' Truncate reporting triangle to a specific maximum delay
#'
#' Creates a new reporting_triangle with columns filtered to include only
#' delays from 0 to the specified maximum delay. This is useful when you want
#' to limit the delay distribution used for estimation.
#'
#' @param x A [reporting_triangle] object.
#' @param max_delay Integer specifying the maximum delay to retain. Must be
#'   between 0 and the current maximum delay of the triangle.
#' @return A new [reporting_triangle] object with delays 0 through max_delay.
#' @family reporting_triangle
#' @export
#' @examples
#' # Truncate to delays 0-2
#' rt_short <- truncate_to_delay(example_downward_corr_rt, max_delay = 2)
#' get_max_delay(rt_short)  # Returns 2
truncate_to_delay <- function(x, max_delay) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }

  current_max_delay <- get_max_delay(x)

  # Validate max_delay
  if (!is.numeric(max_delay) || length(max_delay) != 1) {
    cli_abort(message = "`max_delay` must be a single numeric value")
  }

  if (max_delay < 0) {
    cli_abort(message = "`max_delay` must be non-negative")
  }

  if (max_delay > current_max_delay) {
    cli_abort(
      message = c(
        "`max_delay` cannot be greater than current maximum delay",
        i = "Current max_delay is {current_max_delay}, requested {max_delay}"
      )
    )
  }

  # If max_delay unchanged, return original
  if (max_delay == current_max_delay) {
    return(x)
  }

  # Truncate if we can reduce the max_delay
  if (max_delay < current_max_delay) {
    cli_alert_info(
      text = "Truncating from max_delay = {current_max_delay} to {max_delay}."
    )

    # Subset columns
    trunc_mat <- as.matrix(x)[, 1:(max_delay + 1), drop = FALSE]

    # Update with new matrix
    return(.update_triangle_matrix(x, trunc_mat))
  }

  return(x)
}

#' Internal truncate implementation without validation
#'
#' @param reporting_triangle A [reporting_triangle] object (already validated).
#' @param t Integer indicating truncation amount.
#' @return Truncated [reporting_triangle] object.
#' @keywords internal
#' @noRd
.truncate_triangle_impl <- function(reporting_triangle, t) {
  n_obs <- nrow(reporting_triangle)
  if (t >= n_obs) {
    cli_abort(
      message = c(
        "The as of time point is greater than or equal to the number of ",
        "rows in the original triangle."
      )
    )
  }

  # Use head which preserves class and attributes
  n_rows <- n_obs - t
  return(head(reporting_triangle, n = n_rows))
}

#' Generate truncated reporting triangles
#'
#' This function ingests a reporting triangle/matrix and the number of
#'   truncated reporting triangles we want to create, `n`, and iteratively
#'   truncates the reporting triangle, working from the latest reference time
#'   (bottom) to the older reference times (top) for `n`
#'   snapshots.
#'
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to generate truncated matrices for
#'   each row up until there are insufficient rows to generate nowcasts
#'   from, where the minimum requirement is one more than the  number of
#'   horizon rows (rows containing NAs).
#' @inheritParams estimate_delay
#' @returns `trunc_rep_tri_list` List of `n` truncated `reporting_triangle`
#'   objects with as many rows as available given the truncation, and the same
#'   number of columns as the input `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @family generate_retrospective_data
#' @export
#' @examples
#' # Generate multiple truncated triangles
#' truncated_rts <- truncate_triangles(example_reporting_triangle, n = 2)
#' truncated_rts[1:2]
truncate_triangles <- function(reporting_triangle,
                               n = nrow(reporting_triangle) -
                                 sum(is.na(rowSums(reporting_triangle))) - 1) {
  assert_reporting_triangle(reporting_triangle)
  assert_integerish(n, lower = 0)
  trunc_rep_tri_list <- lapply(
    seq_len(n),
    function(t) .truncate_triangle_impl(reporting_triangle, t)
  )

  return(trunc_rep_tri_list)
}

#' Get a single truncated triangle
#'
#' This function takes in a reporting triangle and an integer `t` and generates
#'   a truncated reporting triangle, removing the last `t` observations.
#'
#' @inheritParams estimate_delay
#' @param t Integer indicating the number of timepoints to truncate off the
#'   bottom of the original reporting triangle.
#' @returns `trunc_rep_tri` A `reporting_triangle` object with `t` fewer rows
#'   than the input. The class and metadata are preserved with updated reference
#'   dates.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#' @family generate_retrospective_data
#' @export
#' @examples
#' # Generate single truncated triangle
#' trunc_rep_tri <- truncate_triangle(example_reporting_triangle, t = 1)
#' trunc_rep_tri
truncate_triangle <- function(reporting_triangle,
                              t) {
  # Full validation for standalone use
  assert_reporting_triangle(reporting_triangle)
  assert_integerish(t, lower = 0)

  return(.truncate_triangle_impl(reporting_triangle, t))
}
