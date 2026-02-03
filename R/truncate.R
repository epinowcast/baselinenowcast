#' Truncate reporting data to quantile-based maximum delay
#'
#' Automatically determines an appropriate maximum delay based on when a
#' specified proportion of cases have been reported (CDF cutoff). This is useful
#' for reducing computational burden when most cases are reported within a
#' shorter delay window.
#'
#' @param x A [reporting_triangle] or [reporting_triangle_df] object
#' @param p Numeric value between 0 and 1 indicating the quantile cutoff.
#'   For example, p = 0.99 truncates to the delay at which 99% of cases have
#'   been reported. Default is 0.99.
#' @param ... Additional arguments passed to methods.
#' @return Object of the same class truncated to the maximum quantile delay,
#'   or the original object if no truncation is needed
#' @family reporting_triangle
#' @family reporting_triangle_df
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
truncate_to_quantile <- function(x, p = 0.99, ...) {
  UseMethod("truncate_to_quantile")
}

#' @rdname truncate_to_quantile
#' @export
#' @method truncate_to_quantile reporting_triangle
truncate_to_quantile.reporting_triangle <- function(x, p = 0.99, ...) {
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

#' @rdname truncate_to_quantile
#' @export
#' @method truncate_to_quantile reporting_triangle_df
truncate_to_quantile.reporting_triangle_df <- function(x, p = 0.99, ...) {
  assert_numeric(p, lower = 0, upper = 1, len = 1)
  assert_reporting_triangle_df(x)

  # Get delays_unit from the object
  delays_unit <- attr(x, "delays_unit")

  # Compute delays for all rows
  delays <- get_delays_from_dates(
    x$report_date,
    x$reference_date,
    delays_unit
  )

  # Find the quantile delay
  max_delay_needed <- as.integer(quantile(delays, probs = p, na.rm = TRUE))

  # If max_delay_needed is invalid, return original
  if (is.na(max_delay_needed) || max_delay_needed < 0) {
    cli_alert_info(
      text = "Could not determine quantile delay, returning original data."
    )
    return(x)
  }

  # Ensure max_delay is at least 1
  max_delay_needed <- max(1L, max_delay_needed)

  current_max_delay <- max(delays, na.rm = TRUE)

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

#' Truncate reporting data to a specific maximum delay
#'
#' Creates a new object with data filtered to include only
#' delays from 0 to the specified maximum delay. This is useful when you want
#' to limit the delay distribution used for estimation.
#'
#' @param x A [reporting_triangle] or [reporting_triangle_df] object.
#' @param max_delay Integer specifying the maximum delay to retain. Must be
#'   between 0 and the current maximum delay.
#' @param ... Additional arguments passed to methods.
#' @return A new object of the same class with delays 0 through max_delay.
#' @family reporting_triangle
#' @family reporting_triangle_df
#' @export
#' @examples
#' # Truncate reporting_triangle to delays 0-2
#' rt_short <- truncate_to_delay(example_downward_corr_rt, max_delay = 2)
#' get_max_delay(rt_short) # Returns 2
#'
#' # Truncate reporting_triangle_df
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rt_df <- as_reporting_triangle_df(data_as_of_df)
#' rt_df_short <- truncate_to_delay(rt_df, max_delay = 10)
truncate_to_delay <- function(x, max_delay, ...) {
  UseMethod("truncate_to_delay")
}

#' @rdname truncate_to_delay
#' @export
#' @method truncate_to_delay reporting_triangle
truncate_to_delay.reporting_triangle <- function(x, max_delay, ...) {
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

#' @rdname truncate_to_delay
#' @export
#' @method truncate_to_delay reporting_triangle_df
truncate_to_delay.reporting_triangle_df <- function(x, max_delay, ...) {
  # Validate max_delay
  if (!is.numeric(max_delay) || length(max_delay) != 1) {
    cli_abort(message = "`max_delay` must be a single numeric value")
  }

  if (max_delay < 0) {
    cli_abort(message = "`max_delay` must be non-negative")
  }

  # Get delays_unit from the object
  delays_unit <- attr(x, "delays_unit")

  # Compute delays
  delays <- get_delays_from_dates(
    x$report_date,
    x$reference_date,
    delays_unit
  )

  # Check current max delay
  current_max_delay <- max(delays, na.rm = TRUE)

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

  # Filter rows where delay <= max_delay
  if (max_delay < current_max_delay) {
    cli_alert_info(
      text = "Truncating from max_delay = {current_max_delay} to {max_delay}."
    )

    filtered_data <- x[delays <= max_delay, , drop = FALSE]

    # Preserve class and attributes
    class(filtered_data) <- class(x)
    attr(filtered_data, "delays_unit") <- delays_unit
    attr(filtered_data, "strata") <- attr(x, "strata")

    # Validate the result
    assert_reporting_triangle_df(filtered_data)

    return(filtered_data)
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

#' Truncate reporting triangle by removing bottom rows
#'
#' Generates a list of retrospective reporting triangles by successively
#'   removing rows from the bottom of the original triangle.
#' Each truncated triangle represents what would have been observed at an
#'   earlier reference time.
#' This function truncates row(s) of the reporting triangle, removing the most
#' recent observations (starting from the bottom of the reporting triangle).
#'
#' @param n Integer indicating the number of retrospective
#'   truncated triangles to be generated, always starting from the most
#'   recent reference time. Default is to generate truncated matrices for
#'   each row up until there are insufficient rows to generate nowcasts
#'   from, where the minimum requirement is one more than the  number of
#'   horizon rows (rows containing NAs).
#' @inheritParams estimate_delay
#' @inheritParams assert_reporting_triangle
#' @returns `trunc_rep_tri_list` List of `n` truncated `reporting_triangle`
#'   objects with as many rows as available given the truncation, and the same
#'   number of columns as the input `reporting_triangle`.
#' @importFrom checkmate assert_integerish
#' @family generate_retrospective_data
#' @export
#' @examples
#' # Generate multiple truncated triangles
#' truncated_rts <- truncate_to_rows(example_reporting_triangle, n = 2)
#' truncated_rts[1:2]
truncate_to_rows <- function(reporting_triangle,
                             n = nrow(reporting_triangle) -
                               sum(is.na(rowSums(reporting_triangle))) - 1,
                             validate = TRUE) {
  assert_reporting_triangle(reporting_triangle, validate)
  assert_integerish(n, lower = 0)
  trunc_rep_tri_list <- lapply(
    seq_len(n),
    function(t) .truncate_triangle_impl(reporting_triangle, t)
  )

  return(trunc_rep_tri_list)
}

#' Truncate reporting triangle by removing a specified number of the last rows
#'
#' Removes the last `t` rows from a reporting triangle to simulate what would
#'   have been observed at an earlier reference time.
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
#' trunc_rep_tri <- truncate_to_row(example_reporting_triangle, t = 1)
#' trunc_rep_tri
truncate_to_row <- function(reporting_triangle,
                            t,
                            validate = TRUE) {
  # Full validation for standalone use
  assert_reporting_triangle(reporting_triangle, validate)
  assert_integerish(t, lower = 0)

  return(.truncate_triangle_impl(reporting_triangle, t))
}
