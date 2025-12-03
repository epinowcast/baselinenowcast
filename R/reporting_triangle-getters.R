#' Get reference dates from reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of Date objects
#' @family reporting_triangle
#' @export
#' @examples
#' ref_dates <- get_reference_dates(example_reporting_triangle)
#' head(ref_dates)
get_reference_dates <- function(x) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  return(as.Date(rownames(x)))
}

#' Get maximum delay from reporting_triangle
#'
#' @param x A reporting_triangle object
#' @param non_zero Logical. If TRUE, returns the maximum delay where at least
#'   one observation is non-zero. Useful for identifying the actual extent of
#'   the delay distribution. Default FALSE.
#' @return Maximum delay (integer), or -1 if all zero when non_zero = TRUE
#' @family reporting_triangle
#' @export
#' @examples
#' # Get maximum delay from triangle structure
#' max_delay <- get_max_delay(example_reporting_triangle)
#' max_delay
#'
#' # Get maximum delay with non-zero observations
#' max_delay_nz <- get_max_delay(example_reporting_triangle, non_zero = TRUE)
#' max_delay_nz
get_max_delay <- function(x, non_zero = FALSE) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }

  if (non_zero) {
    col_sums <- colSums(x, na.rm = TRUE)
    non_zero_cols <- which(col_sums > 0)
    if (length(non_zero_cols) == 0) {
      return(-1L)
    }
    return(max(non_zero_cols) - 1L)
  }

  return(ncol(x) - 1L)
}

#' Get delays unit from a reporting triangle
#'
#' @param x A [reporting_triangle] object.
#' @return Character string indicating the delays unit.
#' @family reporting_triangle
#' @export
#' @examples
#' delays_unit <- get_delays_unit(example_reporting_triangle)
#' delays_unit
get_delays_unit <- function(x) {
  if (inherits(x, "reporting_triangle")) {
    return(attr(x, "delays_unit"))
  }
  return(cli_abort("x must be a reporting_triangle object"))
}

#' Internal: Add days to dates
#' @param dates Date vector
#' @param delays Numeric vector of delays
#' @return Date vector
#' @keywords internal
.add_days <- function(dates, delays) {
  return(dates + delays)
}

#' Internal: Add weeks to dates
#' @param dates Date vector
#' @param delays Numeric vector of delays
#' @return Date vector
#' @keywords internal
.add_weeks <- function(dates, delays) {
  return(dates + (delays * 7))
}

#' Internal: Add months to dates
#' @param dates Date vector
#' @param delays Numeric vector of delays
#' @return Date vector
#' @keywords internal
.add_months <- function(dates, delays) {
  # Use seq.Date for proper month arithmetic
  result <- mapply(function(d, n) {
    if (n == 0) return(d)
    return(seq(d, by = "month", length.out = n + 1)[n + 1])
  }, dates, delays, SIMPLIFY = FALSE)
  return(as.Date(unlist(result), origin = "1970-01-01"))
}

#' Internal: Add years to dates
#' @param dates Date vector
#' @param delays Numeric vector of delays
#' @return Date vector
#' @keywords internal
.add_years <- function(dates, delays) {
  # Use seq.Date for proper year arithmetic
  result <- mapply(function(d, n) {
    if (n == 0) return(d)
    return(seq(d, by = "year", length.out = n + 1)[n + 1])
  }, dates, delays, SIMPLIFY = FALSE)
  return(as.Date(unlist(result), origin = "1970-01-01"))
}

#' Compute report dates from reference dates and delays
#'
#' Adds delays to reference dates using unit-aware date arithmetic.
#'
#' @param reference_dates Date vector of reference dates.
#' @param delays Numeric vector of delays.
#' @inheritParams as_reporting_triangle
#' @return Date vector of report dates.
#' @family reporting_triangle
#' @export
#' @examples
#' # Compute report dates with days
#' ref_date <- as.Date("2024-01-01")
#' get_report_dates(ref_date, 7, "days") # 2024-01-08
#'
#' # Compute report dates with weeks
#' get_report_dates(ref_date, 2, "weeks") # 2024-01-15
get_report_dates <- function(reference_dates, delays, delays_unit) {
  assert_delays_unit(delays_unit)

  add_fn <- switch(delays_unit,
    days = .add_days,
    weeks = .add_weeks,
    months = .add_months,
    years = .add_years
  )
  return(add_fn(reference_dates, delays))
}

#' Internal: Compute day difference between dates
#' @param report_date Date vector of report dates
#' @param reference_date Date vector of reference dates
#' @return Numeric vector of delays
#' @keywords internal
.diff_days <- function(report_date, reference_date) {
  return(as.numeric(difftime(
    as.Date(report_date),
    as.Date(reference_date),
    units = "days"
  )))
}

#' Internal: Compute week difference between dates
#' @param report_date Date vector of report dates
#' @param reference_date Date vector of reference dates
#' @return Numeric vector of delays
#' @keywords internal
.diff_weeks <- function(report_date, reference_date) {
  return(as.numeric(difftime(
    as.Date(report_date),
    as.Date(reference_date),
    units = "weeks"
  )))
}

#' Internal: Compute month difference between dates
#' @param report_date Date vector of report dates
#' @param reference_date Date vector of reference dates
#' @return Numeric vector of delays
#' @keywords internal
.diff_months <- function(report_date, reference_date) {
  # Compute month difference
  report <- as.Date(report_date)
  reference <- as.Date(reference_date)

  result <- mapply(function(r, ref) {
    if (r == ref) return(0)

    years_diff <- as.numeric(format(r, "%Y")) -
      as.numeric(format(ref, "%Y"))
    months_diff <- as.numeric(format(r, "%m")) -
      as.numeric(format(ref, "%m"))

    return(years_diff * 12 + months_diff)
  }, report, reference)

  return(as.numeric(result))
}

#' Internal: Compute year difference between dates
#' @param report_date Date vector of report dates
#' @param reference_date Date vector of reference dates
#' @return Numeric vector of delays
#' @keywords internal
.diff_years <- function(report_date, reference_date) {
  # Compute year difference
  report <- as.Date(report_date)
  reference <- as.Date(reference_date)

  result <- as.numeric(format(report, "%Y")) -
    as.numeric(format(reference, "%Y"))

  return(as.numeric(result))
}

#' Compute delays between report dates and reference dates
#'
#' @description Computes delays between report dates and reference dates
#'   using the specified time unit. This is the inverse operation of
#'   [get_report_dates()].
#'
#' @param report_dates Date vector of report dates.
#' @param reference_dates Date vector of reference dates.
#' @inheritParams as_reporting_triangle
#' @return Numeric vector of delays.
#' @family reporting_triangle
#' @export
#' @examples
#' # Compute delays in days
#' ref_date <- as.Date("2024-01-01")
#' report_date <- as.Date("2024-01-08")
#' get_delays_from_dates(report_date, ref_date, "days") # 7
#'
#' # Compute delays in weeks
#' report_date_weeks <- as.Date("2024-01-15")
#' get_delays_from_dates(report_date_weeks, ref_date, "weeks") # 2
get_delays_from_dates <- function(report_dates, reference_dates,
                                  delays_unit) {
  assert_delays_unit(delays_unit)

  diff_fn <- switch(delays_unit,
    days = .diff_days,
    weeks = .diff_weeks,
    months = .diff_months,
    years = .diff_years
  )
  return(diff_fn(report_dates, reference_dates))
}

#' Get mean delay for each row of reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of mean delays for each reference date (numeric)
#' @family reporting_triangle
#' @importFrom stats weighted.mean
#' @export
#' @examples
#' mean_delays <- get_mean_delay(example_reporting_triangle)
#' mean_delays
get_mean_delay <- function(x) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  delays <- 0:get_max_delay(x)
  x_mat <- as.matrix(x)
  mean_delays <- apply(x_mat, 1, function(row_counts) {
    if (sum(row_counts, na.rm = TRUE) == 0) return(NA_real_)
    return(weighted.mean(delays, row_counts, na.rm = TRUE))
  })
  return(mean_delays)
}

#' Get quantile delay for each row of reporting_triangle
#'
#' @param x A reporting_triangle object
#' @param p Numeric value between 0 and 1 indicating the quantile to compute.
#'   For example, p = 0.99 returns the delay at which 99% of cases have been
#'   reported. Default is 0.99.
#' @return Vector of quantile delays for each reference date (integer). Returns
#'   NA for rows with no observations.
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_numeric
#' @examples
#' # Get 99th percentile delay for each reference date
#' quantile_delays_99 <- get_quantile_delay(example_reporting_triangle)
#' quantile_delays_99
#'
#' # Get median delay
#' median_delays <- get_quantile_delay(example_reporting_triangle, p = 0.5)
#' median_delays
get_quantile_delay <- function(x, p = 0.99) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  assert_numeric(p, lower = 0, upper = 1, len = 1)

  delays <- 0:get_max_delay(x)
  x_mat <- as.matrix(x)
  quantile_delays <- apply(x_mat, 1, function(row_counts) {
    total_counts <- sum(row_counts, na.rm = TRUE)
    if (total_counts == 0) return(NA_integer_)

    # Calculate cumulative proportion and find first delay >= quantile
    cumulative_prop <- cumsum(row_counts) / total_counts
    quantile_idx <- which(cumulative_prop >= p)[1]

    # Return max delay if quantile never reached, otherwise return the delay
    if (is.na(quantile_idx)) {
      return(delays[length(delays)])
    }
    return(delays[quantile_idx])
  })
  return(as.integer(quantile_delays))
}
#' Get reporting structure from a reporting triangle
#'
#' Returns an integer or vector specifying the reporting structure, which
#' indicates how the reporting triangle is organized. This structure tells
#' [apply_reporting_structure()] how to create new reporting triangles with
#' the same reporting pattern.
#'
#' @inheritParams estimate_delay
#'
#' @returns  Integer or vector specifying the reporting structure.
#'   If integer, divides columns evenly by that integer (with last possibly
#'   truncated).  If vector, the sum must not be greater than or equal to the
#'   number of columns. Default is 1 (standard triangular structure). If
#'   there are no NAs, will return 0.
#' @family reporting_triangle
#' @export
#'
#' @examples
#' # Get structure from example triangle
#' structure <- get_reporting_structure(example_reporting_triangle)
#' structure
get_reporting_structure <- function(reporting_triangle) {
  n_row_nas <- sum(is.na(rowSums(reporting_triangle)))
  # Structure is 0 if there are no NAs
  if (n_row_nas == 0) {
    cli_alert_info(
      text =
        "The reporting triangle does not contain any missing values." # nolint
    )
    return(0)
  }
  n_prev_nas <- 0
  structure_long <- rep(NA, ncol(reporting_triangle))
  for (i in 1:n_row_nas) {
    n_nas <- sum(!is.na(reporting_triangle[nrow(reporting_triangle) - i + 1, ])) - n_prev_nas # nolint
    structure_long[i] <- n_nas
    n_prev_nas <- n_prev_nas + n_nas
  }
  struct <- structure_long[!is.na(structure_long)]

  # Check to see if this can be reduced to just a single number
  expanded <- .expand_structure_vec(struct[1],
    cols = ncol(reporting_triangle)
  )

  if (identical(expanded, struct)) {
    struct <- struct[1]
  }
  return(struct)
}
