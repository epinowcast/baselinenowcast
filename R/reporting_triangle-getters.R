#' Get reference dates from reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of Date objects
#' @family reporting_triangle
#' @export
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
get_delays_unit <- function(x) {
  if (inherits(x, "reporting_triangle")) {
    return(attr(x, "delays_unit"))
  }
  cli_abort("x must be a reporting_triangle object")
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
    return(seq(d, by = "month", length.out = n + 1)[n + 1]) # nolint: keyword_quote_linter
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
    return(seq(d, by = "year", length.out = n + 1)[n + 1]) # nolint: keyword_quote_linter
  }, dates, delays, SIMPLIFY = FALSE)
  return(as.Date(unlist(result), origin = "1970-01-01"))
}

#' Get delay unit function for date arithmetic
#'
#' Returns a function that performs date arithmetic based on the delays_unit.
#' This is useful for adding delays to reference dates in a unit-aware way.
#'
#' @param delays_unit Character string specifying the temporal granularity.
#'   Options are `"days"`, `"weeks"`, `"months"`, `"years"`.
#' @return A function that takes a Date vector and numeric delays vector,
#'   returns Date vector with delays added.
#' @family reporting_triangle
#' @export
#' @examples
#' # Get function for days
#' add_days <- get_delay_unit_function("days")
#' ref_date <- as.Date("2024-01-01")
#' add_days(ref_date, 7) # 2024-01-08
#'
#' # Get function for weeks
#' add_weeks <- get_delay_unit_function("weeks")
#' add_weeks(ref_date, 2) # 2024-01-15
get_delay_unit_function <- function(delays_unit) {
  assert_delays_unit(delays_unit)

  result <- switch(delays_unit,
    "days" = .add_days,
    "weeks" = .add_weeks,
    "months" = .add_months,
    "years" = .add_years
  )
  return(result)
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

#' Compute delays between two dates based on delay unit
#'
#' @description Returns a function that computes delays between report dates
#'   and reference dates using the specified time unit. This is the inverse
#'   operation of [get_delay_unit_function()].
#'
#' @param delays_unit Character string specifying the time unit for delays.
#'   Must be one of "days", "weeks", "months", or "years".
#' @return A function that takes two date vectors (report_date, reference_date)
#'   and returns integer delays
#' @family reporting_triangle
#' @keywords internal
get_delay_from_dates_function <- function(delays_unit) {
  assert_delays_unit(delays_unit)

  result <- switch(delays_unit,
    "days" = .diff_days,
    "weeks" = .diff_weeks,
    "months" = .diff_months,
    "years" = .diff_years
  )
  return(result)
}

#' Get mean delay for each row of reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of mean delays for each reference date (numeric)
#' @family reporting_triangle
#' @export
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
    if (is.na(quantile_idx)) delays[length(delays)] else delays[quantile_idx]
  })
  return(as.integer(quantile_delays))
}
