#' Reporting Triangle Object
#' @name reporting_triangle-class
#' @aliases reporting_triangle
#' @family reporting_triangle
#'
#' @description
#' A `reporting_triangle` object contains the data and metadata needed for
#' nowcasting.
#'
#' @section Structure:
#' A `reporting_triangle` is a matrix with class
#' `c("reporting_triangle", "matrix")`:
#'
#' - Rows: Reference dates
#' - Columns: Delays (0, 1, 2, ...)
#' - Row names: Reference dates as character
#' - Column names: Delays as character
#'
#' Attributes:
#' - `delays_unit`: Character ("days", "weeks", "months", "years")
#'
#' Reference dates are stored as row names and can be extracted using
#' [get_reference_dates()].
#' The maximum delay can be obtained using [get_max_delay()].
#' The structure can be computed using [get_reporting_structure()].
#' See the corresponding [as_reporting_triangle.matrix()] and
#' [as_reporting_triangle.data.frame()] functions
#' for more details on the required input formats to generate the object.
#'
#' @section Low-level function compatibility:
#' The matrix-based structure works seamlessly with low-level functions:
#'
#' - [fill_triangle()]: Fill missing values with zeros
#' - [estimate_delay()]: Extract delay distribution from triangle
#' - [apply_delay()]: Apply delay distribution for nowcasting
#' - [truncate_triangle()]: Remove most recent rows
#' - Standard matrix operations: `rowSums()`, `colSums()`, subsetting
#'
#' @examples
#' # Create a reporting triangle from data
#' data <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data = data)
#'
#' # Use with low-level functions
#' filled <- fill_triangle(rep_tri)
#' delay_pmf <- estimate_delay(rep_tri)
#' nowcast <- apply_delay(rep_tri, delay_pmf)
#'
#' # Direct matrix operations
#' total_by_date <- rowSums(rep_tri, na.rm = TRUE)
#' total_by_delay <- colSums(rep_tri, na.rm = TRUE)
#'
#' # Subsetting and inspection
#' recent <- tail(rep_tri, n = 10)
#' summary(rep_tri)
NULL

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

    # Create new reporting_triangle
    result <- new_reporting_triangle(
      reporting_triangle_matrix = trunc_mat,
      reference_dates = get_reference_dates(x),
      delays_unit = get_delays_unit(x)
    )

    return(result)
  }

  return(x)
}

#' Class constructor for `reporting_triangle` objects
#'
#' Creates a new reporting_triangle object from a matrix.
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle where rows
#'   are reference times, columns are delays, and entries are incident counts.
#' @param reference_dates Vector of Date objects indicating the reference dates
#'   corresponding to each row of the matrix.
#' @param delays_unit Character string specifying the temporal granularity.
#'   Options are `"days"`, `"weeks"`, `"months"`, `"years"`.
#'
#' @returns An object of class [reporting_triangle]
#' @family reporting_triangle
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   delays_unit) {
  .validate_rep_tri_args(
    reporting_triangle_matrix,
    reference_dates,
    delays_unit
  )

  max_delay <- ncol(reporting_triangle_matrix) - 1L
  rownames(reporting_triangle_matrix) <- as.character(reference_dates)
  colnames(reporting_triangle_matrix) <- as.character(0:max_delay)

  result <- structure(
    reporting_triangle_matrix,
    class = c("reporting_triangle", "matrix"),
    delays_unit = delays_unit
  )
  return(result)
}

#' Validate a reporting_triangle object
#'
#' @param data A [reporting_triangle] object to validate
#' @return The validated object (invisibly) or throws error
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
validate_reporting_triangle <- function(data) {
  if (!inherits(data, "matrix")) {
    cli_abort(message = "data must be a matrix")
  }
  if (!inherits(data, "reporting_triangle")) {
    cli_abort(message = "data must have class 'reporting_triangle'")
  }

  # Check matrix is not all NA
  if (all(is.na(data))) {
    cli_abort(message = "Matrix cannot be all NA")
  }

  # Check NA pattern (IF NAs exist, THEN must be in bottom-right pattern)
  if (anyNA(data)) {
    na_check <- .check_na_pattern(data)
    if (!na_check$valid) {
      cli_abort(
        message = c(
          "!" = "Invalid reporting triangle structure",
          "x" = paste0(
            "Found ", na_check$n_out_of_pattern, " NA value",
            if (na_check$n_out_of_pattern != 1) "s" else "",
            " in unexpected position",
            if (na_check$n_out_of_pattern != 1) "s" else ""
          ),
          "i" = paste0(
            "NA values should only appear in the bottom right ",
            "portion of the triangle"
          ),
          "i" = paste0(
            "Affected row", if (length(na_check$rows_affected) != 1) "s" else "",
            ": ", paste(na_check$rows_affected, collapse = ", ")
          )
        )
      )
    }
  }

  reference_dates <- as.Date(rownames(data))
  delays_unit <- get_delays_unit(data)

  .validate_rep_tri_args(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    delays_unit = delays_unit
  )

  return(invisible(data))
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @return NULL
#' @family reporting_triangle
#' @export
assert_reporting_triangle <- function(data) {
  validate_reporting_triangle(data)
  return(NULL)
}

#' Check if an object is a reporting_triangle
#'
#' @param x An object to check.
#' @return Logical indicating whether the object is a reporting_triangle.
#' @family reporting_triangle
#' @export
is_reporting_triangle <- function(x) {
  return(inherits(x, "reporting_triangle"))
}

#' Update reporting_triangle with new matrix data
#'
#' Internal helper to create a new reporting_triangle from modified matrix data
#' while preserving the original object's metadata (reference dates, delays_unit).
#' This simplifies the pattern of converting to matrix, operating on it, then
#' restoring the reporting_triangle class and attributes.
#'
#' @param reporting_triangle The original [reporting_triangle] object.
#' @param new_matrix The modified matrix data.
#' @return A new [reporting_triangle] object with the updated matrix data.
#' @keywords internal
.update_triangle_matrix <- function(reporting_triangle, new_matrix) {
  new_reporting_triangle(
    reporting_triangle_matrix = new_matrix,
    reference_dates = get_reference_dates(reporting_triangle),
    delays_unit = get_delays_unit(reporting_triangle)
  )
}
