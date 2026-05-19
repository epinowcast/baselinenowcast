#' Reporting Triangle DataFrame Object
#' @name reporting_triangle_df-class
#' @aliases reporting_triangle_df
#' @family reporting_triangle_df
#'
#' @description
#' A `reporting_triangle_df` object contains nowcasting data and metadata in long
#' (data.frame) format, supporting both single and multiple strata.
#'
#' @section Structure:
#' A `reporting_triangle_df` is a data.frame with class
#' `c("reporting_triangle_df", "data.frame")`:
#'
#' **Required columns:**
#' - `reference_date`: Date of primary event occurrence
#' - `report_date`: Date of event report
#' - `count`: Numeric/Integer count of cases
#' - Strata columns (e.g., `age_group`, `location`) - optional
#'
#' **Computed on-the-fly (not stored):**
#' - `delay`: Computed from `report_date - reference_date` using `delays_unit`
#' - `max_delay`: Computed as `max(delay)` when needed
#'
#' **Attributes:**
#' - `delays_unit`: Character - "days", "weeks", "months", or "years"
#' - `strata`: Character vector of strata column names (NULL if no strata)
#'
#' @section Conversion:
#' Use [as_reporting_triangle()] to convert a single-stratum
#' `reporting_triangle_df` to [reporting_triangle]. For multiple strata, use
#' [as_reporting_triangles()] to get a named list of [reporting_triangle]
#' objects.
#'
#' @examples
#' # Create a reporting_triangle_df from data
#' data <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rt_df <- as_reporting_triangle_df(data)
#'
#' # With strata
#' rt_df_strata <- as_reporting_triangle_df(
#'   germany_covid19_hosp,
#'   by = c("age_group", "location")
#' )
#'
#' # Get list of triangles for multiple strata
#' rt_list <- as_reporting_triangles(rt_df_strata)
NULL

#' Class constructor for `reporting_triangle_df` objects
#'
#' Creates a new reporting_triangle_df object from a data.frame.
#'
#' @param data Data.frame with required columns: reference_date, report_date,
#'   count, and optionally strata columns.
#' @param delays_unit Character string specifying the temporal granularity.
#'   Options: "days", "weeks", "months", "years".
#' @param strata Character vector of strata column names. NULL if no strata.
#'
#' @returns An object of class [reporting_triangle_df]
#' @family reporting_triangle_df
#' @export
new_reporting_triangle_df <- function(data, delays_unit, strata = NULL) {
  # Validate inputs
  assert_data_frame(data)
  assert_delays_unit(delays_unit)

  if (!is.null(strata)) {
    assert_character(strata, min.len = 1)
  }

  # Set class and attributes
  result <- structure(
    data,
    class = c("reporting_triangle_df", "data.frame"),
    delays_unit = delays_unit,
    strata = strata
  )

  return(result)
}

#' Validate a reporting_triangle_df object
#'
#' @param data A [reporting_triangle_df] object to validate
#' @return The validated object (invisibly) or throws error
#' @family reporting_triangle_df
#' @export
#' @importFrom checkmate assert_data_frame assert_date assert_numeric
#'    assert_character
validate_reporting_triangle_df <- function(data) {
  if (!inherits(data, "data.frame")) {
    cli_abort(message = "data must be a data.frame")
  }
  if (!inherits(data, "reporting_triangle_df")) {
    cli_abort(message = "data must have class 'reporting_triangle_df'")
  }

  # Check required columns
  required_cols <- c("reference_date", "report_date", "count")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli_abort(
      message = c(
        "Required columns missing from data",
        "x" = "Missing: {.val {missing_cols}}"
      )
    )
  }

  # Validate column types
  assert_date(data$reference_date)
  assert_date(data$report_date)
  assert_numeric(data$count)

  # Validate attributes
  delays_unit <- attr(data, "delays_unit")
  strata <- attr(data, "strata")

  assert_delays_unit(delays_unit)

  if (!is.null(strata)) {
    assert_character(strata, min.len = 1)

    # Check that strata columns exist
    missing_strata_cols <- setdiff(strata, names(data))
    if (length(missing_strata_cols) > 0) {
      cli_abort(
        message = c(
          "Strata columns missing from data",
          "x" = "Missing: {.val {missing_strata_cols}}"
        )
      )
    }
  }

  # Check for duplicates within strata
  # Use unclass to avoid triggering the subsetting method
  data_plain <- as.data.frame(unclass(data))

  if (!is.null(strata)) {
    # Check for duplicates within each stratum
    check_cols <- c(strata, "reference_date", "report_date")
  } else {
    # Check for duplicates when no strata
    check_cols <- c("reference_date", "report_date")
  }

  dup_pairs <- duplicated(data_plain[, check_cols])
  if (any(dup_pairs)) {
    cli_abort(
      message = c(
        "Data contains duplicate combinations",
        "x" = "Found {sum(dup_pairs)} duplicate pair{?s}",
        "i" = "Each combination of strata, reference_date, and report_date should appear only once"
      )
    )
  }

  return(invisible(data))
}

#' Assert validity of `reporting_triangle_df` objects
#'
#' @param data A [reporting_triangle_df] object to check for validity.
#' @param validate Logical. If TRUE (default), validates the object. Set to
#'   FALSE only when called from functions that already validated.
#' @return NULL
#' @family reporting_triangle_df
#' @export
assert_reporting_triangle_df <- function(data, validate = TRUE) {
  if (isTRUE(validate)) {
    validate_reporting_triangle_df(data)
  }
  return(NULL)
}

#' Check if an object is a reporting_triangle_df
#'
#' @param x An object to check.
#' @return Logical indicating whether the object is a reporting_triangle_df.
#' @family reporting_triangle_df
#' @export
is_reporting_triangle_df <- function(x) {
  return(inherits(x, "reporting_triangle_df"))
}
