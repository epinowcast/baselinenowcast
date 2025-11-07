# Custom Expectations for baselinenowcast Tests
#
# This file contains custom expectation functions that centralize error
# messages, warnings, and structure validation logic used across test files.
# By defining these in one place, we ensure consistency and make maintenance
# easier when error messages or validation logic changes.

# Error Expectations -------------------------------------------------------

#' Expect error: Invalid strata required columns
#' @keywords internal
expect_err_strata_required <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "`strata_cols` cannot contain any of the required columns"
  )))
}

#' Expect error: Invalid strata missing columns
#' @keywords internal
expect_err_strata_missing <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "`strata_cols`, if specified, must be a column in `data`"
  )))
}

#' Expect error: No overlapping dates
#' @keywords internal
expect_error_no_overlap <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = paste0(
      "There is no overlapping set of reference and report dates ",
      "across all"
    )
  )))
}

#' Expect error: Duplicate dates
#' @keywords internal
expect_error_duplicate_dates <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = paste0(
      "Data contains duplicate `reference_date` and `report_date`"
    )
  )))
}

#' Expect error: Strata sharing conflict
#' @keywords internal
expect_err_strata_sharing <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = paste0(
      "`strata_sharing` cannot be both 'none' and 'delay'/'uncertainty'"
    )
  )))
}

#' Expect error: Missing names
#' @keywords internal
expect_error_missing_names <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "Names must include the elements"
  )))
}

#' Expect error: Wrong date class
#' @keywords internal
expect_error_wrong_date_class <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "Must be of class 'Date'"
  )))
}

#' Expect error: Invalid triangle dimensions
#' @keywords internal
expect_error_invalid_dims <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "The number of rows .* does not match"
  )))
}

#' Expect error: Negative values
#' @keywords internal
expect_error_negative_values <- function(object) {
  return(invisible(testthat::expect_error(
    object,
    regexp = "must be positive|negative values"
  )))
}

# Warning Expectations -----------------------------------------------------

#' Expect warning: Partial overlap
#' @keywords internal
expect_warning_partial_overlap <- function(object) {
  return(invisible(testthat::expect_warning(
    object,
    regexp = paste0(
      "Not all reference dates and report dates combinations are ",
      "available for all"
    )
  )))
}

# Structure Expectations ---------------------------------------------------

#' Expect baselinenowcast output structure
#'
#' @param object Object to test
#' @param expected_cols Expected column names
#' @param output_type Expected output_type value (NULL to skip check)
#' @return Invisibly returns object for piping
#' @keywords internal
expect_blnc_structure <- function(object,
                                             expected_cols,
                                             output_type = "samples") {
  testthat::expect_s3_class(object, "data.frame")
  testthat::expect_s3_class(object, "baselinenowcast_df")
  testthat::expect_true(all(expected_cols %in% colnames(object)))
  if (!is.null(output_type)) {
    testthat::expect_identical(object$output_type[1], output_type)
  }
  return(invisible(object))
}

#' Expect columns are present
#' @keywords internal
expect_columns_present <- function(object, cols) {
  testthat::expect_true(all(cols %in% colnames(object)))
  return(invisible(object))
}

#' Expect columns are absent
#' @keywords internal
expect_columns_absent <- function(object, cols) {
  for (col in cols) {
    testthat::expect_false(col %in% colnames(object))
  }
  return(invisible(object))
}

#' Expect list structure
#' @keywords internal
expect_list_structure <- function(object,
                                  expected_length,
                                  expected_names = NULL) {
  testthat::expect_type(object, "list")
  testthat::expect_length(object, expected_length)
  if (!is.null(expected_names)) {
    testthat::expect_named(object, expected_names)
  }
  return(invisible(object))
}
