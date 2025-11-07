# Helper Functions for baselinenowcast Tests
#
# This file contains partial functions with good defaults and data
# manipulation helpers used across test files. By centralizing these
# patterns, we reduce boilerplate and make test intent clearer.

# Partial Functions --------------------------------------------------------

#' baselinenowcast with test defaults
#'
#' Wrapper around baselinenowcast with sensible defaults for testing.
#' Reduces boilerplate in tests while allowing easy override of parameters.
#'
#' @param data Input data
#' @param max_delay Maximum delay (default: 40)
#' @param draws Number of draws (default: 100)
#' @param ... Additional arguments passed to baselinenowcast
#' @return baselinenowcast output
#' @keywords internal
baselinenowcast_test <- function(data, max_delay = 40, draws = 100, ...) {
  baselinenowcast(
    data = data,
    max_delay = max_delay,
    draws = draws,
    ...
  )
}

# Data Manipulation Helpers ------------------------------------------------

#' Summarise mean for final day
#'
#' Common pattern for extracting mean predictions for the most recent
#' reference date, grouped by specified variables.
#'
#' @param df Data frame with reference_date and pred_count columns
#' @param group_vars Variables to group by (default: reference_date, age_group)
#' @return Data frame with mean_est column
#' @keywords internal
summarise_final_day_mean <- function(df, group_vars = c(
                                       "reference_date",
                                       "age_group"
                                     )) {
  df |>
    dplyr::filter(reference_date == max(reference_date)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(mean_est = mean(pred_count), .groups = "drop")
}

# Comparison Helpers -------------------------------------------------------

#' Expect estimates differ
#'
#' Asserts that two estimates are different (opposite of expect_equal).
#' Useful for testing that different parameters produce different results.
#'
#' @param est1 First estimate
#' @param est2 Second estimate
#' @param tol Tolerance for comparison
#' @keywords internal
expect_estimates_differ <- function(est1, est2, tol = 0.01) {
  expect_failure(
    expect_equal(est1, est2, tolerance = tol)
  )
}

#' Expect all values greater than threshold
#' @keywords internal
expect_all_greater_than <- function(vec, threshold) {
  expect_true(all(vec > threshold, na.rm = TRUE))
}

#' Expect all values less than threshold
#' @keywords internal
expect_all_less_than <- function(vec, threshold) {
  expect_true(all(vec < threshold, na.rm = TRUE))
}

# Validation Helpers -------------------------------------------------------

#' Validate triangle output matches input
#' @keywords internal
validate_triangle_output <- function(result, input_triangle) {
  expect_valid_matrix(result)
  expect_dimensions_match(result, input_triangle)
  invisible(result)
}

#' Validate nowcast draws structure
#' @keywords internal
validate_nowcast_draws <- function(nowcast_df, n_draws, n_dates) {
  expect_s3_class(nowcast_df, "data.frame")
  expect_true("draw" %in% colnames(nowcast_df))
  expect_length(unique(nowcast_df$draw), n_draws)
  invisible(nowcast_df)
}
