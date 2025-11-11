#' Estimate a delay distribution from a reporting triangle
#'
#' @description Provides an estimate of the reporting delay as a function
#'   of the delay, based on the reporting triangle and the number of reference
#'   date observations to be used in the estimation. This point estimate of the
#'   delay is computed empirically, using an iterative algorithm starting from
#'   the most recent observations. Use [truncate_to_delay()] if you want to
#'   limit the maximum delay before estimation.
#'   This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW
#'   German Hospitalization Nowcasting Hub.
#'   Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#' @param reporting_triangle A [reporting_triangle] object with rows
#'   representing reference times and columns representing delays.
#'   Can be a reporting matrix or incomplete reporting matrix.
#'   Can also be a ragged reporting triangle, where multiple columns are
#'   reported for the same row (e.g., weekly reporting of daily data).
#' @param n Integer indicating the number of reference times (observations) to
#'   be used in the estimate of the reporting delay, always starting from the
#'   most recent reporting delay. The default is to use the whole reporting
#'   triangle, so `nrow(reporting_triangle)`.
#' @param preprocess Function to apply to the truncated triangle before
#'   estimation, or NULL to skip preprocessing. Default is
#'   [preprocess_negative_values()], which handles negative values by
#'   redistributing them to earlier delays. Set to NULL if you want to preserve
#'   negative PMF entries (e.g., when working with downward corrections where
#'   negative probabilities reflect systematic adjustments).
#' @returns Vector indexed at 0 of length `ncol(reporting_triangle)` with
#'   columns indicating the point estimate of the empirical probability
#'   mass on each delay.
#' @importFrom cli cli_warn
#' @family estimate_delay
#' @export
#' @examples
#' # Example 1: Standard usage with default preprocessing
#' delay_pmf <- estimate_delay(
#'   reporting_triangle = example_reporting_triangle,
#'   n = 3
#' )
#' delay_pmf
#'
#' # Example 2: Using data with downward corrections without preprocessing
#' # This preserves negative PMF entries reflecting systematic corrections
#' triangle_ex2 <- example_downward_corr_rt
#' delay_pmf_negative <- estimate_delay(
#'   reporting_triangle = triangle_ex2,
#'   n = 5,
#'   preprocess = NULL
#' )
#' delay_pmf_negative
#' # Note: PMF may contain negative values and not sum to 1
#' sum(delay_pmf_negative)
estimate_delay <- function(
    reporting_triangle,
    n = nrow(reporting_triangle),
    preprocess = preprocess_negative_values) {
  assert_reporting_triangle(reporting_triangle)

  # Check that the input reporting triangle is formatted properly.
  .validate_triangle(
    triangle = reporting_triangle,
    n = n
  )
  # Filter the reporting_triangle down to relevant rows and columns
  trunc_triangle <- .prepare_triangle(
    reporting_triangle,
    n,
    preprocess = preprocess
  )

  # Fill in missing values in the triangle
  expectation <- .chainladder_fill_triangle(trunc_triangle)

  # Calculate probability mass function from filled triangle
  pmf <- .calculate_pmf(expectation)

  return(pmf)
}


#' Prepare the triangle by truncating and optionally preprocessing
#'
#' @param reporting_triangle The original reporting triangle
#' @param n Number of reference times to use
#' @param preprocess Function to apply to the truncated triangle before
#'   estimation, or NULL to skip preprocessing. Default is
#'   [preprocess_negative_values()].
#' @return Prepared reporting triangle
#' @noRd
.prepare_triangle <- function(reporting_triangle, n,
                              preprocess = preprocess_negative_values) {
  nr0 <- nrow(reporting_triangle)
  # Convert to plain matrix for subsetting
  trunc_triangle <- as.matrix(reporting_triangle)[
    (nr0 - n + 1):nr0, , drop = FALSE
  ]

  # Apply preprocessing if provided
  if (!is.null(preprocess)) {
    rep_tri <- preprocess(trunc_triangle)
  } else {
    rep_tri <- trunc_triangle
  }

  return(rep_tri)
}

#' Fill in missing values in the reporting triangle using the iterative
#'    "chainladder" method
#'
#' @param rep_tri Prepared reporting triangle
#' @return Matrix with imputed values for missing entries
#' @noRd
.chainladder_fill_triangle <- function(rep_tri) {
  # Convert to plain matrix for internal operations
  rep_tri_mat <- as.matrix(rep_tri)
  n_delays <- ncol(rep_tri_mat)
  n_dates <- nrow(rep_tri_mat)
  expectation <- rep_tri_mat

  # Find the column to start filling in
  start_col <- which(colSums(is.na(rep_tri_mat)) > 0)[1]

  # Only fill in reporting triangle if it is incomplete
  if (!is.na(start_col)) {
    for (co in start_col:n_delays) {
      start_row <- which(is.na(rep_tri_mat[, co]))[1]
      # Extract relevant blocks of the triangle
      block_top_left <- .extract_block_top_left(
        rep_tri_mat, co, n_dates, start_row
      )
      block_top <- .extract_block_top(rep_tri_mat, co, n_dates, start_row)

      # Calculate multiplication factor
      mult_factor <- .calculate_mult_factor(block_top, block_top_left)

      # Extract block bottom left
      block_bottom_left <- .extract_block_bottom_left(
        expectation,
        co,
        n_dates,
        start_row
      )

      # Compute expectations for bottom right
      expectation[start_row:n_dates, co] <- .compute_expectations(
        mult_factor,
        block_bottom_left
      )
    }
  }

  return(expectation)
}

#' Extract the top left block of the triangle
#'
#' @param rep_tri Reporting triangle
#' @param co Current column
#' @param n_dates Number of dates
#' @param start_row Starting row
#' @return Top left block of the triangle
#' @noRd
.extract_block_top_left <- function(rep_tri, co, n_dates, start_row) {
  return(rep_tri[1:(start_row - 1), 1:(co - 1), drop = FALSE])
}

#' Extract the top block of the triangle
#'
#' @inheritParams .extract_block_top_left
#' @return Top block of the triangle
#' @noRd
.extract_block_top <- function(rep_tri, co, n_dates, start_row) {
  return(rep_tri[1:(start_row - 1), co, drop = FALSE])
}

#' Extract the bottom left block of the triangle
#'
#' @param expectation Expectation matrix
#' @param co Current column
#' @inheritParams .extract_block_top_left
#' @return Bottom left block of the triangle
#' @noRd
.extract_block_bottom_left <- function(expectation, co, n_dates, start_row) {
  return(expectation[start_row:n_dates, 1:(co - 1), drop = FALSE])
}

#' Calculate multiplication factor
#'
#' @param block_top Top block
#' @param block_top_left Top left block
#' @return Multiplication factor
#' @noRd
.calculate_mult_factor <- function(block_top, block_top_left) {
  return(sum(block_top) / max(sum(block_top_left), 1))
}

#' Compute expectations for the bottom right part
#'
#' @param mult_factor Multiplication factor
#' @param block_bottom_left Bottom left block
#' @return Vector of expectations
#' @noRd
.compute_expectations <- function(mult_factor, block_bottom_left) {
  return(mult_factor * rowSums(block_bottom_left))
}

#' Calculate the probability mass function from the filled triangle
#'
#' @param expectation Filled triangle matrix
#' @return Probability mass function
#' @noRd
.calculate_pmf <- function(expectation) {
  return(colSums(expectation) / sum(expectation))
}
