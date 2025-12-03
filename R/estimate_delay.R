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
#' @inheritParams assert_reporting_triangle
#' @returns Vector indexed at 0 of length `ncol(reporting_triangle)` with
#'   columns indicating the point estimate of the empirical probability
#'   mass on each delay.
#' @importFrom cli cli_warn
#' @family estimate_delay
#' @export
#' @examples
#' # Example 1: Standard usage
#' delay_pmf <- estimate_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' delay_pmf
#'
#' # Example 2: Estimate delay using fewer observations
#' delay_pmf_ex2 <- estimate_delay(
#'   reporting_triangle = example_reporting_triangle,
#'   n = 5
#' )
#' delay_pmf_ex2
estimate_delay <- function(
    reporting_triangle,
    n = nrow(reporting_triangle),
    validate = TRUE) {
  assert_reporting_triangle(reporting_triangle, validate)

  # Check that the input reporting triangle is formatted properly.
  .validate_for_delay_estimation(
    triangle = reporting_triangle,
    n = n
  )

  # Truncate to last n rows
  trunc_triangle <- tail(reporting_triangle, n = n)

  # Convert to matrix for chainladder fill
  trunc_matrix <- as.matrix(trunc_triangle)

  # Fill in missing values in the triangle
  expectation <- .chainladder_fill_triangle(trunc_matrix)

  # Calculate probability mass function from filled triangle
  pmf <- .calculate_pmf(expectation)

  return(pmf)
}


#' Fill in missing values in the reporting triangle using the iterative
#'    "chainladder" method
#'
#' @param rep_tri_mat Matrix representation of reporting triangle
#' @return Matrix with imputed values for missing entries
#' @noRd
.chainladder_fill_triangle <- function(rep_tri_mat) {
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
