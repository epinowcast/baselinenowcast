#' Estimate a delay distribution from a reporting triangle
#' Provides an estimate of the reporting delay as a function
#'   of the delay, based on the reporting triangle and the specified maximum
#'   delay and number of reference date observations to be used in the
#'   estimation. This point estimate of the delay is computed empirically,
#'   using an iterative algorithm starting from the most recent observations.
#'   This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW
#'   German Hospitalization Nowcasting Hub.
#'   Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#' @param reporting_triangle Matrix of the reporting triangle, with rows
#'   representing the time points of reference and columns representing the
#'   delays. Can be a reporting matrix or incomplete reporting matrix.
#' @param max_delay Integer indicating the maximum delay to estimate, in units
#'   of the delay. The default is to use the whole reporting triangle,
#'   `ncol(triangle) -1`.
#' @param n Integer indicating the number of reference times (observations) to
#'   be used in the estimate of the reporting delay, always starting from the
#'   most recent reporting delay. The default is to use the whole reporting
#'   triangle, so `nrow(triangle)`.
#' @returns Vector indexed at 0 of length `max_delay + 1` with columns
#'   indicating the point estimate of the empirical probability
#'   mass on each delay.
#' @importFrom cli cli_warn
#' @export
#' @examples
#' triangle <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' delay_pmf <- get_delay_estimate(
#'   reporting_triangle = triangle,
#'   max_delay = 3,
#'   n = 4
#' )
#' print(delay_pmf)
get_delay_estimate <- function(
  reporting_triangle,
  max_delay = ncol(reporting_triangle) - 1,
  n = nrow(reporting_triangle)
) {
  # Check that the input reporting triangle is formatted properly.
  .validate_triangle(
    triangle = reporting_triangle,
    max_delay = max_delay,
    n = n
  )

  # Produce a warning if the bottom left of a reporting trianglei s all 0s.
  if (isTRUE(.check_zeros_bottom_right(reporting_triangle))) {
    cli_warn(
      message = c(
        "All entries in bottom right are 0. Are these true observations ",
        "with zero reports, or are these unobserved? If the latter, ",
        "replace with NA using the `replace_bottom_right_with_NA()` ",
        "function."
      )
    )
  }

  # Filter the reporting_triangle down to relevant rows and columns
  trunc_triangle <- .prepare_triangle(reporting_triangle, max_delay, n)

  # Fill in missing values in the triangle
  expectation <- .fill_triangle(trunc_triangle)

  # Calculate probability mass function from filled triangle
  pmf <- .calculate_pmf(expectation)

  return(pmf)
}

#' Prepare the triangle by truncating and handling negative values
#'
#' @param reporting_triangle The original reporting triangle
#' @param max_delay Maximum delay to consider
#' @param n Number of reference times to use
#' @return Prepared reporting triangle
#' @noRd
.prepare_triangle <- function(reporting_triangle, max_delay, n) {
  nr0 <- nrow(reporting_triangle)
  trunc_triangle <- reporting_triangle[(nr0 - n + 1):nr0, 1:(max_delay + 1)]
  rep_tri <- .handle_neg_vals(trunc_triangle)
  return(rep_tri)
}

#' Fill in missing values in the reporting triangle
#'
#' @param rep_tri Prepared reporting triangle
#' @return Matrix with imputed values for missing entries
#' @noRd
.fill_triangle <- function(rep_tri) {
  n_delays <- ncol(rep_tri)
  n_dates <- nrow(rep_tri)
  expectation <- rep_tri

  # Find the column to start filling in
  start_col <- which(colSums(is.na(rep_tri)) > 0)[1]

  # Only fill in reporting triangle if it is incomplete
  if (!is.na(start_col)) {
    for (co in start_col:n_delays) {
      # Extract relevant blocks of the triangle
      block_top_left <- .extract_block_top_left(rep_tri, co, n_dates)
      block_top <- .extract_block_top(rep_tri, co, n_dates)

      # Calculate multiplication factor
      mult_factor <- .calculate_mult_factor(block_top, block_top_left)

      # Extract block bottom left
      block_bottom_left <- .extract_block_bottom_left(expectation, co, n_dates)

      # Compute expectations for bottom right
      expectation[(n_dates - co + 2):n_dates, co] <- .compute_expectations(
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
#' @return Top left block of the triangle
#' @noRd
.extract_block_top_left <- function(rep_tri, co, n_dates) {
  return(rep_tri[1:(n_dates - co + 1), 1:(co - 1), drop = FALSE])
}

#' Extract the top block of the triangle
#'
#' @param rep_tri Reporting triangle
#' @param co Current column
#' @param n_dates Number of dates
#' @return Top block of the triangle
#' @noRd
.extract_block_top <- function(rep_tri, co, n_dates) {
  return(rep_tri[1:(n_dates - co + 1), co, drop = FALSE])
}

#' Extract the bottom left block of the triangle
#'
#' @param expectation Expectation matrix
#' @param co Current column
#' @param n_dates Number of dates
#' @return Bottom left block of the triangle
#' @noRd
.extract_block_bottom_left <- function(expectation, co, n_dates) {
  return(expectation[(n_dates - co + 2):n_dates, 1:(co - 1), drop = FALSE])
}

#' Calculate multiplication factor
#'
#' @param block_top Top block
#' @param block_top_left Top left block
#' @return Multiplication factor
#' @noRd
.calculate_mult_factor <- function(block_top, block_top_left) {
  sum(block_top) / max(sum(block_top_left), 1)
}

#' Compute expectations for the bottom right part
#'
#' @param mult_factor Multiplication factor
#' @param block_bottom_left Bottom left block
#' @return Vector of expectations
#' @noRd
.compute_expectations <- function(mult_factor, block_bottom_left) {
  mult_factor * rowSums(block_bottom_left)
}

#' Calculate the probability mass function from the filled triangle
#'
#' @param expectation Filled triangle matrix
#' @return Probability mass function
#' @noRd
.calculate_pmf <- function(expectation) {
  colSums(expectation) / sum(expectation)
}
