#' Apply the delay to generate a point nowcast
#' @description
#' Generate a point estimate of a completed reporting square (or rectangle)
#'  from a reporting triangle that we want to complete with a nowcast and a
#'  delay pmf. This code was adapted from code written (under an MIT license)
#'  by the Karlsruhe Institute of Technology RESPINOW
#'  German Hospitalization Nowcasting Hub.
#'  Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#' @param triangle_to_nowcast Matrix of the incomplete reporting triangle to be
#'   nowcasted, with rows representing the time points of reference and columns
#'   representing the delays
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `triangle_to_nowcast`
#' @return expectation, matrix of the same number of rows and columns as the
#'   `triangle_to_nowcast` but with the missing values filled in as point
#'   estimates
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
#' delay_df <- get_delay_estimate(
#'   triangle = triangle,
#'   max_delay = 3,
#'   n_history = 4
#' )
#' reporting_square <- apply_delay(
#'   triangle_to_nowcast = triangle,
#'   delay_pmf = delay_df$pmf
#' )
#' print(reporting_square)
apply_delay <- function(triangle_to_nowcast,
                        delay_pmf) {
  # Checks that the delay df and the triangle are compatible
  .validate_delay_and_triangle(
    triangle_to_nowcast,
    delay_pmf
  )
  n_delays <- length(delay_pmf)
  n_dates <- nrow(triangle_to_nowcast)

  # Iterates through each column and adds entries to the expected reporting
  # triangle
  expectation <- Reduce(function(acc, co) {
    .calc_expectation(co, acc, n_dates, delay_pmf)
  }, 2:n_delays, init = triangle_to_nowcast)
  return(expectation)
}

#' Calculate the updates rows of the expected nowcasted triangle
#'
#' @param co Integer indicating the column index
#' @param expectation Matrix of the partially complete reporting triangle
#' @param n_dates Integer indicating the number of dates in the reporting
#'   triangle (number of rows in the reporting triangle)
#' @param delay_pmf Vector specifying the probability of a case being
#'   reported with delay d
#'
#' @returns Matrix with another set of entries corresponding to the updated
#'   values for the specified rows and column
#' @keywords internal
.calc_expectation <- function(co, expectation, n_dates, delay_pmf) {
  block_bottom_left <- expectation[
    (n_dates - co + 2):n_dates,
    1:(co - 1),
    drop = FALSE
  ]

  exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])
  expectation[(n_dates - co + 2):n_dates, co] <- exp_total * delay_pmf[co]
  return(expectation)
}
