#' Apply the delay to generate a point nowcast
#' @description
#' This function takes as an input the reporting triangle that we want to
#' complete with a nowcast and a delay pmf and generates a point estimate of a
#' completed reporting square (or rectangle). This code is based on the code
#' originally developed by the Karlsruhe Institute of Technology RESPINOW
#' German Hospitalization Nowcasting Hub,
#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#'
#'
#' @param triangle_to_nowcast matrix of the incomplete reporting triangle to be
#' nowcasted, with rows representing the time points of reference and columns
#' representing the delays
#' @param delay_pmf vector of delays assumed to be indexed starting at the
#' first delay column in `triangle_to_nowcast`
#'
#' @return expectation, a matrix of the same number of rows and columns as the
#' `triangle_to_nowcast` but with the missing values filled in as point
#' estimates
#' @export
#'
#' @examples
#' library(epinowcast)
#' nat_germany_hosp <-
#'   germany_covid19_hosp[location == "DE"][age_group == "00+"]
#' nat_germany_hosp <- enw_filter_report_dates(
#'   nat_germany_hosp,
#'   latest_date = "2021-10-01"
#' )
#' pobs <- enw_preprocess_data(nat_germany_hosp, max_delay = 21)
#' triangle_raw <- pobs$reporting_triangle[[1]] |>
#'   dplyr::select(-`.group`, -reference_date) |>
#'   as.matrix() |>
#'   unname()
#' delay_df <- get_delay_estimate(
#'   triangle = triangle_raw,
#'   max_delay = 20,
#'   n_history = 30
#' )
#' reporting_square <- apply_delay(
#'   triangle_to_nowcast = triangle_raw,
#'   delay_pmf = delay_df$pmf
#' )
apply_delay <- function(triangle_to_nowcast,
                        delay_pmf) {
  # Checks that the delay df and the triangle are compatible
  validate_delay_and_triangle(
    triangle_to_nowcast,
    delay_pmf
  )
  n_delays <- length(delay_pmf)
  n_dates <- nrow(triangle_to_nowcast)
  expectation <- triangle_to_nowcast

  # Iterates through each column and adds entries to the expected reporting
  # triangle
  expectation <- Reduce(function(acc, co) {
    calc_expectation(co, acc, n_dates, delay_pmf)
  }, 2:n_delays, init = triangle_to_nowcast)


  return(expectation)
}

#' Calculate the updates rows of the expected nowcasted triangle
#'
#' @param co An integer indicating the column index
#' @param expectation A matrix of the partially complete reporting triangle
#' @param n_dates An integer indicating the number of dates in the reporting
#' triangle (number of rows in the reporting triangle)
#' @param delay_pmf A vector specifying the probability of a case being
#' reported with delay d
#'
#' @returns a matrix with another set of entries corresponding to the updated
#' values for the specified rows and column
calc_expectation <- function(co, expectation, n_dates, delay_pmf) {
  block_bottom_left <- expectation[(n_dates - co + 2):n_dates, 1:(co - 1), drop = FALSE] # nolint
  exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])
  expectation[(n_dates - co + 2):n_dates, co] <- exp_total * delay_pmf[co]
  return(expectation)
}
