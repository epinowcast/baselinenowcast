#' Apply the delay to generate a point nowcast
#'
#' Generate a point estimate of a completed reporting square (or rectangle)
#'   from a reporting triangle that we want to complete with a nowcast and a
#'   delay PMF. Each element is computed by taking the product of the expected
#'   number of total cases assigned to a reference time $t$ and the proportion
#'   of those cases reported on delay $d$. The formula to obtain the expected
#'   number of total cases as a function of the reporting delay and previous
#'   observations was derived elsewhere.
#'   This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW
#'   German Hospitalization Nowcasting Hub.
#'   Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#' @param reporting_triangle Matrix of the reporting triangle to be
#'   nowcasted, with rows representing the time points of reference and columns
#'   representing the delays
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `reporting_triangle`.
#' @return `point_nowcast_matrix` Matrix of the same number of rows and columns
#'    as the `rep_mat_to_nowcast` but with the missing values filled in as point
#'    estimates
#' @family generate_point_nowcasts
#' @export
#' @examples
#' # Example 1: Standard usage with positive delay PMF
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
#' delay_pmf <- estimate_delay(
#'   reporting_triangle = triangle,
#'   max_delay = 3,
#'   n = 4
#' )
#' point_nowcast_matrix <- apply_delay(
#'   reporting_triangle = triangle,
#'   delay_pmf = delay_pmf
#' )
#' print(point_nowcast_matrix)
#'
#' # Example 2: Using delay PMF with negative entries from downward
#' # corrections. Create a delay PMF with a negative value representing
#' # systematic corrections
#' delay_pmf_negative <- c(0.7, 0.4, -0.15, 0.05)
#'
#' nowcast_with_corrections <- apply_delay(
#'   reporting_triangle = example_downward_corr_mat,
#'   delay_pmf = delay_pmf_negative
#' )
#' # The nowcast includes negative predictions at delay 2,
#' # correctly reflecting expected downward corrections
#' print(nowcast_with_corrections)
apply_delay <- function(reporting_triangle, delay_pmf) {
  # Checks that the delay df and the triangle are compatible
  .validate_delay_and_triangle(
    reporting_triangle,
    delay_pmf
  )
  n_delays <- length(delay_pmf)
  n_rows <- nrow(reporting_triangle)

  n_row_nas <- sum(is.na(rowSums(reporting_triangle)))
  if (n_row_nas == 0) {
    cli_abort(
      message = c("`reporting_triangle` doesn't contain any missing values, there is nothing to nowcast.", # nolint
        "i" = "Check to make sure missing observations are coded as NAs rather than 0s.", # nolint
        "i" = "If performing nowcasts retrospectively, report times after the last reference date should not be available.", # nolint
        "i" = "See `?construct_triangle` for more details on creating a reporting triangle from a complete set of observations." # nolint
      )
    )
  }

  # Precompute CDFs for the delay PMF
  delay_cdf <- cumsum(delay_pmf)

  # Iterates through each column (delay) and adds entries to the reporting
  # matrix to nowcast
  point_nowcast_matrix <- Reduce(
    function(acc, index) {
      return(.calc_expectation(
        index,
        acc,
        delay_pmf[index],
        delay_cdf[index - 1],
        n_rows
      ))
    },
    2:n_delays,
    init = reporting_triangle
  )
  return(point_nowcast_matrix)
}

#' Calculate the updated rows of the expected nowcasted triangle
#'
#' @param index Integer indicating the delay index
#' @param expectation Matrix of the incomplete reporting matrix
#' @param delay_prob Scalar probability for the current delay
#' @param delay_cdf_prev Scalar cumulative probability up to previous delay
#' @param n_rows Number of rows in the expectation matrix
#' @returns Matrix with another set of entries corresponding to the updated
#'   values for the specified rows and column
#' @keywords internal
.calc_expectation <- function(
    index,
    expectation,
    delay_prob,
    delay_cdf_prev,
    n_rows) {
  # Find rows with NA in this column that need to be filled
  na_rows <- .where_is_na_in_col(expectation, index)

  if (length(na_rows) == 0) {
    return(expectation)
  }

  # Start with the first row that has NA
  row_start <- min(na_rows)

  # Extract the left block for these rows
  block_bottom_left <- .extract_block_bottom_left(
    expectation,
    index,
    n_rows,
    row_start
  )

  # Calculate row sums for the extracted block
  x <- rowSums(block_bottom_left)

  # Calculate expectations with support for zero values
  exp_N <- .calc_modified_expectation(x, delay_cdf_prev)

  # Update only the NA rows in the column
  expectation[row_start:n_rows, index] <- exp_N * delay_prob

  return(expectation)
}

.where_is_na_in_col <- function(expectation, co) {
  return(which(is.na(expectation[, co])))
}

.calc_modified_expectation <- function(x, delay_cdf_prev) {
  return((x + 1 - delay_cdf_prev) / delay_cdf_prev)
}
