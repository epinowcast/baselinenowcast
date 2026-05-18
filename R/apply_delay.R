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
#'   Modified from: \url{https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55} #nolint
#' @param reporting_triangle Matrix of the reporting triangle to be
#'   nowcasted, with rows representing the time points of reference and columns
#'   representing the delays
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `reporting_triangle`.
#' @inheritParams assert_reporting_triangle
#' @return `point_nowcast_matrix` Matrix of the same number of rows and columns
#'    as the `rep_mat_to_nowcast` but with the missing values filled in as point
#'    estimates
#' @family generate_point_nowcasts
#' @export
#' @examples
#' # Example 1: Standard usage with example dataset
#' delay_pmf <- estimate_delay(example_reporting_triangle)
#' point_nowcast_matrix <- apply_delay(
#'   reporting_triangle = example_reporting_triangle,
#'   delay_pmf = delay_pmf
#' )
#' print(point_nowcast_matrix)
#'
#' # Example 2: Using delay PMF with negative entries from downward corrections
#' delay_pmf_negative <- c(0.7, 0.4, -0.15, 0.05)
#' nowcast_with_corrections <- apply_delay(
#'   reporting_triangle = example_downward_corr_rt,
#'   delay_pmf = delay_pmf_negative
#' )
#' # The nowcast includes negative predictions at delay 2,
#' # correctly reflecting expected downward corrections
#' print(nowcast_with_corrections)
apply_delay <- function(reporting_triangle, delay_pmf, validate = TRUE) {
  assert_reporting_triangle(reporting_triangle, validate)

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

  # Convert to plain matrix for efficiency in Reduce iterations
  # (avoids repeated validation/attribute checks)
  init_matrix <- as.matrix(reporting_triangle)

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
    init = init_matrix
  )

  # Preserve reporting_triangle class and attributes
  point_nowcast_matrix <- .update_triangle_matrix(
    reporting_triangle,
    point_nowcast_matrix
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
  n_rows
) {
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

#' Validate triangle to nowcast and delay PMF together
#'
#' Various checks to make sure that the reporting triangle  and the delay PMF
#'   passed in to [apply_delay()] are formatted properly and compatible.
#' @param triangle Matrix of values with rows indicating the time points and
#'   columns indicating the delays.
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay.
#' @importFrom checkmate assert_class assert_integerish assert_matrix
#' @importFrom cli cli_abort
#' @returns NULL, invisibly
#' @keywords internal
.validate_delay_and_triangle <- function(triangle, delay_pmf) {
  assert_class(triangle, "matrix")
  assert_class(delay_pmf, "numeric")
  assert_matrix(triangle, all.missing = FALSE)

  if ((ncol(triangle) != length(delay_pmf))) {
    cli_abort(
      message = c(
        "Length of the delay PMF is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay PMF using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }

  if (is.na(triangle[nrow(triangle), 2]) && delay_pmf[1] == 0) {
    cli_abort(
      message = c(
        "Value of delay PMF at delay = 0 is 0, and the latest reference time ",
        "in the reporting matrix only contains a value at delay = 0. There is",
        "insufficient information to generate a point nowcast for the latest ",
        "reference time. Consider truncating to an earlier reference time to ",
        "ensure a nowcast, not a forecast, is being produced."
      )
    )
  }

  if (delay_pmf[1] < 0) {
    cli_abort(
      message = c(
        "x" = "First entry of delay PMF (delay = 0) is negative ({delay_pmf[1]}).", # nolint
        "i" = "Negative PMF entries are only valid for later delays where they represent systematic downward corrections.", # nolint
        "i" = "The first delay must have a non-negative probability as it represents the baseline reporting pattern." # nolint
      )
    )
  }

  return(NULL)
}

#' Validate the delay PMF if it is passed in
#'
#' @inheritParams .validate_delay_and_triangle
#' @inheritParams sample_prediction
#'
#' @returns NULL invisibly
#' @importFrom checkmate check_numeric
#' @keywords internal
.validate_delay <- function(triangle, delay_pmf) {
  pmf_sum <- sum(delay_pmf)
  test <- check_numeric(pmf_sum, lower = 0.99, upper = 1.01, len = 1)
  if (!isTRUE(test)) {
    cli_alert_info(
      "Delay PMF does not sum to approximately 1 (sum = {round(pmf_sum, 4)}). This may be expected when working with corrections or incomplete data, but could also indicate an error in the delay estimation." # nolint: line_length_linter
    )
  }

  n_delays <- ncol(triangle)
  if (n_delays != length(delay_pmf)) {
    cli_abort(
      message = c("`delay_pmf` is not the same length as the number of delays in the reporting triangle.") # nolint: line_length_linter
    )
  }
  return(NULL)
}
