#' Validate triangle
#' Various checks to make sure that the reporting triangle passed in to
#'   [get_delay_estimate()] is formatted properly.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @inheritParams get_delay_estimate
#' @returns NULL, invisibly
#' @keywords internal
.validate_triangle <- function(triangle,
                               max_delay = ncol(triangle) - 1,
                               n_history = nrow(triangle)) {
  # Make sure the input triangle only contains integer values
  # and is of the correct class
  assert_class(triangle, "matrix")
  assert_integerish(triangle)
  assert_integerish(max_delay)
  assert_integerish(n_history)
  assert_matrix(triangle, all.missing = FALSE)

  if (nrow(triangle) <= ncol(triangle)) {
    cli_abort(
      message =
        "Number of observations must be greater than the maximum delay"
    )
  }

  if (nrow(triangle) < n_history) {
    cli_abort(
      message = c(
        "Number of observations in input data not sufficient for",
        "user specified number of historical observations to use",
        "for estimaton."
      )
    )
  }

  if (ncol(triangle) < (max_delay + 1)) {
    cli_abort(
      message = c(
        "Number of delays in input data not sufficient for",
        "user specified maximum delay"
      )
    )
  }

  if ((max_delay < 1 || n_history < 1)) {
    cli_abort(
      message = c(
        "Insufficient `max_delay` or `n_history`, must be greater than or",
        " equal to 1."
      )
    )
  }
}

#' Validate triangle to nowcast and delay pmf together
#' Various checks to make sure that the reporting triangle  and the delay pmf
#'   passed in to [apply_delay()] are formatted properly and compatible
#' @param triangle Matrix of values with rows indicating the time points and
#'   columns indicating the delays
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @returns NULL, invisibly
#' @keywords internal
.validate_delay_and_triangle <- function(triangle,
                                         delay_pmf) {
  # Check that the input triangle only contains integer values
  assert_integerish(triangle)
  # Check that the inputs are the correct type
  assert_class(triangle, "matrix")
  assert_class(delay_pmf, "numeric")
  assert_matrix(triangle, all.missing = FALSE)


  # Make sure the triangle has the same number of colums as the delay
  if ((ncol(triangle) != length(delay_pmf))) {
    cli_abort(
      message = c(
        "Length of the delay pmf is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay pmf using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }
}

#' Validate the triangle used to estimate uncertainty and the specified
#'   inputs together, as only certain combinations are valid
#'
#' @param triangle_for_uncertainty Matrix of values with rows indicating the
#'   time points and columns indicating the delays. Specifically, the triangle
#'   used to generate the uncertainty estimates.
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay
#' @param n_history_dispersion Integer indicating the number of observations
#'   to be used to estimate the dispersion parameters in the observation model
#' @param n_history Integer indicating the number of observations to be used to
#'   estimate the delay distribution in order to generate retrospective
#'   nowcasts
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_integerish
#' @returns NULL, invisibly
#' @keywords internal
.validate_uncertainty_inputs <- function(triangle_for_uncertainty,
                                         delay_pmf,
                                         n_history_dispersion,
                                         n_history) {
  if (is.null(n_history_dispersion)) {
    cli_abort(
      message = c(
        "User must specify the number of observation to estimate ",
        "dispersion via `n_history_dispersion`"
      )
    )
  }
  if (n_history_dispersion < 1) {
    cli_abort(
      message = c(
        "Number of observations to use to estimate dispersion, ",
        "`n_history_dispersion`, must be greater than one."
      )
    )
  }

  if ((n_history_dispersion + n_history) > nrow(triangle_for_uncertainty)) {
    cli_abort(
      message = c(
        "Insufficient rows in reporting triangle for specified number ",
        "of observations used to estimate dispersion and delay pmf"
      )
    )
  }

  assert_integerish(n_history)
  assert_integerish(n_history_dispersion)

  # Case where you are not providing delay_pmf and n_history_dispersion +
  # n_history < nrow(triangle_for_uncertainty
  if (nrow(triangle_for_uncertainty) < (n_history + n_history_dispersion) &
    is.null(delay_pmf)) {
    cli_abort(
      message = c(
        "Reporting triangle to estimate uncertainty does not contain ",
        "sufficient observations to use `n_history_dispersion` observations ",
        "to recompute a delay distribution using `n_history` observations. ",
        "User must either pass in longer historical data or adjust the number ",
        "of observations used for either estimating the delay or estimating ",
        "the dispersion."
      )
    )
  }

  if (ncol(triangle_for_uncertainty) < n_history &&
    is.null(delay_pmf)) {
    cli_abort(
      message = c(
        "Number of observations to estimate delay from must be greater than",
        "the maximum delay."
      )
    )
  }

  # Message about which delay not being re-estimated
  if (!is.null(delay_pmf)) {
    message(
      "The delay distribution specified will be used to compute ",
      "retrospective nowcasts. `n_history` is not being used, because the ",
      "delay distribution is not being re-estimated at each iteration."
    )
  }

  # Message about delay being re-estimated
  if (is.null(delay_pmf)) {
    message(
      "No delay distribution was specified, therefore the delay ",
      "distribution will be re-estimated at each `n_history_dispersion` ",
      "reporting triangle."
    )
  }
}
