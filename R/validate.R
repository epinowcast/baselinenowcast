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
#' Various checks to make sure that the reporting triangle  and the delay PMF
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
        "Length of the delay PMF is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay PMF using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }
}
