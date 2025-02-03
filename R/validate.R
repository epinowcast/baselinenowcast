#' Validate triangle
#' @description
#' Various checks to make sure that the reporting triangle passed in to
#' `estimate_delay()` is formatted properly.
#' @inheritParams get_delay_estimate
#'
#' @returns NULL, invisibly
validate_triangle <- function(triangle,
                              max_delay,
                              n_history) {
  # Make sure the input triangle only contains integer values
  # and is of the correct class
  checkmate::assert_class(triangle, "matrix")
  checkmate::assert_integerish(triangle)
  checkmate::assert_integerish(max_delay)
  checkmate::assert_integerish(n_history)

  if (nrow(triangle) <= ncol(triangle)) {
    cli::cli_abort(
      message = c(
        "Number of observations must be greater than the maximum",
        "delay"
      )
    )
  }

  if (nrow(triangle) < n_history) {
    cli::cli_abort(
      message = c(
        "Number of observations in input data not sufficient for",
        "user specified number of historical observations to use",
        "for estimaton."
      )
    )
  }

  if (ncol(triangle) < (max_delay + 1)) {
    cli::cli_abort(
      message = c(
        "Number of delays in input data not sufficient for",
        "user specified maximum delay"
      )
    )
  }

  if ((max_delay < 1 || n_history < 1)) {
    cli::cli_abort(
      message = c(
        "Insufficient `max_delay` or `n_history`, must be greater than or",
        " equal to 1."
      )
    )
  }
}

#' Validate triangle to nowcast and delay pmf together
#' @description
#' Various checks to make sure that the reporting triangle  and the delay pmf
#' passed in to `apply_delay()` are formatted properly and compaitble
#' @param triangle Matrix of values with rows indicating the time points and
#' columns indicating the delays
#' @param delay_pmf Vector of length of the number of delays indicating the
#' probability of a case being reportined on a given delay
#'
#' @returns NULL, invisibly
validate_delay_and_triangle <- function(triangle,
                                        delay_pmf) {
  # Check that the input triangle only contains integer values
  checkmate::assert_integerish(triangle)
  # Check that the inputs are the correct type
  checkmate::assert_class(triangle, "matrix")
  checkmate::assert_class(delay_pmf, "numeric")

  # Make sure the triangle has the same number of colums as the delay
  if ((ncol(triangle) != length(delay_pmf))) {
    cli::cli_abort(
      message = c(
        "Length of the delay pmf is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay pmf using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }
}
