#' Validate triangle
#' @description
#' Various checks to make sure that the reporting triangle passed in to
#' `estimate_delay()` is formatted properly.
#' @param triangle a matrix of values with rows indicating the time points and
#' columns indicating the delays
#' @inheritParams get_delay_estimate
#'
#' @returns NULL, invisibly
validate_triangle <- function(triangle,
                              max_delay,
                              n_history) {
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
