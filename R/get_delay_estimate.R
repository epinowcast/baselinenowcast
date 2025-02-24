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
#' @param triangle Matrix of the reporting triangle, with rows representing
#'   the time points of reference and columns representing the delays
#' @param max_delay Integer indicating the maximum delay to estimate, in units
#'   of the delay. The default is to use the whole reporting triangle,
#'   `ncol(triangle) -1`.
#' @param n_history Integer indicating the number of reference dates to be
#'   used in the estimate of the reporting delay, always starting from the most
#'   recent reporting delay. The default is to use the whole reporting triangle,
#'   so `nrow(triangle)-1`
#' @returns Vector indexed at 0 of length `max_delay + 1` with columns
#'   indicating the point estimate of the empirical probability
#'   mass on each delay
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
#'   triangle = triangle,
#'   max_delay = 3,
#'   n_history = 4
#' )
#' print(delay_pmf)
get_delay_estimate <- function(triangle,
                               max_delay = ncol(triangle) - 1,
                               n_history = nrow(triangle)) {
  # Check that the input reporting triangle is formatted properly.
  .validate_triangle(triangle,
    max_delay = max_delay,
    n_history = n_history
  )

  # Filter the triangle down to nrow = n_history + 1, ncol = max_delay
  nr0 <- nrow(triangle)
  trunc_triangle <- triangle[(nr0 - n_history + 1):nr0, 1:(max_delay + 1)]
  rt <- .handle_neg_vals(trunc_triangle)
  n_delays <- ncol(rt)
  n_dates <- nrow(rt)
  factor <- vector(length = max_delay - 1)
  expectation <- rt
  for (co in 2:(n_delays)) {
    block_top_left <- rt[1:(n_dates - co + 1), 1:(co - 1), drop = FALSE]
    block_top <- rt[1:(n_dates - co + 1), co, drop = FALSE]
    factor[co - 1] <- sum(block_top) / max(sum(block_top_left), 1)
    block_bottom_left <- expectation[(n_dates - co + 2):n_dates, 1:(co - 1),
      drop = FALSE
    ]
    # We compute the expectation so that we can get the delay estimate
    expectation[(n_dates - co + 2):n_dates, co] <- factor[co - 1] * rowSums(
      block_bottom_left
    )
  }

  # Use the completed reporting square to get the point estimate of the delay
  # distribution
  pmf <- colSums(expectation) / sum(expectation)

  return(pmf)
}
