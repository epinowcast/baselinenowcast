#' Estimate a delay distribution from a reporting triangle
#' @description
#' Provides an estimate of the reporting delay as a function
#' of the delay, based on the reporting triangle and the specified maximum
#' delay and number of reference date observations to be used in the estimation.
#' This point estimate of the delay is computed empirically, using an
#' iterative algorithm starting from the most recent observations. It was
#' modified from the code originally developed by the Karlsruhe Institute
#' of Technology RESPINOW German Hospitalization Nowcasting Hub,
#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7cce3ae2728116e8c8cc0e4ab29074462c24650e/code/baseline/functions.R#L55 #nolint
#' @param triangle matrix of the reporting triangle, with rows representing
#' the time points of reference and columns representing the delays
#' indexed at 0
#' @param max_delay integer indicating the maximum delay to estimate, in units
#' of the delay. The default is to use the whole reporting triangle,
#'  `ncol(triangle) -1`.
#' @param n_history integer indicating the number of reference dates to be
#' used in the estimate of the reporting delay, always starting from the most
#' recent reporting delay. The default is to use the whole reporting triangle,
#' so `nrow(triangle)-1`
#' @returns delay_df dataframe of length `max_delay` with columns `delay`
#' and `pmf`, indicating the point estimate of the empirical probability
#' mass on each delay
#' @export
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
#' delay_df <- get_delay_estimate(triangle_raw,
#'   max_delay = 20,
#'   n_history = 30
#' )
get_delay_estimate <- function(triangle,
                               max_delay = ncol(triangle) - 1,
                               n_history = nrow(triangle)) {
  # Check that the input reporting triangle is formatted properly.
  validate_triangle(triangle,
    max_delay = max_delay,
    n_history = n_history
  )

  # Filter the triangle down to nrow = n_history + 1, ncol = max_delay
  nr0 <- nrow(triangle)
  trunc_triangle <- triangle[(nr0 - n_history + 1):nr0, 1:(max_delay + 1)]
  rt <- handle_neg_vals(trunc_triangle)
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
  delay_df <- data.frame(
    delay = 0:max_delay,
    pmf = pmf
  )
  return(delay_df)
}
