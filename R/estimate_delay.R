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
#' @param triangle dataframe of the reporting triangle, with one column for the
#' reference date and the remaining columns specifying the delay (units not
#' specified). Values indicate the number of new observations assigned to
#' the reference date at the specified delay.
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
#' @importFrom dplyr filter
#' @examples
#' library(epinowcast)
#' nat_germany_hosp <-
#'   germany_covid19_hosp[location == "DE"][age_group == "00+"]
#' nat_germany_hosp <- enw_filter_report_dates(
#'   nat_germany_hosp,
#'   latest_date = "2021-10-01"
#' )
#' pobs <- enw_preprocess_data(nat_germany_hosp, max_delay = 21)
#' triangle_raw <- pobs$reporting_triangle[[1]]
#' delay_df <- estimate_delay(triangle_raw[, -1],
#'   max_delay = 20,
#'   n_history = 30
#' )
estimate_delay <- function(triangle,
                           max_delay = ncol(triangle) - 2,
                           n_history = nrow(triangle)) {
  # Filter the triangle down to nrow = n_history + 1, ncol = max_delay
  max_date <- max(triangle$reference_date, na.rm = TRUE) - n_history
  trunc_triangle <- preprocess_reporting_triangle(triangle, max_delay) |>
    dplyr::filter(reference_date >= max_date) |>
    as.data.frame() # nolint
  # Make the date the rowname, so the matrix is just the entries
  integer_cols <- which(colnames(trunc_triangle) %in% (grep("^\\d+$", names(trunc_triangle), value = TRUE))) # nolint
  # the `..` is because its a data.table, we probably don't want to expect this from users. #nolint
  rt <- as.matrix(trunc_triangle[, integer_cols])
  dates <- as.character(trunc_triangle$reference_date)
  rownames(rt) <- dates
  n_delays <- ncol(rt)
  n_dates <- nrow(rt)
  factor <- matrix(nrow = max_delay - 1, ncol = 1)
  expectation <- rt
  for (co in 2:(n_delays)) {
    block_top_left <- rt[1:(n_dates - co + 1), 1:(co - 1), drop = FALSE]
    block_top <- rt[1:(n_dates - co + 1), co, drop = FALSE]
    factor[co - 1] <- sum(block_top) / max(sum(block_top_left), 1)
    block_bottom_left <- expectation[(n_dates - co + 2):n_dates, 1:(co - 1),
      drop = FALSE
    ]
    expectation[(n_dates - co + 2):n_dates, co] <- factor[co - 1] * rowSums(
      block_bottom_left
    )
  }
  # Not sure if this is right, as it might be overly weighting the
  # estimate data (but I do think that is the point...)
  delay_df <- data.frame(
    delay = 0:max_delay,
    pmf = colSums(expectation) / sum(expectation)
  )
  return(delay_df)
}
