#' Apply the delay to generate a point nowcast
#' @description
#' This function takes as an input the reporting triangle that we want to
#' complete with a nowcast and a delay pmf and generates a point estimate of a
#' completed reporting square (or rectangle)
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
    triangle,
    delay_pmf
  )
  n_delays <- nrow(delay_df)
  n_dates <- nrow(triangle_to_nowcast)
  expectation <- triangle_to_nowcast

  for (co in 2:n_delays) {
    block_bottom_left <- expectation[(n_dates - co + 2):n_dates, 1:(co - 1),
      drop = FALSE
    ]
    # Uses the observed data to find the expected total on that reference date
    exp_total <- rowSums(block_bottom_left) / sum(delay_pmf[1:(co - 1)])
    # * Note, we will have to do some correction if this is 0, ignore for now*
    # Finds the expected value for the particular delay by scaling by the
    # delay pmf for delay d
    expectation[(n_dates - co + 2):n_dates, co] <- exp_total * delay_pmf[co]
  }

  return(expectation)
}
