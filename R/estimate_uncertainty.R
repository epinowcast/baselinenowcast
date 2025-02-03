#' Estimate the uncertainty as a function of delay d
#' @description
#' This function takes in as input the triangle that we want to nowcast and
#' the delay pmf, which may have been estimated separately. It uses the delay
#' pmf to compute, for each forecast date and delay, the expected number of
#' confirmed cases on that forecast date at that delay d. For each delay, it
#' estimates an independent negative binomial dispersion parameter. This code
#' is based on the code originally developed by the Karlsruhe Institute of
#' Technology RESPINOW German Hospitalization Nowcasting Hub,
#' https://github.com/KITmetricslab/RESPINOW-Hub/blob/39e2b17bc79492b0aee4c0b615a1c8dbf978ef53/code/baseline/functions.R#L142
#'
#'
#' @param triangle_to_nowcast matrix of the incomplete reporting triangle to be
#' nowcasted, with rows representing the time points of reference and columns
#' representing the delays
#' @param delay_pmf vector of delays assumed to be indexed starting at the
#' first delay column in `triangle_to_nowcast`
#' @param n_history_dispersion integer indicating the number of reference dates
#' to be used in the estimate of the dispersion, always starting from the most
#' recent refrence date. The default is to use the whole reporting triangle,
#' so `nrow(triangle_to_nowcast) - 1`
#'
#' @returns a dataframe of dispersion parameters alongside their corresponding
#' delay d
#' @export
#'
#' @examples
#' #' library(epinowcast)
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
#' disp_df <- estimate_uncertainty(
#'   triangle_to_nowcast = triangle_raw,
#'   delay_pmf = delay_df$pmf,
#'   n_history_dispersion = n_history_dispersion
#' )
estimate_uncertainty <- function(triangle_to_nowcast,
                                 delay_pmf,
                                 n_history_dispersion = nrow(triangle_to_nowcast)) { # nolint

  # Get the truncated matrix of observations you will use to estimate the
  # dispersion (get rid of early rows that we're not using and add NAs to
  # bottom right of the triangle)
  matr_observed_raw <- triangle_to_nowcast[(nrow(triangle_to_nowcast) - n_history_dispersion):nrow(triangle_to_nowcast), ] # nolint
  matr_observed <- replace_lower_right_with_NA(matr_observed_raw)
  expectation_to_add_already_observed <-
    to_add_already_observed <-
    matrix(NA, nrow = n_history_dispersion, ncol = length(delay_pmf))
  for (t in seq_along(1:n_history_dispersion)) {
    # What would we have already observed as of the first forecast date we are
    # using to estimate the dispersion?
    # matr_observed_temp <- matr_NAs
    # matr_obs_temp_to_nowcast <- matr_NAs

    # Truncate the matrix observed by ignoring rows after t, replace rows that
    # wouldn't be observed with NAs
    matr_observed_temp <- matrix(matr_observed[1:t, ], nrow = t)
    matr_observed_temp <- replace_lower_right_with_NA(matr_observed_temp)

    # From the truncated would have been observed as of t matrix, get the
    # expected nowcast as of t for each d
    matr_exp_temp <- apply_delay(
      triangle_to_nowcast = matr_observed_temp,
      delay_pmf = delay_pmf
    )

    # We now have what we would have added, and what we observed to have added.
    trunc_matr_observed <- matrix(matr_observed[1:t, ], nrow = t)
    indices_nowcast <- matrix(is.na(matr_observed_temp), nrow = t)
    indices_observed <- matrix(!is.na(trunc_matr_observed), nrow = t)

    # For each delay, find what would have been added at that delay for that
    # forecast date, and compare to what was actually added at that delay up
    # until and including that forecast date
    for (d in 1:length(delay_pmf)) {
      expectation_to_add_already_observed[t, d] <- sum(indices_nowcast[, d] * indices_observed[, d] * matr_exp_temp[, d], na.rm = TRUE)
      to_add_already_observed[t, d] <- sum(indices_nowcast[, d] * indices_observed[, d] * trunc_matr_observed[, d], na.rm = TRUE)
    }
  }

  disp_params <- vector(length = length(delay_pmf))
  # run through horizons
  for (i in 1:length(delay_pmf)) {
    obs_temp <- to_add_already_observed[, i]
    mu_temp <- expectation_to_add_already_observed[, i]
    mu_temp <- mu_temp + 0.1
    disp_params[i] <- fit_nb(x = obs_temp, mu = mu_temp)
  }

  return(disp_params)
}

#' Fit a negative binomial to a vector of observations and expectations
#'
#' @description
#' Takes in a vector of observations and a vector of expectations and performs
#' a MLE estimator to estimate the dispersion parameter of a negative binomial.
#' This code is based on the code originally developed by the Karlsruhe
#' Institute of Technology RESPINOW German Hospitalization Nowcasting Hub
#' https://github.com/KITmetricslab/RESPINOW-Hub/blob/7fab4dce7b559c3076ab643cf22048cb5fb84cc2/code/baseline/functions.R#L404 #nolint
#'
#' @param x the observed values
#' @param mu the expected values
#' @returns the maximum likelihood estimate of the dispersion
fit_nb <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }
  nllik <- function(size) {
    -sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)
  }
  opt <- optimize(nllik, c(0.1, 1000))
  opt$minimum
}
