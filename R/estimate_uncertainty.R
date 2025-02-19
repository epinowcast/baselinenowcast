#' Estimate the uncertainty in the nowcasts
#' @description
#' This function takes in as input the triangle that we want to nowcast and
#'   the delay pmf, which may have been estimated separately. It uses the delay
#'   pmf to compute, for each forecast date and delay, the expected number of
#'   confirmed cases on that forecast date at that delay d. For each delay, it
#'   estimates an independent negative binomial dispersion parameter. This code
#'   was adapted from code written (under an MIT license) by the Karlsruhe
#'   Institute of Technology RESPINOW German Hospitalization Nowcasting Hub,
#' Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/39e2b17bc79492b0aee4c0b615a1c8dbf978ef53/code/baseline/functions.R#L142 #nolint
#' @param triangle_for_uncertainty matrix of the incomplete reporting triangle
#'   to be nowcasted, with rows representing the time points of reference and
#'   columns representing the delays
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `triangle_for_uncertainty`. Default is `NULL`
#' @param n_history_dispersion Integer indicating the number of observations
#'   to be used in the estimate of the dispersion, always starting from the most
#'   recent reference date. The default is to use the whole reporting triangle,
#'   so `nrow(triangle_for_uncertainty) - 1`
#' @param n_history Integer indicating the number of observations to be
#'  used in the estimate of the delay estimate for each retrospective reporting
#'  triangle, if the delay estimate is being re-estimated. Default is `NULL`
#' @importFrom cli cli_abort
#' @returns Vector of dispersion parameters of length of the `delay_pmf` -1
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )
#' delay_pmf <- get_delay_estimate(
#'   triangle = triangle,
#'   max_delay = 3,
#'   n_history = 4
#' )
#' # If using delay_pmf to estimate uncertainty
#' disp_params <- estimate_uncertainty(
#'   triangle_for_uncertainty = triangle,
#'   delay_pmf = delay_pmf,
#'   n_history_dispersion = 5
#' )
#' # If using iterative process of re-estimating delay to estimate uncertainty
#' disp_params <- estimate_uncertainty(
#'   triangle_for_uncertainty = triangle,
#'   n_history_dispersion = 2,
#'   n_history = 4
#' )
estimate_uncertainty <- function(triangle_for_uncertainty,
                                 delay_pmf = NULL,
                                 n_history = max(
                                   floor(
                                     nrow(triangle_for_uncertainty) / 2
                                   ) - 1,
                                   ncol(triangle_for_uncertainty)
                                 ),
                                 n_history_dispersion = NULL) { # nolint

  # If we are using the delay_pmf, than n_history = 0
  if (!is.null(delay_pmf)) {
    n_history <- 0
  }
  .validate_uncertainty_inputs(
    triangle_for_uncertainty,
    delay_pmf,
    n_history_dispersion,
    n_history
  )


  n_horizons <- ncol(triangle_for_uncertainty) - 1


  .validate_triangle(triangle_for_uncertainty)
  if (n_history_dispersion > nrow(triangle_for_uncertainty - 1)) {
    cli_abort(
      message = c(
        "Triangle to nowcast does not contain sufficient rows to ",
        "estimate uncertainty from `n_history_dispersion` observations. Either",
        "pass in a triangle of more rows or lower the `n_history_dispersion`"
      )
    )
  }

  # Method 1: Generate retrospective nowcasts by re-estimating the delay
  # distribution and recomputing the nowcast
  if (is.null(delay_pmf)) {
    # Get the truncated matrix of observations you will use to estimate the
    # dispersion (get rid of early rows that we're not using and add NAs to
    # bottom right of the triangle)
    matr_observed <- triangle_for_uncertainty[
      (nrow(triangle_for_uncertainty) - n_history_dispersion - n_history):
      nrow(triangle_for_uncertainty),
    ] |>
      .replace_lower_right_with_NA()

    results <- lapply(seq_len(n_history_dispersion),
      .get_delay_nowcast_and_obs_t,
      matr_observed = matr_observed,
      n_history = n_history
    )
    ## Method 2: Generate retrospective nowcasts using the delay distribution
    # specified
  } else if (!is.null(delay_pmf)) {
    .validate_delay_and_triangle(
      triangle = triangle_for_uncertainty,
      delay_pmf = delay_pmf
    )
    # Get the truncated matrix of observations you will use to estimate the
    # dispersion (get rid of early rows that we're not using and add NAs to
    # bottom right of the triangle)
    matr_observed <- triangle_for_uncertainty[
      (nrow(triangle_for_uncertainty) - n_history_dispersion):
      nrow(triangle_for_uncertainty),
    ] |>
      .replace_lower_right_with_NA()

    results <- lapply(seq_len(n_history_dispersion),
      .get_exp_and_obs_as_of_t,
      matr_observed = matr_observed,
      delay_pmf = delay_pmf
    )
  }


  # Extracting results
  exp_to_add_already_observed <- do.call(
    rbind,
    lapply(results, `[[`, "exp_to_add")
  )
  to_add_already_observed <- do.call(
    rbind,
    lapply(results, `[[`, "to_add")
  )


  disp_params <- vector(length = n_horizons)
  # remove rows with zero initial reports (Christmas etc)
  to_keep <- abs(exp_to_add_already_observed[, 1]) >= 0.1
  to_add_already_observed <- to_add_already_observed[to_keep, ]
  # run through horizons
  for (i in seq_along(1:n_horizons)) {
    obs_temp <- to_add_already_observed[, i]
    mu_temp <- exp_to_add_already_observed[, i]
    mu_temp <- mu_temp + 0.1
    disp_params[i] <- .fit_nb(x = obs_temp, mu = mu_temp)
  }

  return(disp_params)
}

#' Compute the sum of entries of a column in a matrix where both sets of
#'   matrices of booleans are TRUE
#'
#' @param col Integer indicating the column to sum over
#' @param matrix_bool1 Matrix of booleans for the first set of indices
#' @param matrix_bool2 Matrix of booleans for the second set of indices
#' @param matrix_to_sum Matrix to be summed for that specific colun
#'
#' @returns Numeric summing the values in the `matrix_to_sum` at the specified
#'   column for the entries that are true
#' @keywords internal
.conditional_sum_cols <- function(col,
                                  matrix_bool1,
                                  matrix_bool2,
                                  matrix_to_sum) {
  if (col > dim(matrix_to_sum)[2]) {
    cli::cli_abort(
      message = "Column to sum is out of bounds of input matrices"
    )
  }

  if (!all(dim(matrix_bool1) == dim(matrix_bool2))) {
    cli::cli_abort(
      message = "Dimensions of boolean matrices are not the same"
    )
  }

  if (!all(dim(matrix_to_sum) == dim(matrix_bool1))) {
    cli::cli_abort(
      message =
        "Dimensions of boolean matrices and matrix to sum are not the same"
    )
  }

  cond_sum <- sum(
    matrix_bool1[, col] *
      matrix_bool2[, col] *
      matrix_to_sum[, col],
    na.rm = TRUE
  )
  return(cond_sum)
}

#' Get expected and observed additional counts as of reference time t
#' @param t Integer indicating the "as of" time
#' @param matr_observed Matrix of the available observations, with NAs for
#'   the observations that are missing.
#' @param n_history Integer indicating the number of observations to be
#'   used in the estimate of the delay estimate for each retrospective reporting
#'   triangle, if the delay estimate is being re-estimated.
#'
#' @return Dataframe of length of `delay_pmf` containing the expected values
#'   to be added at each delay as of time t and the observed values to be added
#'   as of time t
#' @keywords internal
.get_delay_nowcast_and_obs_t <- function(t,
                                         matr_observed,
                                         n_history) {
  n_horizons <- ncol(matr_observed) - 1
  # Truncate the matrix observed by ignoring rows after t, replace rows that
  # wouldn't be observed with NAs
  matr_observed_temp <- matrix(matr_observed[1:(n_history + t), ],
    nrow = (n_history + t)
  ) |>
    .replace_lower_right_with_NA()
  # From the truncated would have been observed as of t matrix, get the
  # expected nowcast as of t for each d
  temp_delay <- get_delay_estimate(
    triangle = matr_observed_temp,
    n_history = n_history
  )
  matr_exp_temp <- apply_delay(
    triangle_to_nowcast = matr_observed_temp,
    delay_pmf = temp_delay
  )

  # We now have what we would have estimated for each delay,
  # and the corresponding observations for those reference dates and delays
  # (if they are present).
  trunc_matr_observed <- matrix(matr_observed[1:(n_history + t), ],
    nrow = (n_history + t)
  )
  indices_nowcast <- matrix(is.na(matr_observed_temp),
    nrow = (n_history + t)
  )
  indices_observed <- matrix(!is.na(trunc_matr_observed),
    nrow = (n_history + t)
  )

  # First delay is 0, there will never be any nowcasts. We skip to
  # delay = 1 (index of 2), and apply the logic for each horizon, finding the
  # indices that were both nowcasted and observed, and estimating what we
  # would have added at that delay vs what we observed.
  exp_to_add <- sapply((seq_len(n_horizons) + 1),
    .conditional_sum_cols,
    matrix_bool1 = indices_nowcast,
    matrix_bool2 = indices_observed,
    matrix_to_sum = matr_exp_temp
  )
  to_add <- sapply((seq_len(n_horizons) + 1),
    .conditional_sum_cols,
    matrix_bool1 = indices_nowcast,
    matrix_bool2 = indices_observed,
    matrix_to_sum = trunc_matr_observed
  )
  return(
    data.frame(
      exp_to_add = exp_to_add,
      to_add = to_add,
      t = t,
      d = seq_len(n_horizons)
    )
  )
}


#' Get expected and observed additional counts as of reference time t
#' @param t Integer indicating the "as of" time
#' @param matr_observed Matrix of the available observations, with NAs for
#'   the observations that are missing.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `matr_observed`
#'
#' @return Dataframe of length of `delay_pmf` containing the expected values
#'    to be added at each delay as of time t and the observed values to be added
#'    as of time t
#' @keywords internal
.get_exp_and_obs_as_of_t <- function(t,
                                     matr_observed,
                                     delay_pmf) {
  n_horizons <- length(delay_pmf) - 1
  # Truncate the matrix observed by ignoring rows after t, replace rows that
  # wouldn't be observed with NAs
  matr_observed_temp <- matrix(matr_observed[1:t, ], nrow = t) |>
    .replace_lower_right_with_NA()
  # From the truncated would have been observed as of t matrix, get the
  # expected nowcast as of t for each d
  matr_exp_temp <- apply_delay(
    triangle_to_nowcast = matr_observed_temp,
    delay_pmf = delay_pmf
  )

  # We now have what we would have estimated for each delay,
  # and the corresponding observations for those reference dates and delays
  # (if they are present).
  trunc_matr_observed <- matrix(matr_observed[1:t, ], nrow = t)
  indices_nowcast <- matrix(is.na(matr_observed_temp), nrow = t)
  indices_observed <- matrix(!is.na(trunc_matr_observed), nrow = t)

  # First delay is 0, there will never be any nowcasts. We skip to
  # delay = 1 (index of 2), and apply the logic for each horizon, finding the
  # indices that were both nowcasted and observed, and estimating what we
  # would have added at that delay vs what we observed.
  exp_to_add <- sapply((seq_len(n_horizons) + 1),
    .conditional_sum_cols,
    matrix_bool1 = indices_nowcast,
    matrix_bool2 = indices_observed,
    matrix_to_sum = matr_exp_temp
  )
  to_add <- sapply((seq_len(n_horizons) + 1),
    .conditional_sum_cols,
    matrix_bool1 = indices_nowcast,
    matrix_bool2 = indices_observed,
    matrix_to_sum = trunc_matr_observed
  )
  return(
    data.frame(
      exp_to_add = exp_to_add,
      to_add = to_add,
      t = t,
      d = seq_len(n_horizons)
    )
  )
}

#' Fit a negative binomial to a vector of observations and expectations
#'
#' @description
#' Takes in a vector of observations and a vector of expectations and performs
#'   a MLE estimator to estimate the dispersion parameter of a negative
#'   binomial. This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW German Hospitalization
#'   Nowcasting Hub.
#'   Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/7fab4dce7b559c3076ab643cf22048cb5fb84cc2/code/baseline/functions.R#L404 #nolint
#' @importFrom stats dnbinom optimize
#' @param x the observed values
#' @param mu the expected values
#' @returns the maximum likelihood estimate of the dispersion
#' @keywords internal
.fit_nb <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }
  nllik <- function(size) {
    -sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)
  }
  opt <- optimize(nllik, c(0.1, 1000))
  opt$minimum
}
