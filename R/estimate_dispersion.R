#' Estimate dispersion parameters
#'
#' This function ingests a list of point nowcast matrices and a corresponding
#'    list of truncated reporting matrices and uses both to estimate a
#'    vector of negative binomial dispersion parameters from the observations
#'    and estimates at each reference time up until the latest reference time
#'    minust the maximum delay (excluding the latest one).
#'
#' @param pt_nowcast_mat_list List of point nowcast matrices where rows
#'    represent reference time points and columns represent delays.
#' @param trunc_rep_mat_list List of truncated reporting matrices,
#'    containing all observations as of the latest reference time. Elements of
#'    list are paired with elements of `pt_nowcast_mat_list`.
#' @param n Integer indicating the number of reporting matrices to use to
#'    estimate the dispersion parameters. Default is the number of matrices
#'    in the `pt_nowcast_mat_list`
#' @importFrom checkmate assert_integerish
#' @returns Vector of length one less than the number of columns in the
#'    latest reporting triangle, with each element representing the estimate
#'    of the dispersion parameter for each delay d, starting at delay d=1.
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
#'
#' trunc_rts <- truncate_triangles(
#'   reporting_triangle = triangle,
#'   n = 2
#' )
#' retro_rts <- generate_triangles(
#'   trunc_rep_mat_list = trunc_rts
#' )
#' retro_nowcasts <- generate_pt_nowcast_mat_list(
#'   reporting_triangle_list = retro_rts,
#'   n = 5
#' )
#' disp_params <- estimate_dispersion(
#'   pt_nowcast_mat_list = retro_nowcasts,
#'   trunc_rep_mat_list = trunc_rts,
#'   n = 2
#' )
estimate_dispersion <- function(
    pt_nowcast_mat_list,
    trunc_rep_mat_list,
    n = length(pt_nowcast_mat_list)) {
  # Check that the length of the list of nowcasts is greater than
  # or equal to the specified n
  if (length(pt_nowcast_mat_list) < n) {
    cli_abort(message = c(
      "Insufficient elements in `pt_nowcast_mat_list` for the `n` desired ",
      "number of nowcasted reporting triangles specified for dispersion ",
      "estimation"
    ))
  }
  if (length(trunc_rep_mat_list) < n) {
    cli_abort(message = c(
      "Insufficient elements in `trunc_rep_mat_list` for the `n` desired ",
      "number of observed reporting triangles specified for dispersion ",
      "estimation"
    ))
  }
  if (length(pt_nowcast_mat_list) < 1) {
    "`pt_nowcast_mat_list` is an empty list"
  }
  if (length(trunc_rep_mat_list) < 1) {
    "`trunc_rep_mat_list` is an empty list"
  }

  assert_integerish(n, lower = 0)

  # Truncate to only n nowcasts
  list_of_ncs <- pt_nowcast_mat_list[1:n]
  list_of_obs <- trunc_rep_mat_list[1:n]

  # Check that nowcasts has no NAs, trunc_rts has some NAs
  if (any(sapply(list_of_ncs, anyNA))) {
    cli_abort(
      message =
        "`pt_nowcast_mat_list` contains NAs"
    )
  }
  if (!any(sapply(list_of_obs, anyNA))) {
    cli_abort(
      message =
        "`trunc_rep_mat_list` does not contain any NAs"
    )
  }
  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `pt_nowcast_mat_list` and ",
      "`trunc_rep_mat_list` are not the same."
    ))
  }

  max_delay <- ncol(list_of_ncs[[1]]) - 1
  for (i in seq_len(n)) {
    # Rretrospective nowcast as of i delays ago
    nowcast_i <- list_of_ncs[[i]]
    # Remove the last i observations
    trunc_matr_observed <- list_of_obs[[i]]
    # What would have been nowcasted? The values that would have been NA
    indices_nowcast <- is.na(replace_lower_right_with_NA(trunc_matr_observed))
    # What was observed? Any non-NAs from the truncated observed matrix
    indices_observed <- !is.na(trunc_matr_observed)

    # For each retrospective nowcast, get a vector of expectations as a
    # indexed by the original reference time and the delay
    bool_mat <- (indices_nowcast * indices_observed) == 1
    exp_val <- nowcast_i[bool_mat]
    obs_val <- trunc_matr_observed[bool_mat]
    indices <- which(bool_mat, arr.ind = TRUE)
    exp_vals <- data.frame(
      mu_t_d = exp_val,
      obs_t_d = obs_val,
      ref_time = indices[, 1],
      delay = indices[, 2] - 1
    )
    if (i == 1) {
      df_exp_obs <- exp_vals
    } else {
      df_exp_obs <- rbind(df_exp_obs, exp_vals)
    }
  }

  df_exp_obs <- df_exp_obs |>
    mutate(
      horizon = nrow(list_of_ncs[[1]]) + 1 - ref_time
    )
  # Separate step which uses the dataframe that compares the expected values to
  # add and the values observed at each reference time and delay to estimate
  # the dispersion as a function of reference time. We intentionally only
  # go back to the maximum delay though, so these are only estimated for the
  # parts of the matrix that we've nowcasted
  disp_params <- vector(length = max_delay)
  for (i in 1:max_delay) {
    obs_temp <- df_exp_obs$obs_t_d[df_exp_obs$horizon == i]
    mu_temp <- df_exp_obs$mu_t_d[df_exp_obs$horizon == i] + 0.1
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
#' @param matrix_to_sum Matrix to be summed for that specific column
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
    nll <- -sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)
    return(nll)
  }
  opt <- optimize(nllik, c(0.1, 1000))
  return(opt$minimum)
}
