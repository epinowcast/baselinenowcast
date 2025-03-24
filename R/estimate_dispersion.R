#' Estimate dispersion parameters
#'
#' This function ingests a list of point nowcasts (completed reporting
#'    rectangles) and a corresponding list of truncated reporting triangles and
#'    estimates and uses both to estimate a vector of negative binomial
#'    dispersion parameters from the observations and estimates at each delay,
#'    starting at delay = 1.
#'
#' @param list_of_nowcasts List of complete reporting rectangles where rows
#'    represent reference time points and columns represent delays.
#' @param list_of_trunc_rts List of truncated reporting triangle matrices,
#'    containing all observations as of the latest reference time. Elements of
#'    list are paired with elements of `list_of_nowcasts`.
#' @param n Integer indicating the number of reporting rectangles to use to
#'    estimate the dispersion parameters.
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
#'   triangle = triangle,
#'   n = 2
#' )
#' retro_rts <- generate_triangles(
#'   list_of_trunc_rts = trunc_rts
#' )
#' retro_nowcasts <- generate_point_nowcasts(
#'   list_of_rts = retro_rts,
#'   n = 5
#' )
#' disp_params <- estimate_dispersion(
#'   list_of_nowcasts = retro_nowcasts,
#'   list_of_trunc_rts = trunc_rts,
#'   n = 2
#' )
estimate_dispersion <- function(
    list_of_nowcasts,
    list_of_trunc_rts,
    n = length(list_of_nowcasts)) {
  # Check that the length of the list of nowcasts is greater than
  # or equal to the specified n
  if (length(list_of_nowcasts) < n) {
    cli_abort(message = c(
      "Insufficient elements in `list_of_nowcasts` for the `n` desired number ",
      "of nowcasted reporting triangles specified for dispersion estimation"
    ))
  }
  if (length(list_of_trunc_rts) < n) {
    cli_abort(message = c(
      "Insufficient elements in `list_of_trunc_rts` for the `n` desired number ", # nolint
      "of observed reporting triangles specified for dispersion estimation"
    ))
  }
  if (length(list_of_nowcasts) < 1) {
    "`list_of_nowcasts` is an empty list"
  }
  if (length(list_of_trunc_rts) < 1) {
    "`list_of_trunc_rts` is an empty list"
  }

  assert_integerish(n, lower = 0)

  # Truncate to only n nowcasts
  list_of_ncs <- list_of_nowcasts[1:n]
  list_of_obs <- list_of_trunc_rts[1:n]

  # Check that nowcasts has no NAs, trunc_rts has some NAs
  if (any(sapply(list_of_ncs, anyNA))) {
    cli_abort(
      message =
        "`list_of_nowcasts` contains NAs"
    )
  }
  if (!any(sapply(list_of_obs, anyNA))) {
    cli_abort(
      message =
        "`list_of_obs` does not contain any NAs"
    )
  }
  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `list_of_nowcasts` and ",
      "`list_of_trunc_rts` are not the same."
    ))
  }


  n_horizons <- ncol(list_of_ncs[[1]])
  for (i in seq_len(n)) {
    # Rretrospective nowcast as of i delays ago
    nowcast_i <- list_of_ncs[[i]]
    # Remove the last i observations
    trunc_matr_observed <- list_of_obs[[i]]
    # What would have been nowcasted? The values that would have been NA
    indices_nowcast <- is.na(.replace_lower_right_with_NA(trunc_matr_observed))
    # What was observed? Any non-NAs from the truncated observed matrix
    indices_observed <- !is.na(trunc_matr_observed)

    # Get a vector of for each delay of what would have been added in this
    # reference time, based on the indices nowcasted and the indices later
    # observed
    exp_to_add <- sapply(seq_len(ncol(trunc_matr_observed)),
      .conditional_sum_cols,
      matrix_bool1 = indices_nowcast,
      matrix_bool2 = indices_observed,
      matrix_to_sum = nowcast_i
    )
    # Get a vector for each delay of what was observed using the truncated
    # observed matrix
    to_add <- sapply(seq_len(ncol(trunc_matr_observed)),
      .conditional_sum_cols,
      matrix_bool1 = indices_nowcast,
      matrix_bool2 = indices_observed,
      matrix_to_sum = trunc_matr_observed
    )
    # Create a dataframe to what would have been added at each delay for
    # each t delay ago.
    df_i <- data.frame(
      exp_to_add = exp_to_add,
      to_add = to_add,
      t = i,
      d = seq_len(n_horizons) - 1
    )
    if (i == 1) {
      df_exp_obs <- df_i
    } else {
      df_exp_obs <- rbind(df_exp_obs, df_i)
    }
  }

  # Separate step which uses the dataframe that compares the expected values to
  # add and the values observed at each reference time and delay to estimate
  # the dispersion (we could make this a separate function).
  disp_params <- vector(length = n_horizons - 1)
  for (i in seq_len(n_horizons - 1)) {
    obs_temp <- df_exp_obs$to_add[df_exp_obs$d == i]
    mu_temp <- df_exp_obs$exp_to_add[df_exp_obs$d == i] + 0.1
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
