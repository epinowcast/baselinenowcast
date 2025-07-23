#' Estimate dispersion parameters
#'
#' This function ingests a list of point nowcast matrices and a corresponding
#'    list of truncated reporting matrices and uses both to estimate a
#'    vector of negative binomial dispersion parameters from the observations
#'    and estimates at each horizon, starting at 0 up until the max delay
#'    number of horizons.
#'
#' @param pt_nowcast_matrices List of point nowcast matrices where rows
#'    represent reference time points and columns represent delays.
#' @param trunc_reporting_triangles List of truncated reporting matrices,
#'    containing all observations as of the latest reference time. Elements of
#'    list are paired with elements of `pt_nowcast_matrices`.
#' @param retro_reporting_triangles List of `n` truncated reporting triangle
#'   matrices with as many rows as available given the truncation.
#' @param n Integer indicating the number of reporting matrices to use to
#'    estimate the dispersion parameters.
#' @param error_model Function that ingests a matrix of observations and a
#'    matrix of predictions and returns a vector that can be used to
#'    apply uncertainty using the same error model. Default is `NULL`.
#' @param aggregator Function that operates along the rows of the retrospective
#'    point nowcast matrix after it has been aggregated across columns (delays).
#' @param observation_model Character string indicating the choice of
#'   observation model to fit to the predicted nowcasts versus the
#'   observations. Default is `negative binomial`.
#' @param fun_to_aggregate Function that will operate along the nowcast
#'    vectors after summing across delays. Eventually, we can add things like
#'    mean, but for now since we are only providing a negative binomial
#'    observation model, we can only allow sum. Currently supported
#'    functions: `sum`.
#' @param k Integer indicating the number of reference times to apply the
#'    `fun_to_aggregate` over to create target used to compute the nowcast
#'    errors.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort cli_warn
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
#' trunc_rts <- truncate_triangles(triangle, n = 2)
#' retro_rts <- construct_triangles(trunc_rts)
#'
#' retro_nowcasts <- fill_triangles(retro_rts, n = 5)
#' disp_params <- estimate_uncertainty(
#'   pt_nowcast_matrices = retro_nowcasts,
#'   trunc_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts,
#'   n = 2
#' )
#' disp_params
#'
#' # Estimate dispersion parameters from rolling sum
#' disp_params_agg <- estimate_uncertainty(
#'   pt_nowcast_matrices = retro_nowcasts,
#'   trunc_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts,
#'   n = 2,
#'   error_model = fit_obs_vs_pred
#'   aggregator = sum,
#'   k = 2
#' )
#' disp_params_agg
estimate_uncertainty <- function(
    pt_nowcast_matrices,
    trunc_reporting_triangles,
    retro_reporting_triangles,
    n = length(pt_nowcast_matrices),
    error_model = fit_obs_vs_pred, # these each have arguments (this would be the parameteric form)
    error_args = list(observation_model = "negative binomial"),
    aggregator = zoo::rollsum, # this would be the args to rollsum. How do we handle this?
    aggregator_args = list(k = 1,
                           align = "right")
    ) {
  #.validate_aggregation_function(fun_to_aggregate)
  assert_integerish(n, lower = 0)
  .check_list_length(pt_nowcast_matrices, "pt_nowcast_matrices", n)
  .check_list_length(trunc_reporting_triangles, "trunc_reporting_triangles", n)
  .check_list_length(
    retro_reporting_triangles,
    "retro_reporting_triangles",
    n,
    empty_check = FALSE
  )

  # Truncate to only n nowcasts and extract only non-null elements of both lists
  non_null_indices <- which(!sapply(pt_nowcast_matrices[1:n], is.null))
  n_iters <- length(non_null_indices)
  list_of_ncs <- pt_nowcast_matrices[non_null_indices]
  list_of_obs <- trunc_reporting_triangles[non_null_indices]
  list_of_rts <- retro_reporting_triangles[non_null_indices]
  if (n_iters == 0) {
    cli_abort(
      message = c(
        "No valid retrospective nowcasts were found, therefore ",
        "uncertainty can not be estimated using the reporting ",
        "triangle passed in. This may be due to invalid data ",
        "in reporting triangles, such as zeros in the first column."
      )
    )
  }



  # Check that nowcasts has no NAs, trunc_rts has some NAs
  if (any(sapply(list_of_ncs, anyNA))) {
    cli_abort(
      message =
        "`pt_nowcast_matrices` contains NAs"
    )
  }
  if (!any(sapply(list_of_obs, anyNA))) {
    cli_warn(
      message =
        "`trunc_reporting_triangles` does not contain any NAs"
    )
  }
  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `pt_nowcast_matrices` and ",
      "`trunc_reporting_triangles` are not the same."
    ))
  }


  n_possible_horizons <- ncol(list_of_ncs[[1]]) - 1
  # Each row is retrospective nowcast date, each column is a horizon (i.e
  # columns are not delays, but hoirzons, and each cell contains a total
  # value corresponding to that horizon -- the total expected value to add
  exp_to_add <-
    to_add_already_observed <- matrix(NA, nrow = n, ncol = n_possible_horizons)
  for (i in seq_len(n_iters)) {
    # For each individual retrospective nowcast, extract matrices we need:
    nowcast_i <- list_of_ncs[[i]]
    trunc_matr_observed <- list_of_obs[[i]]
    triangle_observed <- list_of_rts[[i]]
    # Get the number of rows
    max_t <- nrow(trunc_matr_observed)

    # Apply the aggregation
    aggr_obs <- do.call(aggregator, c(list(trunc_matr_observed), aggregator_args))
    aggr_nowcast <- do.call(aggregator, c(list( nowcast_i), aggregator_args))
    aggr_rt_obs <- do.call(aggregator, c(list(triangle_observed), aggregator_args))
    n_horizons <- nrow(aggr_obs)
    for (d in 1:n_horizons) {
      indices_nowcast <- is.na(aggr_rt_obs)
      indices_obs <- !is.na(aggr_obs)
      masked_nowcast <- .apply_mask(aggr_nowcast, indices_nowcast, indices_obs)
      masked_obs <- .apply_mask(aggr_obs, indices_nowcast, indices_obs)
      exp_to_add[i,d] <- sum(masked_nowcast, na.rm = TRUE)
      to_add_alread_observed[i,d] <- sum(masked_obs, na.rm = TRUE)
    }
  }
  # Take matrix of observations and predictions and get uncertainty parameters

  uncertainty_params <- do.call(error_model,
                                c(obs = to_add_already_observed,
                                  pred = exp_to_add,
                                  error_args)
                                )
  return(uncertainty_params)
}

.apply_mask <- function(mat,
                        indices_1,
                        indices_2){

  mat_masked <- as.matrix(
    mat * indices_1 * indices_2
  )
  return(mat_masked)
}

.check_list_length <- function(list_obj, name, required_length,
                               custom_msg = NULL, empty_check = TRUE) {
  # Validate input is a list
  if (!is.list(list_obj)) {
    cli_abort(paste0("`", name, "` must be a list"))
  }

  if (length(list_obj) < required_length) {
    cli_abort(message = c(
      "Insufficient elements in `", name, "` for the `n` desired ",
      custom_msg
    ))
  }
  if (empty_check && length(list_obj) < 1) {
    cli_abort(paste0("`", name, "` is an empty list"))
  }
  return(invisible(NULL))
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
  # Check that all observations are integers
  assert_integerish(x)
  nllik <- function(size) {
    nll <- -sum(dnbinom(x = x, mu = mu, size = size, log = TRUE), na.rm = TRUE)
    return(nll)
  }
  opt <- optimize(nllik, c(0.1, 1000))
  return(opt$minimum)
}
