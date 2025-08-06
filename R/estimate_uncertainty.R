#' Estimate dispersion parameters
#'
#' This function ingests a list of point nowcast matrices and a corresponding
#'    list of truncated reporting matrices and uses both to estimate a
#'    vector of negative binomial dispersion parameters from the observations
#'    and estimates at each horizon, starting at 0 up until the max delay
#'    number of horizons.
#'
#' @param point_nowcast_matrices List of point nowcast matrices where rows
#'    represent reference time points and columns represent delays.
#' @param truncated_reporting_triangles List of truncated reporting matrices,
#'    containing all observations as of the latest reference time. Elements of
#'    list are paired with elements of `point_nowcast_matrices`.
#' @param retro_reporting_triangles List of `n` truncated reporting triangle
#'   matrices with as many rows as available given the truncation.
#' @param n Integer indicating the number of reporting matrices to use to
#'    estimate the dispersion parameters.
#' @param error_model Function that ingests a matrix of observations and a
#'     matrix of predictions and returns a vector that can be used to
#'     apply uncertainty using the same error model. Default is
#'     fit_distribution.
#' @param error_args List of arguments needed for the specified error model.
#'     Default is `list(observation_model_name = "negative_binomial").`
#' @param ref_time_aggregator Function that operates along the rows (reference
#'    times) of the retrospective point nowcast matrix before it has been
#'    aggregated across columns (delays). Default is `zoo::rollsum`.
#' @param ref_time_aggregator_args List of arguments needed for the specified
#'     `ref_time_aggregator`. Default is `list(k=1, align = "right")`.
#' @param delay_aggregator Function that operates along the columns (delays)
#'    of the retrospective point nowcast matrix after it has been aggregated
#'    across reference times. Default is `rowSums`.
#' @param delay_aggregator_args List of arguments needed for the specified
#'    `delay_aggregator`. Default is `list(na.rm = TRUE)`.
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort cli_warn
#' @returns `uncertainty_params` Vector of length one less than the number of
#'    columns in the latest reporting triangle, with each element
#'    representing the estimate of the dispersion parameter for each delay d,
#'    starting at delay d=1.
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
#' # Estimate dispersion parameters using default (negative binomial error
#' # model on the sums
#' disp_params <- estimate_uncertainty(
#'   point_nowcast_matrices = retro_nowcasts,
#'   truncated_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts,
#' )
#' disp_params
#'
#' # Estimate dispersion parameters from rolling mean with a normal error model
#' disp_params_agg <- estimate_uncertainty(
#'   point_nowcast_matrices = retro_nowcasts,
#'   truncated_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts,
#'   n = 2,
#'   error_model = fit_distribution,
#'   error_args = list(observation_model_name = "normal"),
#'   ref_time_aggregator = zoo::rollmean,
#'   ref_time_aggregator_args = list(k = 1, align = "right")
#' )
#' disp_params_agg
estimate_uncertainty <- function(
    point_nowcast_matrices,
    truncated_reporting_triangles,
    retro_reporting_triangles,
    n = length(point_nowcast_matrices),
    error_model = fit_distribution,
    error_args = list(observation_model_name = "negative binomial"),
    ref_time_aggregator = zoo::rollsum,
    ref_time_aggregator_args = list(
      k = 1,
      align = "right"
    ),
    delay_aggregator = rowSums,
    delay_aggregator_args = list(na.rm = TRUE)) {
  assert_integerish(n, lower = 0)
  .check_list_length(
    point_nowcast_matrices,
    "point_nowcast_matrices", n
  )
  .check_list_length(
    truncated_reporting_triangles,
    "truncated_reporting_triangles", n
  )
  .check_list_length(
    retro_reporting_triangles,
    "retro_reporting_triangles",
    n,
    empty_check = FALSE
  )

  # Truncate to only n nowcasts and extract only non-null elements of both lists
  non_null_indices <- which(!sapply(point_nowcast_matrices[1:n], is.null))
  n_iters <- length(non_null_indices)
  list_of_ncs <- point_nowcast_matrices[non_null_indices]
  list_of_obs <- truncated_reporting_triangles[non_null_indices]
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
        "`point_nowcast_matrices` contains NAs"
    )
  }
  if (!any(sapply(list_of_obs, anyNA))) {
    cli_warn(
      message =
        "`truncated_reporting_triangles` does not contain any NAs"
    )
  }
  # Check that the sets of matrices are the same dimensions
  dims_ncs <- lapply(list_of_ncs, dim)
  dims_obs <- lapply(list_of_obs, dim)
  all_identical <- all(mapply(identical, dims_ncs, dims_obs))
  if (!all_identical) {
    cli_abort(message = c(
      "Dimensions of the first `n` matrices in `point_nowcast_matrices` and ",
      "`truncated_reporting_triangles` are not the same."
    ))
  }

  n_possible_horizons <- sum(is.na(rowSums(list_of_rts[[1]])))
  # Only use the matrices that have sufficient data once aggregated,
  # and warn user that not everything is being used
  nrow_orig <- nrow(list_of_obs[[1]])
  nrow_agg <- nrow(do.call(
    ref_time_aggregator,
    c(
      list(list_of_obs[[1]]),
      ref_time_aggregator_args
    )
  ))
  # Rows to lose
  rows_to_lose <- nrow_orig - nrow_agg
  # Only use the rows that have enough rows
  n_rows_required <- n_possible_horizons + rows_to_lose
  filtered_list_obs <- list_of_obs[sapply(
    list_of_obs, function(mat) nrow(mat) >= n_rows_required
  )]
  n_iters <- length(filtered_list_obs)
  if (n_iters < n) {
    cli_warn(
      message = c(
        "Only the first {n_iters} retrospective nowcast times were used."
      )
    )
  }


  # Each row is retrospective nowcast date, each column is a horizon (i.e
  # columns are not delays, but horizons, and each cell contains a total
  # value corresponding to that horizon -- the total expected value to add
  exp_to_add <-
    to_add_already_observed <- matrix(NA,
      nrow = n_iters,
      ncol = n_possible_horizons
    )



  for (i in seq_len(n_iters)) {
    # For each individual retrospective nowcast, extract matrices we need from
    # the corresponding elements in the list.
    nowcast_i <- list_of_ncs[[i]]
    trunc_matr_observed <- list_of_obs[[i]]
    triangle_observed <- list_of_rts[[i]]

    # Apply the aggregation to the truncated observations, the nowcast,
    # and the reporting triangle.
    aggr_obs <- do.call(
      ref_time_aggregator,
      c(
        list(trunc_matr_observed),
        ref_time_aggregator_args
      )
    )
    aggr_nowcast <- do.call(
      ref_time_aggregator,
      c(
        list(nowcast_i),
        ref_time_aggregator_args
      )
    )
    aggr_rt_obs <- do.call(
      ref_time_aggregator,
      c(
        list(triangle_observed),
        ref_time_aggregator_args
      )
    )
    max_t <- nrow(aggr_obs)
    # For each horizon, take the partial sum of the nowcasted and already
    # observed components.
    indices_nowcast <- is.na(
      aggr_rt_obs[(max_t - n_possible_horizons + 1):max_t, ]
    )
    indices_obs <- !is.na(aggr_obs[(max_t - n_possible_horizons + 1):max_t, ])
    masked_nowcast <- .apply_mask(
      aggr_nowcast[(max_t - n_possible_horizons + 1):max_t, ],
      indices_nowcast,
      indices_obs
    )
    masked_obs <- .apply_mask(
      aggr_obs[(max_t - n_possible_horizons + 1):max_t, ],
      indices_nowcast,
      indices_obs
    )
    # Reverse because the indices are horizons which are ordered opposite to
    # reference times (last reference time = first horizon)
    exp_to_add[i, ] <- rev(do.call(
      delay_aggregator,
      c(
        list(masked_nowcast),
        delay_aggregator_args
      )
    ))
    to_add_already_observed[i, ] <- rev(do.call(
      delay_aggregator,
      c(
        list(masked_obs),
        delay_aggregator_args
      )
    ))
  }

  if (!any(exp_to_add != 0 & !is.na(exp_to_add))) {
    cli_abort(
      message = c(
        "Insufficient data for uncertainty estimation. Check to ",
        "ensure that input matrices contain sufficient number of ",
        "rows for the specified aggregator model and corresponding",
        "arguments"
      )
    )
  }

  # Take matrix of observations and predictions and get uncertainty parameters
  uncertainty_params <- do.call(
    error_model,
    c(
      list(to_add_already_observed),
      list(exp_to_add),
      error_args
    )
  )

  return(uncertainty_params)
}

#' Apply mask to extract the elements of the matrix that are both true
#'
#' @param mat Matrix containing elements for extraction.
#' @param indices_1 Matrix of booleans of the same dimensions of `mat`.
#' @param indices_2 Matrix of booleans of the same dimensions of `mat`
#'
#' @returns Matrix of same dimensions of `mat` with the overlapping `TRUE`
#'   elements only.
.apply_mask <- function(mat,
                        indices_1,
                        indices_2) {
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
