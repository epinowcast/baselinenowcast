#' Estimate uncertainty parameters
#'
#' This function ingests a list of point nowcast matrices and a corresponding
#'    list of truncated reporting matrices and uses both to estimate a
#'    vector of uncertainty parameters from the observations
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
#'    estimate the uncertainty parameters.
#' @param uncertainty_model Function that ingests a matrix of observations and a
#'     matrix of predictions and returns a vector that can be used to
#'     apply uncertainty using the same error model. Default is
#'     `fit_by_horizon` with arguments of `obs` matrix of observations and
#'     `pred` the matrix of predictions that fits each column (horizon)
#'     to a negative binomial observation model by default. The user can
#'     specify a different fitting model by replacing the
#'     `fit_model` argument in `fit_by_horizon`, e.g.
#'     `function(obs, pred) fit_by_horizon(obs, pred, fit_model = fit_skellam)`.
#'     See [fit_nb()], [fit_normal()] and [fit_skellam()] for the available
#'     fitting models. Only [fit_normal()] and [fit_skellam()] support negative
#'     values, e.g. from downward corrections retained with
#'     `preprocess = NULL`.
#' @param ref_time_aggregator Function that operates along the rows (reference
#'    times) of the retrospective point nowcast matrix before it has been
#'    aggregated across columns (delays). Default is `identity`
#'    which does not aggregate across reference times.
#' @param delay_aggregator Function that operates along the columns (delays)
#'    of the retrospective point nowcast matrix after it has been aggregated
#'    across reference times. Default is `function(x) rowSums(x, na.rm = TRUE)`.
#' @inheritParams assert_reporting_triangle
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort cli_warn
#' @returns `uncertainty_params` Vector of length of the number of horizons,
#'    with each element representing the estimate of the uncertainty parameter
#'    for each horizon. The specific parameter type depends on the chosen error
#'    model.
#' @family estimate_observation_error
#' @export
#'
#' @examples
#' # Use example data to create reporting triangle
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data = data_as_of_df)
#'
#' # Create retrospective nowcasts
#' trunc_rts <- truncate_to_rows(rep_tri, n = 2)
#' retro_rts <- apply_reporting_structures(trunc_rts)
#' retro_nowcasts <- estimate_and_apply_delays(retro_rts)
#'
#' # Estimate dispersion parameters using default negative binomial model
#' disp_params <- estimate_uncertainty(
#'   point_nowcast_matrices = retro_nowcasts,
#'   truncated_reporting_triangles = trunc_rts,
#'   retro_reporting_triangles = retro_rts
#' )
#' disp_params
#'
#' # Estimate dispersion parameters from rolling sum on the reference times
#' if (requireNamespace("zoo", quietly = TRUE)) {
#'   disp_params_agg <- estimate_uncertainty(
#'     point_nowcast_matrices = retro_nowcasts,
#'     truncated_reporting_triangles = trunc_rts,
#'     retro_reporting_triangles = retro_rts,
#'     ref_time_aggregator = function(x) zoo::rollsum(x, k = 2, align = "right")
#'   )
#'   disp_params_agg
#' }
estimate_uncertainty <- function(
  point_nowcast_matrices,
  truncated_reporting_triangles,
  retro_reporting_triangles,
  n = length(point_nowcast_matrices),
  uncertainty_model = fit_by_horizon,
  ref_time_aggregator = identity,
  delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
  validate = TRUE
) {
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
  non_null_indices <- which(!vapply(
    point_nowcast_matrices[1:n], is.null, logical(1)
  ))
  n_iters <- length(non_null_indices)
  list_of_ncs <- point_nowcast_matrices[non_null_indices]
  list_of_obs <- truncated_reporting_triangles[non_null_indices]
  list_of_rts <- retro_reporting_triangles[non_null_indices]

  # Validate reporting triangles if requested
  if (isTRUE(validate)) {
    lapply(list_of_obs, assert_reporting_triangle, validate = TRUE)
    lapply(list_of_rts, assert_reporting_triangle, validate = TRUE)
  }

  # Convert to plain matrices - we only need matrix operations from here on
  list_of_obs <- lapply(list_of_obs, as.matrix)
  list_of_rts <- lapply(list_of_rts, as.matrix)
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
  n_iters <- .calc_n_retro_nowcast_times(
    list_of_obs,
    n_possible_horizons,
    ref_time_aggregator
  )
  if (n_iters == 0) {
    cli_abort("No valid retrospective nowcast times after reference time aggregation.") # nolint
  }

  if (n_iters < n) {
    cli_warn(
      "Only the first {n_iters} retrospective nowcast times were used."
    )
  }

  agg <- as.matrix(ref_time_aggregator(list_of_obs[[1]]))
  if (!is.numeric(agg)) {
    cli_abort("`ref_time_aggregator` must return a numeric matrix.")
  }
  ncol_agg <- ncol(agg)

  if (ncol_agg != ncol(list_of_obs[[1]])) {
    cli_abort(message = c(
      "`ref_time_aggregator` must return a matrix with ",
      "the same number of columns as the input observations"
    ))
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
    aggr_obs <- ref_time_aggregator(trunc_matr_observed)
    aggr_nowcast <- ref_time_aggregator(nowcast_i)
    aggr_rt_obs <- ref_time_aggregator(triangle_observed)

    # For each horizon, take the partial sum of the nowcasted and already
    # observed components.
    indices_nowcast <- is.na(
      .filter_to_recent_horizons(aggr_rt_obs, n_possible_horizons)
    )
    indices_obs <- !is.na(
      .filter_to_recent_horizons(aggr_obs, n_possible_horizons)
    )
    masked_nowcast <- aggr_nowcast |>
      .filter_to_recent_horizons(n_possible_horizons) |>
      .apply_mask(indices_nowcast, indices_obs)
    masked_obs <- aggr_obs |>
      .filter_to_recent_horizons(n_possible_horizons) |>
      .apply_mask(indices_nowcast, indices_obs)
    # Reverse because the indices are horizons which are ordered opposite to
    # reference times (last reference time = first horizon)
    aggr_nowcast <- delay_aggregator(masked_nowcast)
    if (!is.numeric(aggr_nowcast)) {
      cli_abort(
        "`delay_aggregator` must return a numeric vector, got {class(aggr_nowcast)}" # nolint
      )
    }
    if (length(aggr_nowcast) != n_possible_horizons) {
      cli_abort(
        "`delay_aggregator` must return a vector of length {n_possible_horizons}, got {length(aggr_nowcast)}" # nolint
      )
    }
    exp_to_add[i, ] <- rev(aggr_nowcast)
    aggr_obs <- delay_aggregator(masked_obs)
    if (length(aggr_obs) != n_possible_horizons) {
      cli_abort(
        "`delay_aggregator` must return a vector of length {n_possible_horizons}, got {length(aggr_obs)}" # nolint
      )
    }
    to_add_already_observed[i, ] <- rev(aggr_obs)
  }


  # Ensure obs and pred have the same dimensions, are not NULL, etc.
  .check_obs_and_pred(
    obs = to_add_already_observed,
    pred = exp_to_add
  )
  # Take matrix of observations and predictions and get uncertainty parameters
  # for each column (horizon)
  uncertainty_params <- uncertainty_model(
    obs = to_add_already_observed,
    pred = exp_to_add
  )

  return(uncertainty_params)
}

#' Helper function that fits its each column of the matrix (horizon) to an
#'    observation model.
#'
#' @param obs Matrix or vector of observations.
#' @param pred Matrix or vector of predictions.
#' @param fit_model Function that ingests observations and expectations
#'    and returns uncertainty parameters, default is `fit_nb`. Available
#'    options are [fit_nb()] (negative binomial), [fit_normal()] (normal) and
#'    [fit_skellam()] (Skellam). The latter two support negative observations
#'    and expectations, e.g. arising from downward corrections.
#'
#' @returns Vector of uncertainty parameters of the same length as the number
#'    of columns in the `obs` matrix.
#' @family estimate_observation_error
#' @export
#'
#' @examples
#' obs <- matrix(
#'   c(
#'     5, 6, 2,
#'     1, 4, 2,
#'     8, 4, 2
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#' pred <- matrix(
#'   c(
#'     4.2, 5.2, 1.8,
#'     0.7, 3.5, 3.4,
#'     7.3, 4.1, 1.2
#'   ),
#'   nrow = 3,
#'   byrow = TRUE
#' )
#' disp <- fit_by_horizon(obs = obs, pred = pred)
#' disp
fit_by_horizon <- function(obs,
                           pred,
                           fit_model = fit_nb) {
  .check_obs_and_pred(obs, pred)
  # Coerce vectors/data.frames to matrices and validate numeric
  obs <- as.matrix(obs)
  pred <- as.matrix(pred)

  uncertainty_params <- numeric(ncol(obs))
  for (i in seq_len(ncol(obs))) {
    uncertainty_params[i] <- fit_model(obs[, i], pred[, i])
  }

  return(uncertainty_params)
}


#' Calculate the number of retrospective nowcast times that can be used after
#'    aggregating
#'
#' @param list_of_obs List of matrices of truncated reporting triangles
#' @param n_possible_horizons Integer indicating the number of horizons in the
#'     retrospective reporting triangle.
#' @inheritParams estimate_uncertainty
#'
#' @returns `n_iters` Integer indicating the number of iterations, or
#'    number of retrospective nowcast times, that have sufficient data once
#'    aggregated to be used to generate a retrospective point nowcast.
#' @keywords internal
.calc_n_retro_nowcast_times <- function(
  list_of_obs,
  n_possible_horizons,
  ref_time_aggregator = identity
) {
  if (length(list_of_obs) == 0) {
    return(0)
  }
  # Only use the matrices that have sufficient data once aggregated
  nrow_orig <- nrow(list_of_obs[[1]])
  agg <- as.matrix(ref_time_aggregator(list_of_obs[[1]]))
  nrow_agg <- nrow(agg)
  # Rows to lose
  rows_to_lose <- nrow_orig - nrow_agg
  # Only use the rows that have enough rows
  n_rows_required <- n_possible_horizons + rows_to_lose
  filtered_list_obs <- list_of_obs[sapply(
    list_of_obs, function(mat) nrow(mat) >= n_rows_required
  )]
  n_iters <- length(filtered_list_obs)
  return(n_iters)
}

#' Filter to recent horizons
#'
#' @param mat Matrix containing all the rows
#' @param n_possible_horizons Number of rows we want starting from the final
#'    row
#'
#' @returns `bottom_matrix` Matrix containing the last `n_possible_horizons`
#'    rows of the matrix.
#' @keywords internal
.filter_to_recent_horizons <- function(mat,
                                       n_possible_horizons) {
  max_t <- nrow(mat)
  if (n_possible_horizons <= 0) {
    cli_abort("n_possible_horizons must be >= 1")
  }
  if (n_possible_horizons > max_t) {
    cli_abort("n_possible_horizons ({n_possible_horizons}) exceeds matrix rows ({max_t})") # nolint
  }
  bottom_matrix <- mat[(max_t - n_possible_horizons + 1):max_t, , drop = FALSE] # nolint
  return(bottom_matrix)
}

#' Apply mask to extract the elements of the matrix that are both true
#'
#' @param mat Matrix containing elements for extraction.
#' @param indices_1 Matrix of booleans of the same dimensions of `mat`.
#' @param indices_2 Matrix of booleans of the same dimensions of `mat`
#'
#' @returns Matrix of same dimensions of `mat` with the overlapping `TRUE`
#'   elements only.
#' @keywords internal
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
  if (!is.list(list_obj)) {
    cli_abort("`{name}` must be a list")
  }

  if (length(list_obj) < required_length) {
    msg <- "Insufficient elements in `{name}` for the `n` desired."
    if (!is.null(custom_msg)) {
      msg <- c(msg, i = custom_msg)
    }
    cli_abort(msg)
  }
  if (empty_check && length(list_obj) < 1) {
    cli_abort("`{name}` is an empty list")
  }
  return(invisible(NULL))
}

#' Check observations and predictions are compatible
#'
#' @param obs Matrix or vector of observations.
#' @param pred Matrix or vector of predictions.
#'
#' @returns NULL, invisibly
#' @keywords internal
.check_obs_and_pred <- function(obs, pred) {
  if (is.null(obs) || is.null(pred)) {
    cli_abort("Missing `obs` and/or `pred`") # nolint
  }
  obs <- as.matrix(obs)
  pred <- as.matrix(pred)
  if (!is.numeric(obs) || !is.numeric(pred)) {
    cli_abort("`obs` and `pred` must be numeric (after coercion to matrix).")
  }

  if (!identical(dim(obs), dim(pred))) {
    cli_abort("`obs` and `pred` must have the same dimensions") # nolint
  }
  return(NULL)
}

#' Fit a negative binomial to a vector of observations and expectations
#'
#' @description
#' Takes in a vector of observations and a vector of expectations and performs
#'   a MLE estimator to estimate the dispersion parameter of a negative
#'   binomial. This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW German Hospitalization
#'   Nowcasting Hub.
#'   Modified from: \url{https://github.com/KITmetricslab/RESPINOW-Hub/blob/7fab4dce7b559c3076ab643cf22048cb5fb84cc2/code/baseline/functions.R#L404} #nolint
#' @importFrom stats dnbinom optimize
#' @param x Vector of observed values.
#' @param mu Vector of expected values.
#' @returns the maximum likelihood estimate of the dispersion
#' @family estimate_observation_error
#' @export
#' @examples
#' obs <- c(4, 8, 10)
#' pred <- c(3.1, 7.2, 11)
#' disp <- fit_nb(obs, pred)
#' disp
fit_nb <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }

  # Check for negative values in observations
  if (any(x < 0, na.rm = TRUE)) {
    cli_abort(c(
      "Negative values detected in observations for uncertainty estimation",
      x = "fit_nb() requires non-negative integer observations",
      i = "Consider using preprocess_negative_values() on the reporting triangle if appropriate", # nolint: line_length_linter
      i = "Alternatively, to model downward corrections, use an observation model that supports negative values, such as fit_normal() or fit_skellam()" # nolint: line_length_linter
    ))
  }

  # Check for negative values in predictions
  if (any(mu < 0, na.rm = TRUE)) {
    cli_abort(c(
      "Negative values detected in predictions for uncertainty estimation",
      x = "fit_nb() requires non-negative predictions",
      i = "This may indicate an issue with the delay estimation or preprocessing", # nolint: line_length_linter
      i = "If the delay PMF has negative entries from downward corrections, use an observation model that supports negative values, such as fit_normal() or fit_skellam()" # nolint: line_length_linter
    ))
  }

  # Check that all observations are integers
  assert_integerish(x)
  nllik <- function(size) {
    nll <- -sum(dnbinom(x = x, mu = mu, size = size, log = TRUE),
      na.rm = TRUE
    )
    return(nll)
  }
  opt <- suppressWarnings(optimize(nllik, c(0.1, 1000)))
  return(opt$minimum)
}

#' Fit a normal observation model to a vector of observations and expectations
#'
#' @description
#' Takes in a vector of observations and a vector of expectations and returns
#'   the maximum likelihood estimate of the variance of a normal distribution
#'   with mean fixed at the expectations, i.e. the mean squared error
#'   \eqn{\hat{\sigma}^2 = \frac{1}{n}\sum_i (x_i - \mu_i)^2}.
#'
#'   Unlike [fit_nb()], this supports negative observations and expectations,
#'   so it can be used to quantify uncertainty when the reporting triangle
#'   contains downward revisions (i.e. negative values) that have not been
#'   removed with [preprocess_negative_values()]. Pair it with
#'   [sample_normal()] when generating probabilistic nowcasts. A rounded
#'   normal is most suitable when counts are large and downward revisions are
#'   rare.
#'
#'   Adapted from code written (under an Apache 2.0 license) in the
#'   CDCgov/nowcastNHSN package.
#'   Modified from: \url{https://github.com/CDCgov/nowcastNHSN/blob/3eba887604fab1194343ae6ebaf29e6590d1248b/R/fit_methods.R} #nolint
#' @param x Vector of observed values.
#' @param mu Vector of expected values.
#' @returns The maximum likelihood estimate of the variance, or `NA` if there
#'   are no pairs of non-missing observations and expectations.
#' @family estimate_observation_error
#' @export
#' @examples
#' obs <- c(4, -2, 10)
#' pred <- c(3.1, -0.5, 11)
#' var_est <- fit_normal(obs, pred)
#' var_est
#'
#' # Use as the observation model for each horizon
#' normal_model <- function(obs, pred) {
#'   fit_by_horizon(obs, pred, fit_model = fit_normal)
#' }
#' normal_model(
#'   obs = matrix(c(5, -1, 8, 4, 2, 0), nrow = 3),
#'   pred = matrix(c(4.2, 0.7, 7.3, 5.2, 3.5, 0.4), nrow = 3)
#' )
fit_normal <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }

  # Remove pairs with a missing observation or expectation
  valid <- !is.na(x) & !is.na(mu)
  x <- x[valid]
  mu <- mu[valid]
  if (length(x) == 0) {
    return(NA)
  }

  # MLE of the variance with a fixed mean is the mean squared error
  variance <- mean((x - mu)^2)
  return(variance)
}

#' Fit a Skellam observation model to a vector of observations and expectations
#'
#' @description
#' Takes in a vector of observations and a vector of expectations and
#'   estimates the variance of a Skellam distribution with mean fixed at the
#'   expectations. A Skellam random variable is the difference of two
#'   independent Poisson random variables,
#'   \eqn{X = Y_1 - Y_2}, \eqn{Y_k \sim \mathrm{Poisson}(\lambda_k)}, so it
#'   takes integer values that can be negative.
#'
#'   With the mean fixed at \eqn{\mu = \lambda_1 - \lambda_2}, the variance
#'   \eqn{\sigma^2 = \lambda_1 + \lambda_2} is shared across observations,
#'   giving \eqn{\lambda_1 = (\mu + \sigma^2)/2} and
#'   \eqn{\lambda_2 = (\sigma^2 - \mu)/2}. Both rates must be positive, so the
#'   estimate satisfies \eqn{\sigma^2 > \max_i |\mu_i|}.
#'
#'   Unlike [fit_nb()], this supports negative observations and expectations,
#'   so it can be used to quantify uncertainty when the reporting triangle
#'   contains downward revisions (i.e. negative values) that have not been
#'   removed with [preprocess_negative_values()]. Pair it with
#'   [sample_skellam()] when generating probabilistic nowcasts. The Skellam
#'   is most suitable for low counts where downward revisions are common.
#'
#'   Adapted from code written (under an Apache 2.0 license) in the
#'   CDCgov/nowcastNHSN package.
#'   Modified from: \url{https://github.com/CDCgov/nowcastNHSN/blob/3eba887604fab1194343ae6ebaf29e6590d1248b/R/fit_methods.R} #nolint
#' @inheritParams fit_normal
#' @param method Character string indicating the estimation method. Either
#'   `"mle"` (default) for maximum likelihood estimation or `"mom"` for the
#'   method of moments estimate (the mean squared error, equivalent to
#'   [fit_normal()]).
#' @importFrom stats optimize
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
#' @importFrom rlang arg_match
#' @returns The estimate of the variance, or `NA` if there are no pairs of
#'   non-missing observations and expectations.
#' @family estimate_observation_error
#' @export
#' @examples
#' obs <- c(4, -2, 10, 3)
#' pred <- c(3.1, -0.5, 11, 2.4)
#' var_est <- fit_skellam(obs, pred)
#' var_est
#'
#' # Method of moments estimate
#' fit_skellam(obs, pred, method = "mom")
#'
#' # Use as the observation model for each horizon
#' skellam_model <- function(obs, pred) {
#'   fit_by_horizon(obs, pred, fit_model = fit_skellam)
#' }
#' skellam_model(
#'   obs = matrix(c(5, -1, 8, 4, 2, 0), nrow = 3),
#'   pred = matrix(c(4.2, 0.7, 7.3, 5.2, 3.5, 0.4), nrow = 3)
#' )
fit_skellam <- function(x, mu, method = c("mle", "mom")) {
  method <- arg_match(method)
  if (length(x) == 0) {
    return(NA)
  }

  # Check that all observations are integers
  assert_integerish(x)

  # Remove pairs with a missing observation or expectation
  valid <- !is.na(x) & !is.na(mu)
  x <- x[valid]
  mu <- mu[valid]
  if (length(x) == 0) {
    return(NA)
  }

  mom_est <- mean((x - mu)^2)
  if (method == "mom") {
    return(mom_est)
  }

  # Both Poisson rates must be positive, which requires variance > max(|mu|)
  min_variance <- max(abs(mu)) + 0.01
  nllik <- function(sigma2) {
    lambda1 <- (mu + sigma2) / 2
    lambda2 <- (sigma2 - mu) / 2
    if (any(lambda1 <= 0) || any(lambda2 <= 0)) {
      return(Inf)
    }
    nll <- -sum(.dskellam_log(x, lambda1, lambda2), na.rm = TRUE)
    if (!is.finite(nll)) {
      return(Inf)
    }
    return(nll)
  }

  upper_bound <- max(mom_est * 10, min_variance * 100)
  opt <- suppressWarnings(
    optimize(nllik, c(min_variance, upper_bound))
  )

  if (!is.finite(opt$objective)) {
    cli_abort(c(
      "Skellam maximum likelihood estimation failed to converge.",
      x = "The objective function returned {.val {opt$objective}} at the minimum.", # nolint: line_length_linter
      i = "Try using {.code method = \"mom\"} for a method of moments estimate." # nolint: line_length_linter
    ))
  }
  return(opt$minimum)
}

#' Log probability mass function of the Skellam distribution
#'
#' @description
#' Computes
#'   \eqn{\log p(x) = -(\lambda_1 + \lambda_2) +
#'   \frac{x}{2}\log(\lambda_1/\lambda_2) + \log I_{|x|}(2\sqrt{\lambda_1
#'   \lambda_2})}, where \eqn{I_\nu} is the modified Bessel function of the
#'   first kind, computed on the log scale with `.log_bessel_i()`.
#'
#' @param x Vector of integer values.
#' @param lambda1 Vector of rates of the positive Poisson component.
#' @param lambda2 Vector of rates of the negative Poisson component.
#' @returns Vector of log probabilities.
#' @keywords internal
.dskellam_log <- function(x, lambda1, lambda2) {
  z <- 2 * sqrt(lambda1 * lambda2)
  log_pmf <- -(lambda1 + lambda2) +
    (x / 2) * log(lambda1 / lambda2) +
    .log_bessel_i(z, nu = abs(x))
  return(log_pmf)
}

#' Log of the modified Bessel function of the first kind
#'
#' @description
#' Computes \eqn{\log I_\nu(z)} using the exponentially scaled
#'   [besselI()]. Where this underflows to zero, which happens when \eqn{\nu}
#'   is large relative to \eqn{z} (e.g. a large count with a small expected
#'   value) or when \eqn{z > 10^5}, the uniform asymptotic expansion is used
#'   instead,
#'   \deqn{\log I_\nu(z) \approx r + \nu \log\frac{z}{\nu + r} -
#'   \frac{1}{2}\log(2\pi r) + \log\left(1 + \frac{3 - 5\nu^2/r^2}{24 r}
#'   \right),}
#'   with \eqn{r = \sqrt{\nu^2 + z^2}}.
#'
#' @param z Vector of non-negative arguments.
#' @param nu Vector of non-negative orders.
#' @returns Vector of \eqn{\log I_\nu(z)}.
#' @keywords internal
.log_bessel_i <- function(z, nu) {
  n <- max(length(z), length(nu))
  z <- rep_len(z, n)
  nu <- rep_len(nu, n)
  # besselI() warns when precision is lost and returns 0 on underflow, which
  # is handled by the asymptotic expansion below
  log_bessel <- suppressWarnings(
    log(besselI(z, nu = nu, expon.scaled = TRUE)) + z
  )
  use_approx <- !is.finite(log_bessel) & z > 0
  if (any(use_approx)) {
    z_a <- z[use_approx]
    nu_a <- nu[use_approx]
    r <- sqrt(nu_a^2 + z_a^2)
    log_bessel[use_approx] <- r + nu_a * log(z_a / (nu_a + r)) -
      0.5 * log(2 * pi * r) +
      log1p((3 - 5 * nu_a^2 / r^2) / (24 * r))
  }
  return(log_bessel)
}
