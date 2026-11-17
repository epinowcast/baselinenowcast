#' Sample from negative binomial model given a set of predictions
#'
#' @param pred Vector of predictions.
#' @param uncertainty_params Vector of uncertainty parameters.
#' @family generate_probabilistic_nowcasts
#' @importFrom stats rnbinom
#' @importFrom cli cli_abort
#' @export
#' @returns `sampled_pred` Object of the same dimensions as `pred` representing
#'    a single draw from the negative binomial distribution
#'    with the specified `uncertainty params`.
#' @examples
#' pred <- c(3.2, 4.6)
#' sampled_preds <- sample_nb(pred,
#'   uncertainty_params = c(50, 100)
#' )
#' sampled_preds
sample_nb <- function(pred, uncertainty_params) {
  .check_sampler_inputs(pred, uncertainty_params)
  sampled_pred <- rnbinom(
    n = length(pred),
    size = uncertainty_params,
    mu = as.numeric(pred)
  )
  sampled_pred <- .match_pred_dims(sampled_pred, pred)
  return(sampled_pred)
}

#' Sample from a normal model given a set of predictions
#'
#' @description
#' Draws from a normal distribution with mean `pred` and variance
#'   `uncertainty_params`. By default, draws are continuous, suitable for
#'   non-integer measurements such as percentages. Set `rounded = TRUE` to
#'   round draws to the nearest integer for count data. Unlike [sample_nb()],
#'   draws can be negative, so this sampler can represent downward corrections.
#'   Use with uncertainty parameters estimated with [fit_normal()].
#'
#'   To use rounding as the `uncertainty_sampler` in [baselinenowcast()] or
#'   [sample_nowcasts()], pass a two-argument function, e.g.
#'   `function(pred, uncertainty_params) sample_normal(pred,
#'   uncertainty_params, rounded = TRUE)`. For non-integer reporting triangles,
#'   use `preprocess = NULL` in [baselinenowcast()] to retain fractional values.
#'
#'   Adapted from code written (under an Apache 2.0 license) in the
#'   CDCgov/nowcastNHSN package.
#'   Modified from: \url{https://github.com/CDCgov/nowcastNHSN/blob/3eba887604fab1194343ae6ebaf29e6590d1248b/R/sample_methods.R} #nolint
#'
#' @param pred Vector of predictions.
#' @param uncertainty_params Vector of uncertainty parameters, here the
#'   variances of the normal distribution.
#' @param rounded Logical indicating whether to round draws to the nearest
#'   integer. Default is `FALSE`, which returns continuous draws.
#' @family generate_probabilistic_nowcasts
#' @importFrom stats rnorm
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_flag
#' @export
#' @returns `sampled_pred` Object of the same dimensions as `pred` representing
#'    a single draw from the normal distribution, optionally rounded,
#'    with the specified `uncertainty params`.
#' @examples
#' pred <- c(3.2, -1.4)
#' sampled_preds <- sample_normal(pred,
#'   uncertainty_params = c(4, 9)
#' )
#' sampled_preds
#'
#' # Rounded draws for count data
#' sample_normal(pred, uncertainty_params = c(4, 9), rounded = TRUE)
sample_normal <- function(pred, uncertainty_params, rounded = FALSE) {
  .check_sampler_inputs(pred, uncertainty_params)
  .check_sampler_variances(uncertainty_params)
  assert_flag(rounded)
  sampled_pred <- rnorm(
    n = length(pred),
    mean = as.numeric(pred),
    sd = sqrt(uncertainty_params)
  )
  if (rounded) {
    sampled_pred <- round(sampled_pred)
  }
  sampled_pred <- .match_pred_dims(sampled_pred, pred)
  return(sampled_pred)
}

#' Sample from a Skellam model given a set of predictions
#'
#' @description
#' Draws from a Skellam distribution with mean `pred` and variance
#'   `uncertainty_params`, as the difference of two Poisson draws,
#'   \eqn{X = Y_1 - Y_2}, with rates \eqn{\lambda_1 = (\mu + \sigma^2)/2}
#'   and \eqn{\lambda_2 = (\sigma^2 - \mu)/2}. Draws are integers that can be
#'   negative, so this sampler can represent downward corrections. Use with
#'   uncertainty parameters estimated with [fit_skellam()].
#'
#'   Both rates must be positive, which requires the variance to be greater
#'   than the absolute value of the prediction. This can be violated when the
#'   variance is estimated from past nowcasts with smaller predictions. By
#'   default this raises an error. Setting `robust = TRUE` instead widens the
#'   variance of the affected predictions to the smallest valid value, which
#'   preserves the mean.
#'
#'   To use `robust = TRUE` as the `uncertainty_sampler` in
#'   [baselinenowcast()] or [sample_nowcasts()], pass it in a function, e.g.
#'   `function(pred, uncertainty_params) sample_skellam(pred,
#'   uncertainty_params, robust = TRUE)`.
#'
#'   Adapted from code written (under an Apache 2.0 license) in the
#'   CDCgov/nowcastNHSN package.
#'   Modified from: \url{https://github.com/CDCgov/nowcastNHSN/blob/3eba887604fab1194343ae6ebaf29e6590d1248b/R/sample_methods.R} #nolint
#'
#' @inheritParams sample_nb
#' @param uncertainty_params Vector of uncertainty parameters, here the
#'   variances of the Skellam distribution.
#' @param robust Logical indicating whether to widen variances that are not
#'   greater than the absolute value of the prediction so that sampling
#'   succeeds. Default is `FALSE`, which raises an error instead.
#' @family generate_probabilistic_nowcasts
#' @importFrom stats rpois
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_flag
#' @export
#' @returns `sampled_pred` Object of the same dimensions as `pred` representing
#'    a single draw from the Skellam distribution
#'    with the specified `uncertainty params`.
#' @examples
#' pred <- c(3.2, -1.4)
#' sampled_preds <- sample_skellam(pred,
#'   uncertainty_params = c(5, 9)
#' )
#' sampled_preds
#'
#' # Widen invalid variances rather than error
#' sample_skellam(pred, uncertainty_params = 2, robust = TRUE)
sample_skellam <- function(pred, uncertainty_params, robust = FALSE) {
  .check_sampler_inputs(pred, uncertainty_params)
  .check_sampler_variances(uncertainty_params)
  assert_flag(robust)
  pred_vec <- as.numeric(pred)
  variance <- rep_len(uncertainty_params, length(pred_vec))

  # In robust mode, widen the variance to the smallest value that keeps both
  # Poisson rates positive (variance > |pred|). This preserves the mean and
  # only adjusts entries that would otherwise be invalid.
  if (robust) {
    variance <- pmax(variance, abs(pred_vec) + 1e-6)
  }

  lambda1 <- 0.5 * (pred_vec + variance)
  lambda2 <- 0.5 * (variance - pred_vec)
  invalid_idx <- which(lambda1 <= 0 | lambda2 <= 0)
  if (length(invalid_idx) > 0) {
    cli_abort(c(
      "Invalid Skellam parameters: variance must be greater than |pred|.",
      x = "Found {length(invalid_idx)} prediction{?s} with invalid parameters (indices: {head(invalid_idx, 5)}).", # nolint: line_length_linter
      i = "Set `robust = TRUE` to widen the variance where needed, or increase the variance." # nolint: line_length_linter
    ))
  }

  sampled_pred <- rpois(length(pred_vec), lambda1) -
    rpois(length(pred_vec), lambda2)
  sampled_pred <- .match_pred_dims(sampled_pred, pred)
  return(sampled_pred)
}

#' Check numeric sampler inputs and compatible parameter lengths
#'
#' @inheritParams sample_nb
#' @returns NULL, invisibly
#' @keywords internal
.check_sampler_inputs <- function(pred, uncertainty_params) {
  if (!is.numeric(pred)) {
    cli_abort("`pred` must be numeric (vector or matrix).")
  }
  if (!is.numeric(uncertainty_params)) {
    cli_abort("`uncertainty_params` must be numeric (scalar or vector).")
  }
  n <- length(pred)
  if (!(length(uncertainty_params) %in% c(1L, n))) {
    cli_abort("`uncertainty_params` must have length 1 or match length(pred).")
  }
  return(invisible(NULL))
}

#' Check variance parameters for normal and Skellam samplers
#'
#' @inheritParams sample_normal
#' @returns NULL, invisibly
#' @keywords internal
.check_sampler_variances <- function(uncertainty_params) {
  if (anyNA(uncertainty_params) || any(uncertainty_params < 0)) {
    cli_abort(
      "`uncertainty_params` must be non-negative variances without NAs."
    )
  }
  return(invisible(NULL))
}

#' Give sampled values the same dimensions as the predictions
#'
#' @param sampled_pred Vector of sampled values.
#' @inheritParams sample_normal
#' @returns `sampled_pred` with the `dim` and `dimnames` of `pred` if `pred`
#'   is a matrix, otherwise unchanged.
#' @keywords internal
.match_pred_dims <- function(sampled_pred, pred) {
  if (is.matrix(pred)) {
    dim(sampled_pred) <- dim(pred)
    dimnames(sampled_pred) <- dimnames(pred)
  }
  return(sampled_pred)
}
