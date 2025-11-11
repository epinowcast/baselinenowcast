#' Negative Binomial Uncertainty Model
#'
#' @description
#' Creates an uncertainty model using the negative binomial distribution.
#' The model uses maximum likelihood estimation to fit the dispersion
#' parameter and generates random samples from the fitted distribution.
#'
#' @param strategy An object of class `uncertainty_strategy` defining how to
#'   apply the fitting function across horizons. Defaults to
#'   [uncertainty_by_horizon()].
#'
#' @returns An object of class `c("uncertainty_nb", "uncertainty_model")`
#'   with fit and sample functions wrapped by the specified strategy.
#'
#' @family uncertainty_distributions
#' @seealso [fit_nb()], [sample_nb()], [uncertainty_poisson()],
#'   [uncertainty_model()]
#' @export
#'
#' @examples
#' # Create negative binomial model with default strategy
#' nb_model <- uncertainty_nb()
#' print(nb_model)
#'
#' # Create with custom strategy (when available)
#' nb_model_horizon <- uncertainty_nb(strategy = uncertainty_by_horizon())
uncertainty_nb <- function(strategy = uncertainty_by_horizon()) {
  # Validate strategy
  assert_uncertainty_strategy(strategy)

  # Wrap the base fit function with the strategy
  wrapped_fit <- strategy$apply_fit(fit_nb)

  # Create the uncertainty model
  model <- uncertainty_model(
    fit = wrapped_fit,
    sample = sample_nb,
    family = "nb",
    strategy = strategy
  )

  return(model)
}

#' Poisson Uncertainty Model
#'
#' @description
#' Creates an uncertainty model using the Poisson distribution.
#' This model assumes equidispersion (variance equals mean) and is
#' appropriate when uncertainty is proportional to the expected value.
#'
#' @inheritParams uncertainty_nb
#'
#' @returns An object of class `c("uncertainty_poisson", "uncertainty_model")`
#'   with fit and sample functions wrapped by the specified strategy.
#'
#' @family uncertainty_distributions
#' @seealso [uncertainty_nb()], [uncertainty_model()]
#' @export
#'
#' @examples
#' # Create Poisson model with default strategy
#' pois_model <- uncertainty_poisson()
#' print(pois_model)
uncertainty_poisson <- function(strategy = uncertainty_by_horizon()) {
  # Validate strategy
  assert_uncertainty_strategy(strategy)

  # Wrap the base fit function with the strategy
  wrapped_fit <- strategy$apply_fit(.fit_poisson)

  # Create the uncertainty model
  model <- uncertainty_model(
    fit = wrapped_fit,
    sample = .sample_poisson,
    family = "poisson",
    strategy = strategy
  )

  return(model)
}

#' Fit Poisson Distribution (Internal)
#'
#' @description
#' Internal function to fit a Poisson distribution. For Poisson, no
#' additional parameters need to be estimated as the mean fully specifies
#' the distribution. This function returns 1 as a placeholder parameter.
#'
#' @param x Vector of observed values (not used for Poisson fitting).
#' @param mu Vector of expected values (not used for Poisson fitting).
#'
#' @returns Numeric value of 1 (placeholder for consistency with other
#'   fitting functions).
#' @keywords internal
#' @importFrom checkmate assert_integerish
#' @importFrom cli cli_abort
.fit_poisson <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }

  # Check for negative values in observations
  if (any(x < 0, na.rm = TRUE)) {
    cli_abort(c(
      "Negative values detected in observations for uncertainty estimation",
      x = ".fit_poisson() requires non-negative integer observations"
    ))
  }

  # Check for negative values in predictions
  if (any(mu < 0, na.rm = TRUE)) {
    cli_abort(c(
      "Negative values detected in predictions for uncertainty estimation",
      x = ".fit_poisson() requires non-negative predictions"
    ))
  }

  # Check that all observations are integers
  checkmate::assert_integerish(x)

  # For Poisson, no parameters to fit (mean is already known from pred)
  # Return 1 as a placeholder
  return(1)
}

#' Sample from Poisson Distribution (Internal)
#'
#' @description
#' Internal function to generate random samples from a Poisson distribution.
#' The uncertainty_params argument is ignored as Poisson uses only the
#' mean parameter.
#'
#' @param pred Numeric vector or matrix of predicted values (means).
#' @param uncertainty_params Numeric scalar or vector (ignored for Poisson).
#'
#' @returns Numeric vector or matrix of the same shape as `pred` containing
#'   random samples from Poisson distributions with means given by `pred`.
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom stats rpois
.sample_poisson <- function(pred, uncertainty_params) {
  if (!is.numeric(pred)) {
    cli_abort("`pred` must be numeric (vector or matrix).")
  }

  if (!is.null(pred)) {
    sampled_pred <- rpois(n = length(pred), lambda = as.numeric(pred))

    if (is.matrix(pred)) {
      dim(sampled_pred) <- dim(pred)
      dimnames(sampled_pred) <- dimnames(pred)
    }
  } else {
    sampled_pred <- NULL
  }

  return(sampled_pred)
}
