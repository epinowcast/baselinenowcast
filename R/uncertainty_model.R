#' Uncertainty Model Constructor
#'
#' @description
#' Creates an uncertainty model object that defines both the probability
#' distribution family and the fitting strategy for uncertainty estimation.
#' This is the base constructor that allows users to define custom
#' uncertainty models.
#'
#' @param fit Function that takes observed and predicted values and returns
#'   fitted parameters. This function should have already been wrapped by the
#'   strategy at construction time. Signature: `function(obs, pred)`.
#' @param sample Function that takes predictions and fitted parameters and
#'   returns random samples. Signature: `function(pred, params)`.
#' @param family Character string indicating the distribution family
#'   (e.g., "nb", "poisson", "custom").
#' @param strategy An object of class `uncertainty_strategy` that was used to
#'   wrap the base fit function.
#'
#' @returns An object of class `c(paste0("uncertainty_", family),
#'   "uncertainty_model")` containing the fit function, sample function,
#'   family name, and strategy.
#'
#' @family uncertainty_model
#' @seealso [uncertainty_nb()], [uncertainty_poisson()],
#'   [uncertainty_strategy()]
#' @export
#'
#' @examples
#' # Create a custom uncertainty model
#' my_model <- uncertainty_model(
#'   fit = function(obs, pred) mean((obs - pred)^2),
#'   sample = function(pred, params) rnorm(length(pred), pred, sqrt(params)),
#'   family = "gaussian",
#'   strategy = uncertainty_by_horizon()
#' )
#' print(my_model)
uncertainty_model <- function(fit,
                              sample,
                              family,
                              strategy) {
  # Validate inputs
  checkmate::assert_function(fit)
  checkmate::assert_function(sample)
  checkmate::assert_character(family, len = 1)
  assert_uncertainty_strategy(strategy)

  # Create the model object with two-class hierarchy
  model <- structure(
    list(
      fit = fit,
      sample = sample,
      family = family,
      strategy = strategy
    ),
    class = c(paste0("uncertainty_", family), "uncertainty_model")
  )

  # Validate the constructed model
  assert_uncertainty_model(model)

  return(model)
}

#' Print Method for Uncertainty Models
#'
#' @param x An object of class `uncertainty_model`.
#' @param ... Additional arguments (not used).
#'
#' @returns Invisibly returns the input object.
#' @family uncertainty_model
#' @export
#'
#' @examples
#' model <- uncertainty_nb()
#' print(model)
print.uncertainty_model <- function(x, ...) {
  cat("Uncertainty Model:\n")
  cat("  Family:  ", x$family, "\n", sep = "")
  cat("  Strategy:", attr(x$strategy, "name"), "\n")
  invisible(x)
}
