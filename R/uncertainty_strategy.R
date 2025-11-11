#' Uncertainty Strategy Constructor
#'
#' @description
#' Creates a strategy object that defines how a base fitting function should
#' be applied across multiple horizons or other dimensions. Strategies enable
#' different approaches to parameter estimation (e.g., independent fits per
#' horizon, pooled across horizons, smoothed, etc.).
#'
#' @param apply_fit Function that takes a base fit function and returns a
#'   wrapped version. The wrapper should define how the base fit is applied.
#'   Signature: `function(base_fit) function(obs, pred) {...}`.
#' @param name Character string naming the strategy (e.g., "by_horizon",
#'   "pooled", "smoothed").
#'
#' @returns An object of class `c(paste0("uncertainty_", name),
#'   "uncertainty_strategy")` with an `apply_fit` function and a `name`
#'   attribute.
#'
#' @family uncertainty_strategy
#' @seealso [uncertainty_by_horizon()]
#' @export
#'
#' @examples
#' # Create a custom pooled strategy (example)
#' pooled_strategy <- uncertainty_strategy(
#'   apply_fit = function(base_fit) {
#'     function(obs, pred) {
#'       # Pool all observations and predictions
#'       base_fit(as.vector(obs), as.vector(pred))
#'     }
#'   },
#'   name = "pooled"
#' )
#' print(pooled_strategy)
uncertainty_strategy <- function(apply_fit, name) {
  # Validate inputs
  checkmate::assert_function(apply_fit)
  checkmate::assert_character(name, len = 1)

  # Create the strategy object with two-class hierarchy
  strategy <- structure(
    list(
      apply_fit = apply_fit
    ),
    name = name,
    class = c(paste0("uncertainty_", name), "uncertainty_strategy")
  )

  strategy
}

#' Fit Uncertainty Parameters Independently by Horizon
#'
#' @description
#' Creates a strategy that fits uncertainty parameters independently for each
#' horizon (delay). This wraps the existing [fit_by_horizon()] functionality,
#' which iterates over columns of the observation and prediction matrices and
#' applies the base fit function to each column independently.
#'
#' @returns An object of class `c("uncertainty_by_horizon",
#'   "uncertainty_strategy")` that wraps base fit functions to apply them
#'   independently to each horizon.
#'
#' @family uncertainty_strategy
#' @seealso [fit_by_horizon()], [uncertainty_strategy()]
#' @export
#'
#' @examples
#' # Create by-horizon strategy
#' strategy <- uncertainty_by_horizon()
#' print(strategy)
#'
#' # Use with a distribution
#' nb_model <- uncertainty_nb(strategy = uncertainty_by_horizon())
uncertainty_by_horizon <- function() {
  strategy <- uncertainty_strategy(
    apply_fit = function(base_fit) {
      # Return a function that applies fit_by_horizon with the base_fit
      function(obs, pred) {
        fit_by_horizon(obs = obs, pred = pred, fit_model = base_fit)
      }
    },
    name = "by_horizon"
  )

  strategy
}

#' Print Method for Uncertainty Strategies
#'
#' @param x An object of class `uncertainty_strategy`.
#' @param ... Additional arguments (not used).
#'
#' @returns Invisibly returns the input object.
#' @family uncertainty_strategy
#' @export
#'
#' @examples
#' strategy <- uncertainty_by_horizon()
#' print(strategy)
print.uncertainty_strategy <- function(x, ...) {
  cat("Uncertainty Strategy: ", attr(x, "name"), "\n", sep = "")
  invisible(x)
}

#' Validate Uncertainty Strategy
#'
#' @description
#' Internal function to validate that an object is a valid
#' uncertainty_strategy.
#'
#' @param strategy Object to validate.
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @keywords internal
#' @importFrom checkmate assert_class assert_function
#' @importFrom cli cli_abort
assert_uncertainty_strategy <- function(strategy) {
  if (!inherits(strategy, "uncertainty_strategy")) {
    cli_abort(c(
      "Invalid uncertainty strategy",
      x = "Must be an object of class 'uncertainty_strategy'",
      i = "Use uncertainty_by_horizon() or uncertainty_strategy() to create"
    ))
  }

  checkmate::assert_class(strategy, "uncertainty_strategy")
  checkmate::assert_list(strategy)

  if (!"apply_fit" %in% names(strategy)) {
    cli_abort(c(
      "Invalid uncertainty strategy",
      x = "Strategy must contain 'apply_fit' component"
    ))
  }

  checkmate::assert_function(strategy$apply_fit)

  # Use exact = TRUE to avoid partial matching with "names" attribute
  if (is.null(attr(strategy, "name", exact = TRUE))) {
    cli_abort(c(
      "Invalid uncertainty strategy",
      x = "Strategy must have 'name' attribute"
    ))
  }

  invisible(NULL)
}
