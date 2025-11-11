#' Uncertainty Configuration Options
#'
#' @description
#' Creates a complete uncertainty configuration object that bundles together
#' the uncertainty model (distribution and fitting strategy) and aggregation
#' options. This ensures consistency between parameter estimation and sampling,
#' and provides a single object to pass to uncertainty estimation functions.
#'
#' @param model An object of class `uncertainty_model` defining the
#'   probability distribution and fitting strategy. Defaults to
#'   [uncertainty_nb()] with [uncertainty_by_horizon()] strategy.
#' @param aggregation An object of class `aggregation_opts` defining how to
#'   preprocess data before fitting. Defaults to [aggregation_observed()].
#'
#' @returns An object of class `uncertainty_opts` containing the model and
#'   aggregation components. This object can be passed to functions like
#'   [estimate_uncertainty()], [sample_prediction()], and
#'   [estimate_and_apply_uncertainty()].
#'
#' @family uncertainty_opts
#' @seealso [uncertainty_nb()], [uncertainty_poisson()],
#'   [uncertainty_by_horizon()], [aggregation_observed()],
#'   [aggregation_opts()]
#' @export
#'
#' @examples
#' # Use all defaults (negative binomial, by-horizon, observed aggregation)
#' opts <- uncertainty_opts()
#' print(opts)
#'
#' # Use Poisson distribution instead of negative binomial
#' opts_pois <- uncertainty_opts(
#'   model = uncertainty_poisson()
#' )
#' print(opts_pois)
#'
#' # Use custom aggregation
#' opts_custom <- uncertainty_opts(
#'   model = uncertainty_nb(),
#'   aggregation = aggregation_opts(
#'     ref_time = function(x) {
#'       # 3-day rolling sum (requires zoo package)
#'       if (requireNamespace("zoo", quietly = TRUE)) {
#'         zoo::rollsum(x, k = 3, align = "right", fill = NA)
#'       } else {
#'         x
#'       }
#'     },
#'     delay = function(x) rowSums(x, na.rm = TRUE)
#'   )
#' )
#' print(opts_custom)
uncertainty_opts <- function(model = uncertainty_nb(),
                             aggregation = aggregation_observed()) {
  # Validate inputs
  assert_uncertainty_model(model)
  assert_aggregation_opts(aggregation)

  # Create the opts object (single class, no subclasses)
  opts <- structure(
    list(
      model = model,
      aggregation = aggregation
    ),
    class = "uncertainty_opts"
  )

  return(opts)
}

#' Print Method for Uncertainty Options
#'
#' @param x An object of class `uncertainty_opts`.
#' @param ... Additional arguments (not used).
#'
#' @returns Invisibly returns the input object.
#' @family uncertainty_opts
#' @export
#'
#' @examples
#' opts <- uncertainty_opts()
#' print(opts)
#'
#' opts_pois <- uncertainty_opts(model = uncertainty_poisson())
#' print(opts_pois)
print.uncertainty_opts <- function(x, ...) {
  cat("Uncertainty Configuration:\n")
  cat("\n")

  # Print model information
  cat("Model:\n")
  cat("  Family:  ", x$model$family, "\n", sep = "")
  cat("  Strategy:", attr(x$model$strategy, "name"), "\n")
  cat("\n")

  # Print aggregation information
  cat("Aggregation:\n")

  # Determine reference time aggregation description
  if (inherits(x$aggregation, "aggregation_observed")) {
    ref_desc <- "identity (no aggregation)"
  } else if (identical(x$aggregation$ref_time, identity)) {
    ref_desc <- "identity (no aggregation)"
  } else {
    ref_desc <- "custom function"
  }

  # Determine delay aggregation description
  # Check if delay function behaves like default rowSums by comparing bodies
  default_delay <- function(x) rowSums(x, na.rm = TRUE)
  if (identical(body(x$aggregation$delay), body(default_delay))) {
    delay_desc <- "rowSums (total per reference time)"
  } else {
    delay_desc <- "custom function"
  }

  cat("  Reference time: ", ref_desc, "\n", sep = "")
  cat("  Delay:          ", delay_desc, "\n", sep = "")

  invisible(x)
}
