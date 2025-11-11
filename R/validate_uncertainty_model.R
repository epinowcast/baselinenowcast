#' Validate Uncertainty Model
#'
#' @description
#' S3 generic function to validate uncertainty model objects.
#' Uses method dispatch to perform appropriate validation based on the
#' model class.
#'
#' @param model Object to validate.
#' @param ... Additional arguments passed to methods.
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family uncertainty_model
#' @export
#'
#' @examples
#' # Validate a negative binomial model
#' model <- uncertainty_nb()
#' assert_uncertainty_model(model)
#'
#' # Validate a Poisson model
#' model_pois <- uncertainty_poisson()
#' assert_uncertainty_model(model_pois)
assert_uncertainty_model <- function(model, ...) {
  UseMethod("assert_uncertainty_model")
}

#' Default Validation for Uncertainty Models
#'
#' @description
#' Default method that throws an error if the object is not an
#' uncertainty_model.
#'
#' @inheritParams assert_uncertainty_model
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family uncertainty_model
#' @export
#' @importFrom cli cli_abort
assert_uncertainty_model.default <- function(model, ...) {
  cli_abort(c(
    "Invalid uncertainty model",
    x = "Object must be of class 'uncertainty_model'",
    i = "Got class: {class(model)[1]}",
    i = paste(
      "Use uncertainty_nb(), uncertainty_poisson(), or",
      "uncertainty_model() to create valid models"
    )
  ))
}

#' Base Validation for Uncertainty Models
#'
#' @description
#' Base validation method for all uncertainty_model objects.
#' Checks for required components (fit, sample, family, strategy).
#'
#' @inheritParams assert_uncertainty_model
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family uncertainty_model
#' @export
#' @importFrom checkmate assert_list assert_function assert_character
#' @importFrom cli cli_abort
assert_uncertainty_model.uncertainty_model <- function(model, ...) {
  # Check basic structure
  checkmate::assert_list(model)

  # Check required components
  required_components <- c("fit", "sample", "family", "strategy")
  missing_components <- setdiff(required_components, names(model))

  if (length(missing_components) > 0) {
    cli_abort(c(
      "Invalid uncertainty model structure",
      x = "Missing required component{?s}: {missing_components}"
    ))
  }

  # Validate component types
  checkmate::assert_function(model$fit)
  checkmate::assert_function(model$sample)
  checkmate::assert_character(model$family, len = 1)

  # Validate strategy
  assert_uncertainty_strategy(model$strategy)

  return(invisible(NULL))
}

#' Validate Negative Binomial Uncertainty Model
#'
#' @description
#' Validates that the model is correctly configured as a negative binomial
#' uncertainty model.
#'
#' @inheritParams assert_uncertainty_model
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family uncertainty_model
#' @export
#' @importFrom cli cli_abort
assert_uncertainty_model.uncertainty_nb <- function(model, ...) {
  # Call base validation first
  NextMethod()

  # Check that family is "nb"
  if (model$family != "nb") {
    cli_abort(c(
      "Invalid negative binomial model",
      x = "Family must be 'nb' for uncertainty_nb objects",
      i = "Got family: '{model$family}'"
    ))
  }

  return(invisible(NULL))
}

#' Validate Poisson Uncertainty Model
#'
#' @description
#' Validates that the model is correctly configured as a Poisson
#' uncertainty model.
#'
#' @inheritParams assert_uncertainty_model
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family uncertainty_model
#' @export
#' @importFrom cli cli_abort
assert_uncertainty_model.uncertainty_poisson <- function(model, ...) {
  # Call base validation first
  NextMethod()

  # Check that family is "poisson"
  if (model$family != "poisson") {
    cli_abort(c(
      "Invalid Poisson model",
      x = "Family must be 'poisson' for uncertainty_poisson objects",
      i = "Got family: '{model$family}'"
    ))
  }

  return(invisible(NULL))
}
