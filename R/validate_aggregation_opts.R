#' Validate Aggregation Options
#'
#' @description
#' S3 generic function to validate aggregation options objects.
#' Uses method dispatch to perform appropriate validation based on the
#' aggregation class.
#'
#' @param aggregation Object to validate.
#' @param test_data Optional matrix to test that the aggregation functions
#'   work correctly. If provided, both `ref_time` and `delay` functions
#'   will be applied to the test data to verify they execute without error.
#' @param ... Additional arguments passed to methods.
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family aggregation_opts
#' @export
#'
#' @examples
#' # Validate standard observed aggregation
#' agg <- aggregation_observed()
#' assert_aggregation_opts(agg)
#'
#' # Validate with test data
#' test_matrix <- matrix(1:12, nrow = 3, ncol = 4)
#' assert_aggregation_opts(agg, test_data = test_matrix)
#'
#' # Validate custom aggregation
#' agg_custom <- aggregation_opts(
#'   ref_time = identity,
#'   delay = function(x) colSums(x, na.rm = TRUE)
#' )
#' assert_aggregation_opts(agg_custom, test_data = test_matrix)
assert_aggregation_opts <- function(aggregation,
                                    test_data = NULL,
                                    ...) {
  UseMethod("assert_aggregation_opts")
}

#' Default Validation for Aggregation Options
#'
#' @description
#' Default method that throws an error if the object is not an
#' aggregation_opts object.
#'
#' @inheritParams assert_aggregation_opts
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family aggregation_opts
#' @export
#' @importFrom cli cli_abort
assert_aggregation_opts.default <- function(aggregation,
                                            test_data = NULL,
                                            ...) {
  cli_abort(c(
    "Invalid aggregation options",
    x = "Object must be of class 'aggregation_opts'",
    i = "Got class: {class(aggregation)[1]}",
    i = paste(
      "Use aggregation_observed() or aggregation_opts()",
      "to create valid aggregation options"
    )
  ))
}

#' Base Validation for Aggregation Options
#'
#' @description
#' Base validation method for all aggregation_opts objects.
#' Checks for required components (ref_time, delay) and optionally tests
#' that the functions work correctly with test data.
#'
#' @inheritParams assert_aggregation_opts
#'
#' @returns NULL (invisibly) if valid, otherwise throws an error.
#' @family aggregation_opts
#' @export
#' @importFrom checkmate assert_list assert_function assert_matrix
#' @importFrom cli cli_abort
assert_aggregation_opts.aggregation_opts <- function(aggregation,
                                                     test_data = NULL,
                                                     ...) {
  # Check basic structure
  checkmate::assert_list(aggregation)

  # Check required components
  required_components <- c("ref_time", "delay")
  missing_components <- setdiff(required_components, names(aggregation))

  if (length(missing_components) > 0) {
    cli_abort(c(
      "Invalid aggregation options structure",
      x = "Missing required component{?s}: {missing_components}"
    ))
  }

  # Validate component types
  checkmate::assert_function(aggregation$ref_time)
  checkmate::assert_function(aggregation$delay)

  # If test data provided, validate that functions work
  if (!is.null(test_data)) {
    checkmate::assert_matrix(test_data)

    # Test ref_time function
    result_ref <- tryCatch(
      aggregation$ref_time(test_data),
      error = function(e) {
        cli_abort(c(
          "ref_time aggregation function failed on test data",
          x = "Error: {conditionMessage(e)}"
        ))
      }
    )

    if (!is.matrix(result_ref) && !is.array(result_ref)) {
      cli_abort(c(
        "Invalid ref_time aggregation function",
        x = "Function must return a matrix or array",
        i = "Got class: {class(result_ref)[1]}"
      ))
    }

    # Test delay function
    result_delay <- tryCatch(
      aggregation$delay(test_data),
      error = function(e) {
        cli_abort(c(
          "delay aggregation function failed on test data",
          x = "Error: {conditionMessage(e)}"
        ))
      }
    )

    if (!is.numeric(result_delay)) {
      cli_abort(c(
        "Invalid delay aggregation function",
        x = "Function must return a numeric vector or matrix",
        i = "Got class: {class(result_delay)[1]}"
      ))
    }
  }

  return(invisible(NULL))
}
