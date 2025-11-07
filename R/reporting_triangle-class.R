#' Reporting Triangle Object
#' @name reporting_triangle-class
#' @aliases reporting_triangle
#' @family reporting_triangle
#'
#' @description
#' A `reporting_triangle` object contains the data and metadata needed for
#' nowcasting.
#'
#' @section Structure:
#' A `reporting_triangle` is a matrix with class
#' `c("reporting_triangle", "matrix")`:
#'
#' - Rows: Reference dates
#' - Columns: Delays (0, 1, 2, ...)
#' - Row names: Reference dates as character
#' - Column names: Delays as character
#'
#' Attributes:
#' - `reference_dates`: Vector of Date objects
#' - `delays_unit`: Character ("days", "weeks", "months", "years")
#' - `structure`: Integer vector indicating reporting pattern
#' See the corresponding [as_reporting_triangle.matrix()] and
#' [as_reporting_triangle.data.frame()] functions
#' for more details on the required input formats to generate the object.
NULL

#' Class constructor for `reporting_triangle` objects
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle
#' @inheritParams as_reporting_triangle.matrix
#' @inheritParams construct_triangle
#' @inheritParams as_reporting_triangle
#'
#' @returns An object of class [reporting_triangle]
#' @family reporting_triangle
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   structure,
                                   max_delay,
                                   delays_unit,
                                   strata = NULL) {
  .validate_rep_tri_args(
    reporting_triangle_matrix,
    reference_dates,
    structure,
    max_delay,
    delays_unit,
    strata
  )

  rownames(reporting_triangle_matrix) <- as.character(reference_dates)
  colnames(reporting_triangle_matrix) <- as.character(0:max_delay)

  result <- structure(
    reporting_triangle_matrix,
    class = c("reporting_triangle", "matrix"),
    reference_dates = reference_dates,
    delays_unit = delays_unit,
    structure = structure
  )
  return(result)
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @return NULL
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
assert_reporting_triangle <- function(data) {
  if (!inherits(data, "matrix")) {
    cli_abort(message = "data must be a matrix")
  }
  if (!inherits(data, "reporting_triangle")) {
    cli_abort(message = "data must have class 'reporting_triangle'")
  }

  reference_dates <- attr(data, "reference_dates")
  tri_structure <- attr(data, "structure")
  delays_unit <- attr(data, "delays_unit")
  max_delay <- ncol(data) - 1

  .validate_rep_tri_args(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    structure = tri_structure,
    max_delay = max_delay,
    delays_unit = delays_unit,
    strata = NULL
  )

  if (sum(tri_structure) > ncol(data)) {
    cli_abort(message = c(
      message = c(
        "Sum of `structure` must not be greater than or equal",
        "to the number of columns in matrix"
      )
    ))
  }

  return(NULL)
}

#' Print a reporting_triangle object
#'
#' @param x A [reporting_triangle] object to print.
#' @param ... Additional arguments passed to print methods.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @method print reporting_triangle
print.reporting_triangle <- function(x, ...) {
  cat("Reporting Triangle\n")
  cat("------------------\n")
  cat("Delays unit:", attr(x, "delays_unit"), "\n")
  ref_dates <- attr(x, "reference_dates")
  cat(
    "Reference dates:",
    paste(format(range(ref_dates)), collapse = " to "), "\n"
  )
  cat("Max delay:", ncol(x) - 1, "\n")
  cat(
    "Structure:", toString(attr(x, "structure")), "\n\n"
  )
  print(unclass(x), ...)
  return(invisible(x))
}

#' Summarize a reporting_triangle object
#'
#' @param object A [reporting_triangle] object to summarize.
#' @param ... Additional arguments not used.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @method summary reporting_triangle
summary.reporting_triangle <- function(object, ...) {
  cat("Reporting Triangle Summary\n")
  cat("==========================\n")
  cat("Dimensions:", nrow(object), "x", ncol(object), "\n")
  ref_dates <- attr(object, "reference_dates")
  cat(
    "Reference period:",
    paste(format(range(ref_dates)), collapse = " to "), "\n"
  )
  cat(
    "Max delay:", ncol(object) - 1, attr(object, "delays_unit"), "\n"
  )
  cat("Total cases:", sum(object, na.rm = TRUE), "\n")
  cat("Missing cells:", sum(is.na(object)), "\n")
  cat(
    "Structure:", toString(attr(object, "structure")),
    "\n"
  )
  return(invisible(object))
}
