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
#' A `reporting_triangle` is a list with the following components:
#' \describe{
#'  \item{reporting_triangle_matrix}{Matrix with which rows are reference
#'    times and columns are delays and
#'    entries are incident cases at each reference time and delay.}
#'  \item{reference_dates}{Vector of the same length as the rows of the
#'    matrix indicating the dates corresponding to the reference times in the
#'    rows of the reporting triangle.}
#'  \item{structure}{Vector indicating the "structure" of the reporting
#'    triangle, see [construct_triangle()] for more details.
#'    Use [get_structure()] to compute this dynamically from the matrix.}
#'  \item{max_delay}{Integer indicating the maximum delay.
#'    Use [get_max_delay()] to compute this dynamically from the matrix.}
#'  \item{delays_unit}{Character string indicating the unit of the delays.
#'     Valid options are "days", "weeks", "months", "years".}
#'  \item{strata}{Character string indicating the strata.}
#' }
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
    delays_unit
  )
  result <- structure(
    list(
      reporting_triangle_matrix = reporting_triangle_matrix,
      reference_dates = reference_dates,
      structure = structure,
      max_delay = max_delay,
      delays_unit = delays_unit,
      strata = strata
    ),
    class = "reporting_triangle"
  )
  return(result)
}

#' Get maximum delay from a reporting triangle
#'
#' @param x A [reporting_triangle] object.
#' @return Integer indicating the maximum delay.
#' @family reporting_triangle
#' @export
get_max_delay <- function(x) {
  if (inherits(x, "reporting_triangle")) {
    return(ncol(x$reporting_triangle_matrix) - 1)
  } else {
    return(ncol(x) - 1)
  }
}

#' Get structure from a reporting triangle
#'
#' @param x A [reporting_triangle] object.
#' @return Integer or vector specifying the reporting structure.
#' @family reporting_triangle
#' @export
get_structure <- function(x) {
  if (inherits(x, "reporting_triangle")) {
    return(get_reporting_structure(x$reporting_triangle_matrix))
  } else {
    return(get_reporting_structure(x))
  }
}

#' Get delays unit from a reporting triangle
#'
#' @param x A [reporting_triangle] object.
#' @return Character string indicating the delays unit.
#' @family reporting_triangle
#' @export
get_delay_unit <- function(x) {
  if (inherits(x, "reporting_triangle")) {
    return(x$delays_unit)
  }
  cli_abort("x must be a reporting_triangle object")
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @return NULL invisibly
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_class
assert_reporting_triangle <- function(data) {
  assert_class(data, "reporting_triangle")
  return(NULL)
}
