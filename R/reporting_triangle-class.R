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
#'    matrix indicating the ates corresponding to the reference times in the
#'    rows of the reporting triangle.}
#'  \item{structure}{Vector indicating the "structure" of the reporting
#'    triangle, see \code{\link{construct_triangle}} for more details.}
#'  \item{max_delay}{Integer indicating the maximum delay.}
#'  \item{delays_unit}{Character string indicating the unit of the delays.
#'     Valid options are "days", "weeks", "months", "years".}
#'  \item{strata}{Character string indicating the strata}
#' }
#' See the corresponding \code{\link{as_reporting_triangle.matrix}} and
#' \code{\link{as_reporting_triangle.data.frame}} functions
#' for more details on the required input formats to genereate the object.
NULL
