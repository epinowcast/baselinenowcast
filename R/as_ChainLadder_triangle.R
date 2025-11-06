#' Convert reporting_triangle to ChainLadder triangle format
#'
#' This function converts a \code{\link{reporting_triangle}} object to a
#' triangle object from the
#' \href{https://CRAN.R-project.org/package=ChainLadder}{ChainLadder} package.
#' ChainLadder is a mature package for claims reserving in general insurance
#' that provides statistical methods for analysing reporting triangles,
#' including the chain ladder technique, bootstrap methods, and diagnostic
#' tools.
#' Converting to ChainLadder format enables use of these specialized methods
#' alongside baselinenowcast's nowcasting functionality.
#'
#' @param x A \code{\link{reporting_triangle}} object to convert.
#' @param ... Additional arguments passed to
#'   \code{ChainLadder::as.triangle()}.
#'
#' @return A ChainLadder triangle object (class "triangle" and "matrix"),
#'   with rows representing origin periods (reference dates) and columns
#'   representing development periods (delays).
#'   Row names are set to the reference dates for seamless round-trip
#'   conversion.
#'
#' @details
#' This function converts the reporting triangle to ChainLadder's triangle
#' format using \code{ChainLadder::as.triangle()}.
#'
#' The ChainLadder package must be installed to use this function.
#' If ChainLadder is not available, an informative error message will be
#' displayed with installation instructions.
#'
#' Once converted, you can use any ChainLadder methods such as:
#' \itemize{
#'   \item \code{MackChainLadder()} for the Mack chain ladder method
#'   \item \code{BootChainLadder()} for bootstrap chain ladder
#'   \item Standard plotting and summary methods
#' }
#'
#' To convert back to a \code{\link{reporting_triangle}} object, use
#' \code{\link{as_reporting_triangle.triangle}()}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{as_reporting_triangle.triangle}} for converting back
#'   \item ChainLadder package documentation:
#'     \url{https://mages.github.io/ChainLadder/}
#' }
#'
#' @family reporting_triangle
#' @export
#'
#' @examples
#' # Create a reporting triangle from synthetic NSSP data
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#'
#' # Convert to ChainLadder triangle format (requires ChainLadder package)
#' if (requireNamespace("ChainLadder", quietly = TRUE)) {
#'   cl_triangle <- as_ChainLadder_triangle(rep_tri)
#'
#'   # Now you can use ChainLadder methods, for example:
#'   summary(cl_triangle)
#'   plot(cl_triangle)
#' }
as_ChainLadder_triangle <- function(x, ...) {
  if (!requireNamespace("ChainLadder", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ChainLadder} is required to convert to ChainLadder
        triangle format.",
        i = "Install it with: {.code install.packages(\"ChainLadder\")}"
      )
    )
  }

  assert_reporting_triangle(x)

  triangle_matrix <- x$reporting_triangle_matrix
  rownames(triangle_matrix) <- as.character(x$reference_dates)

  cl_triangle <- ChainLadder::as.triangle(triangle_matrix, ...)

  return(cl_triangle)
}

#' Convert ChainLadder triangle to reporting_triangle format
#'
#' This S3 method converts a ChainLadder triangle object to a
#' \code{\link{reporting_triangle}} object, enabling use of baselinenowcast's
#' nowcasting methods.
#'
#' @param data A ChainLadder triangle object (class "triangle").
#' @param reference_dates Vector of dates corresponding to the rows of the
#'   triangle.
#'   If not provided, will attempt to coerce row names to dates.
#'   If row names cannot be coerced to dates and this is not provided,
#'   an error will be raised.
#' @inheritParams as_reporting_triangle
#' @param ... Additional arguments (not currently used).
#'
#' @return A \code{\link{reporting_triangle}} object.
#'   See \code{\link{reporting_triangle-class}} for details on the structure.
#'
#' @details
#' This method converts a ChainLadder triangle back to baselinenowcast's
#' \code{\link{reporting_triangle}} format.
#' Since ChainLadder triangles do not store date information by default, you
#' must provide \code{reference_dates} that correspond to the origin periods
#' (rows) of the triangle, or ensure the triangle has row names that can be
#' coerced to dates.
#'
#' The ChainLadder package must be installed to use this function.
#'
#' The conversion uses \code{\link{as_reporting_triangle.matrix}()} internally
#' after extracting the matrix from the ChainLadder triangle object.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{as_ChainLadder_triangle}} for converting to ChainLadder
#'   \item \code{\link{as_reporting_triangle.matrix}} for the underlying method
#'   \item \code{\link{as_reporting_triangle.data.frame}} for creating from
#'     data frames
#' }
#'
#' @family reporting_triangle
#' @export
#' @method as_reporting_triangle triangle
#'
#' @examples
#' # Create a reporting triangle
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#'
#' if (requireNamespace("ChainLadder", quietly = TRUE)) {
#'   # Convert to ChainLadder triangle
#'   cl_triangle <- as_ChainLadder_triangle(rep_tri)
#'
#'   # Convert back to reporting_triangle (seamless round-trip)
#'   rep_tri_2 <- as_reporting_triangle(
#'     data = cl_triangle,
#'     max_delay = 25
#'   )
#' }
as_reporting_triangle.triangle <- function(data,
                                           max_delay,
                                           strata = NULL,
                                           delays_unit = "days",
                                           reference_dates = NULL,
                                           ...) {
  if (!requireNamespace("ChainLadder", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ChainLadder} is required to convert from ChainLadder
        triangle format.",
        i = "Install it with: {.code install.packages(\"ChainLadder\")}"
      )
    )
  }

  if (is.null(reference_dates)) {
    if (!is.null(rownames(data))) {
      reference_dates <- tryCatch(
        as.Date(rownames(data)),
        error = function(e) NULL
      )
    }

    if (is.null(reference_dates)) {
      cli::cli_abort(
        c(
          "{.arg reference_dates} must be provided when converting from
          ChainLadder triangle.",
          i = "ChainLadder triangles do not store date information by
          default. Provide a vector of dates with length equal to the number
          of rows in the triangle."
        )
      )
    }
  }

  triangle_matrix <- unclass(as.matrix(data))
  dimnames(triangle_matrix) <- dimnames(data)

  rep_tri <- as_reporting_triangle.matrix(
    data = triangle_matrix,
    max_delay = max_delay,
    reference_dates = reference_dates,
    strata = strata,
    delays_unit = delays_unit
  )

  return(rep_tri)
}
