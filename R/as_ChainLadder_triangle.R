#' Convert reporting_triangle to ChainLadder triangle format
#'
#' This function converts a \code{\link{reporting_triangle}} object to a
#' ChainLadder triangle object, enabling use of ChainLadder's functionality for
#' analysing reporting triangles.
#' The ChainLadder package provides mature methods for working with reporting
#' triangles, including chain ladder reserving methods, bootstrap techniques,
#' and diagnostic plots.
#'
#' @param x A \code{\link{reporting_triangle}} object to convert.
#' @param ... Additional arguments (not currently used).
#'
#' @return A ChainLadder triangle object (class "triangle" and "matrix").
#'   The triangle will have the same structure as the input
#'   \code{reporting_triangle_matrix}, with rows representing origin periods
#'   (reference dates) and columns representing development periods (delays).
#'   Row names will be set to the reference dates from the input object.
#'
#' @details
#' This function extracts the \code{reporting_triangle_matrix} component from a
#' \code{\link{reporting_triangle}} object and converts it to ChainLadder's
#' triangle format using \code{ChainLadder::as.triangle()}.
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
#' \dontrun{
#' # Create a reporting triangle from synthetic NSSP data
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#'
#' # Convert to ChainLadder triangle format (requires ChainLadder package)
#' cl_triangle <- as_ChainLadder_triangle(rep_tri)
#'
#' # Now you can use ChainLadder methods, for example:
#' # summary(cl_triangle)
#' # plot(cl_triangle)
#' # MackChainLadder(cl_triangle)
#'
#' # Convert back to reporting_triangle format
#' rep_tri_restored <- as_reporting_triangle(
#'   data = cl_triangle,
#'   max_delay = rep_tri$max_delay,
#'   reference_dates = rep_tri$reference_dates
#' )
#' }
as_ChainLadder_triangle <- function(x, ...) {
  # Check that ChainLadder is available
  if (!requireNamespace("ChainLadder", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ChainLadder} is required to convert to ChainLadder
        triangle format.",
        i = "Install it with: {.code install.packages(\"ChainLadder\")}"
      )
    )
  }

  # Validate input
  assert_reporting_triangle(x)

  # Extract the reporting triangle matrix
  triangle_matrix <- x$reporting_triangle_matrix

  # Convert to ChainLadder triangle format
  cl_triangle <- ChainLadder::as.triangle(triangle_matrix)

  return(cl_triangle)
}

#' Convert ChainLadder triangle to reporting_triangle format
#'
#' This S3 method converts a ChainLadder triangle object to a
#' \code{\link{reporting_triangle}} object, enabling use of baselinenowcast's
#' nowcasting methods.
#'
#' @param data A ChainLadder triangle object (class "triangle").
#' @param max_delay Integer indicating the maximum delay.
#'   Must be greater than or equal to 0.
#' @param strata Character string indicating the strata.
#'   Default is NULL.
#' @param delays_unit Character string specifying the temporal granularity of
#'   the delays.
#'   Options are "days", "weeks", "months", "years".
#'   Default is "days".
#' @param reference_dates Vector of dates corresponding to the rows of the
#'   triangle. Must have the same length as the number of rows in the triangle.
#'   If the triangle has row names that can be coerced to dates, these will be
#'   used automatically if \code{reference_dates} is not provided.
#'   If row names cannot be coerced to dates and \code{reference_dates} is not
#'   provided, an error will be raised.
#' @param ... Additional arguments (not currently used).
#'
#' @return A \code{\link{reporting_triangle}} object containing:
#'   \itemize{
#'     \item \code{reporting_triangle_matrix}: The reporting triangle matrix
#'     \item \code{reference_dates}: Vector of reference dates
#'     \item \code{structure}: Structure of the reporting triangle
#'     \item \code{max_delay}: Maximum delay
#'     \item \code{delays_unit}: Unit of delays
#'     \item \code{strata}: Strata information (if provided)
#'   }
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
#' \dontrun{
#' # Create a reporting triangle
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#'
#' # Convert to ChainLadder triangle
#' cl_triangle <- as_ChainLadder_triangle(rep_tri)
#'
#' # Convert back to reporting_triangle
#' rep_tri_2 <- as_reporting_triangle(
#'   data = cl_triangle,
#'   max_delay = 25,
#'   reference_dates = rep_tri$reference_dates
#' )
#'
#' # Alternative: if row names are dates, reference_dates can be inferred
#' rownames(cl_triangle) <- as.character(rep_tri$reference_dates)
#' rep_tri_3 <- as_reporting_triangle(
#'   data = cl_triangle,
#'   max_delay = 25
#' )
#' }
as_reporting_triangle.triangle <- function(data,
                                           max_delay,
                                           strata = NULL,
                                           delays_unit = "days",
                                           reference_dates = NULL,
                                           ...) {
  # Check that ChainLadder is available
  if (!requireNamespace("ChainLadder", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg ChainLadder} is required to convert from ChainLadder
        triangle format.",
        i = "Install it with: {.code install.packages(\"ChainLadder\")}"
      )
    )
  }

  # Try to extract reference dates from row names if not provided
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

  # Convert triangle to plain matrix
  triangle_matrix <- as.matrix(data)

  # Use the existing matrix method
  rep_tri <- as_reporting_triangle.matrix(
    data = triangle_matrix,
    max_delay = max_delay,
    reference_dates = reference_dates,
    strata = strata,
    delays_unit = delays_unit
  )

  return(rep_tri)
}
