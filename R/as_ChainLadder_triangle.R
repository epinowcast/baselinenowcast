#' Convert reporting_triangle to ChainLadder triangle format
#'
#' This function converts a [reporting_triangle] object to a triangle object
#' from the [ChainLadder](https://CRAN.R-project.org/package=ChainLadder)
#' package.
#' ChainLadder is a mature package for claims reserving in general insurance
#' that provides statistical methods for analysing reporting triangles,
#' including the chain ladder technique, bootstrap methods, and diagnostic
#' tools.
#' Converting to ChainLadder format enables use of these specialized methods
#' alongside baselinenowcast's nowcasting functionality.
#'
#' @param x A [reporting_triangle] object to convert.
#' @param ... Additional arguments passed to [ChainLadder::as.triangle()].
#'
#' @return A ChainLadder triangle object (class "triangle" and "matrix"),
#'   with rows representing origin periods (reference dates) and columns
#'   representing development periods (delays).
#'
#' @details
#' This function converts the reporting triangle to ChainLadder's triangle
#' format using [ChainLadder::as.triangle()].
#' The ChainLadder package must be installed to use this function.
#'
#' Once converted, you can use any ChainLadder methods such as:
#' - [ChainLadder::MackChainLadder()] for the Mack chain ladder method
#' - [ChainLadder::BootChainLadder()] for bootstrap chain ladder
#' - Standard plotting and summary methods
#'
#' To convert back to a [reporting_triangle] object, use
#' [as_reporting_triangle.triangle()].
#'
#' @seealso
#' - [as_reporting_triangle.triangle()] for converting back
#' - [ChainLadder package documentation](https://mages.github.io/ChainLadder/)
#'
#' @family reporting_triangle
#' @export
#'
#' @examplesIf requireNamespace("ChainLadder", quietly = TRUE)
#' # Create a reporting triangle from synthetic NSSP data
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#'
#' # Convert to ChainLadder triangle format
#' cl_triangle <- as_ChainLadder_triangle(rep_tri)
#' print(cl_triangle)
#'
#' # Use ChainLadder's Mack chain ladder method for nowcasting
#' mack_result <- ChainLadder::MackChainLadder(cl_triangle)
#' print(mack_result)
#'
#' # Plot the results
#' plot(mack_result)
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
#' [reporting_triangle] object, enabling use of baselinenowcast's nowcasting
#' methods.
#'
#' @param data A ChainLadder triangle object (class "triangle").
#' @param reference_dates Vector of dates corresponding to the rows of the
#'   triangle.
#'   If not provided, will attempt to coerce row names to dates.
#'   If row names cannot be coerced to dates and this is not provided,
#'   an error will be raised.
#' @inheritParams as_reporting_triangle
#' @param ... Additional arguments passed to [as_reporting_triangle.matrix()].
#'
#' @return A [reporting_triangle] object.
#'   See [reporting_triangle-class] for details on the structure.
#'
#' @details
#' This method converts a ChainLadder triangle back to baselinenowcast's
#' [reporting_triangle] format.
#' If `reference_dates` is not provided, the function will attempt to extract
#' dates from the triangle's row names.
#'
#' The ChainLadder package must be installed to use this function.
#'
#' The conversion uses [as_reporting_triangle.matrix()] internally after
#' extracting the matrix from the ChainLadder triangle object.
#'
#' @seealso
#' - [as_ChainLadder_triangle()] for converting to ChainLadder
#' - [as_reporting_triangle.matrix()] for the underlying method
#' - [as_reporting_triangle.data.frame()] for creating from data frames
#'
#' @family reporting_triangle
#' @export
#' @method as_reporting_triangle triangle
#'
#' @examplesIf requireNamespace("ChainLadder", quietly = TRUE)
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
#' # Convert back to reporting_triangle (seamless round-trip)
#' rep_tri_2 <- as_reporting_triangle(
#'   data = cl_triangle,
#'   max_delay = 25
#' )
#' print(rep_tri_2)
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
    delays_unit = delays_unit,
    ...
  )

  return(rep_tri)
}
