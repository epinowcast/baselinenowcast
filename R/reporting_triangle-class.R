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
#' - Entries: Incident cases at each reference date and delay
#' - Row names: Reference dates as character
#' - Column names: Delays as character
#'
#' Attributes:
#' - `delays_unit`: Character ("days", "weeks", "months", "years")
#'
#' Reference dates are stored as row names and can be extracted using
#' [get_reference_dates()].
#' The maximum delay can be obtained using [get_max_delay()].
#' The structure can be computed using [get_reporting_structure()].
#' See the corresponding [as_reporting_triangle.matrix()] and
#' [as_reporting_triangle.data.frame()] functions
#' for more details on the required input formats to generate the object.
#'
#' @section Working with reporting triangles:
#' Reporting triangle objects provide:
#'
#' **Inspection and display:**
#' - `print()`: Informative display with metadata
#' - `summary()`: Statistics including completion, delays, and zeros
#' - `head()`, `tail()`: Extract first or last rows
#' - Standard matrix operations: `rowSums()`, `colSums()`
#'
#' **Subsetting and modification:**
#' - `[` and `[<-`: Extract or assign values with automatic validation
#' - Subsetting preserves class and attributes when result is a matrix
#'
#' **Package functions:**
#' - [estimate_and_apply_delay()]: Estimate delay and generate point nowcast
#' - [estimate_delay()]: Extract delay distribution from triangle
#' - [apply_delay()]: Apply delay distribution for nowcasting
#' - [truncate_to_row()]: Remove most recent rows
#' - [preprocess_negative_values()]: Handle reporting corrections
#'
#' @examples
#' # Create a reporting triangle from data
#' data <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data = data)
#'
#' # Use with low-level functions
#' filled <- estimate_and_apply_delay(rep_tri)
#' delay_pmf <- estimate_delay(rep_tri)
#' nowcast <- apply_delay(rep_tri, delay_pmf)
#'
#' # Direct matrix operations
#' total_by_date <- rowSums(rep_tri, na.rm = TRUE)
#' total_by_delay <- colSums(rep_tri, na.rm = TRUE)
#'
#' # Subsetting and inspection
#' recent <- tail(rep_tri, n = 10)
#' summary(rep_tri)
#'
#' @return A `reporting_triangle` object. This is a matrix subclass containing
#'   case counts indexed by reference date (rows) and delay (columns). See the
#'   Structure section for details on the object format.
NULL

#' Class constructor for `reporting_triangle` objects
#'
#' Creates a new reporting_triangle object from a matrix.
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle where rows
#'   are reference times, columns are delays, and entries are incident counts.
#' @param reference_dates Vector of Date objects indicating the reference dates
#'   corresponding to each row of the matrix.
#' @inheritParams as_reporting_triangle
#'
#' @returns An object of class [reporting_triangle]
#' @family reporting_triangle
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   delays_unit) {
  .validate_rep_tri_args(
    reporting_triangle_matrix,
    reference_dates,
    delays_unit
  )

  max_delay <- ncol(reporting_triangle_matrix) - 1L
  rownames(reporting_triangle_matrix) <- as.character(reference_dates)
  colnames(reporting_triangle_matrix) <- as.character(0:max_delay)

  result <- structure(
    reporting_triangle_matrix,
    class = c("reporting_triangle", "matrix"),
    delays_unit = delays_unit
  )

  return(result)
}

#' Check if an object is a reporting_triangle
#'
#' @param x An object to check.
#' @return Logical indicating whether the object is a reporting_triangle.
#' @family reporting_triangle
#' @export
is_reporting_triangle <- function(x) {
  return(inherits(x, "reporting_triangle"))
}

#' Update reporting_triangle with new matrix data
#'
#' Internal helper to create a new reporting_triangle from modified matrix
#' data while preserving the original object's metadata (reference dates,
#' delays_unit).
#' This simplifies the pattern of converting to matrix, operating on it, then
#' restoring the reporting_triangle class and attributes.
#'
#' @param reporting_triangle The original [reporting_triangle] object.
#' @param new_matrix The modified matrix data.
#' @param reference_dates Optional. Reference dates for the new matrix. If NULL,
#'   extracts from the original reporting_triangle object.
#' @return A new [reporting_triangle] object with the updated matrix data.
#' @keywords internal
.update_triangle_matrix <- function(reporting_triangle, new_matrix,
                                    reference_dates = NULL) {
  if (is.null(reference_dates)) {
    reference_dates <- get_reference_dates(reporting_triangle)
  }

  return(new_reporting_triangle(
    reporting_triangle_matrix = new_matrix,
    reference_dates = reference_dates,
    delays_unit = get_delays_unit(reporting_triangle)
  ))
}
