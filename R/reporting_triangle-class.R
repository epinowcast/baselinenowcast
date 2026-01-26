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
NULL

#' Validate reporting_triangle constructor arguments
#'
#' Internal helper to validate the arguments passed to new_reporting_triangle.
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle data.
#' @param reference_dates Date vector of reference dates.
#' @inheritParams as_reporting_triangle
#' @return NULL
#' @keywords internal
.validate_rep_tri_args <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   delays_unit) {
  assert_matrix(reporting_triangle_matrix)
  assert_date(reference_dates,
    unique = TRUE,
    null.ok = FALSE,
    min.len = 1,
    len = nrow(reporting_triangle_matrix)
  )

  assert_delays_unit(delays_unit)
  return(NULL)
}

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

#' Check NA pattern validity in reporting triangle
#'
#' Internal function that validates NA positions. Identifies NA values that
#' don't follow the expected triangular reporting delay pattern. Out-of-pattern
#' NAs occur when a value is NA but values below it (later reference dates) or
#' to its right (longer delays) are non-NA, suggesting data quality issues
#' rather than reporting delay.
#'
#' @param x A matrix or [reporting_triangle] object.
#' @return A list with components:
#'   - `valid`: Logical indicating if all NAs are in valid bottom-right pattern
#'   - `n_out_of_pattern`: Count of out-of-pattern NA values
#'
#' @details
#' An NA is considered "out-of-pattern" if:
#' - There exists a non-NA value in the same column but a later row
#'   (indicating data should have been available), OR
#' - There exists a non-NA value in the same row but a later column
#'   (indicating earlier delays were reported)
#'
#' Uses a vectorised cummax-based approach for efficient validation.
#'
#' @keywords internal
#' Check rows for out-of-pattern NAs
#' @noRd
.check_na_rows <- function(na_positions, out_of_pattern) {
  nr <- nrow(na_positions)
  nc <- ncol(na_positions)

  for (i in seq_len(nr)) {
    row_na <- na_positions[i, ]
    na_indices <- which(row_na)

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(row_na[min_na_idx:nc])) {
        for (j in min_na_idx:nc) {
          if (j < nc && na_positions[i, j] && !all(row_na[(j + 1):nc])) {
            out_of_pattern[i, j] <- TRUE
          }
        }
      }
    }
  }
  return(out_of_pattern)
}

#' Check columns for out-of-pattern NAs
#' @noRd
.check_na_cols <- function(na_positions, out_of_pattern) {
  nr <- nrow(na_positions)
  nc <- ncol(na_positions)

  for (j in seq_len(nc)) {
    col_na <- na_positions[, j]
    na_indices <- which(col_na)

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(col_na[min_na_idx:nr])) {
        for (i in min_na_idx:nr) {
          if (i < nr && na_positions[i, j] && !all(col_na[(i + 1):nr])) {
            out_of_pattern[i, j] <- TRUE
          }
        }
      }
    }
  }
  return(out_of_pattern)
}

.check_na_pattern <- function(x) {
  # Convert to matrix if needed
  mat <- if (inherits(x, "reporting_triangle")) {
    as.matrix(x)
  } else {
    x
  }

  # Early return if no NAs
  if (!anyNA(mat)) {
    return(list(
      valid = TRUE,
      n_out_of_pattern = 0L
    ))
  }

  # Track which NA positions are out of pattern
  out_of_pattern <- matrix(FALSE, nrow = nrow(mat), ncol = ncol(mat))
  na_positions <- is.na(mat)

  # Check rows and columns
  out_of_pattern <- .check_na_rows(na_positions, out_of_pattern)
  out_of_pattern <- .check_na_cols(na_positions, out_of_pattern)

  n_out_of_pattern <- sum(out_of_pattern)

  return(list(
    valid = n_out_of_pattern == 0L,
    n_out_of_pattern = n_out_of_pattern
  ))
}

#' Validate a reporting_triangle object
#'
#' @param data A [reporting_triangle] object to validate
#' @return The validated object (invisibly) or throws error
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
validate_reporting_triangle <- function(data) {
  if (!inherits(data, "matrix")) {
    cli_abort(message = "data must be a matrix")
  }
  if (!inherits(data, "reporting_triangle")) {
    cli_abort(message = "data must have class 'reporting_triangle'")
  }

  # Check matrix is not all NA
  if (all(is.na(data))) {
    cli_abort(message = "Matrix cannot be all NA")
  }

  # Check NA pattern (IF NAs exist, THEN must be in bottom-right pattern)
  if (anyNA(data)) {
    na_check <- .check_na_pattern(data)
    if (!na_check$valid) {
      n_oop <- na_check$n_out_of_pattern
      cli_abort(
        message = c(
          `!` = "Invalid reporting triangle structure",
          x = paste0(
            "Found ", n_oop, " NA value",
            if (n_oop != 1) "s" else "",
            " in unexpected position",
            if (n_oop != 1) "s" else ""
          ),
          i = paste0(
            "NA values should only appear in the bottom right ",
            "portion of the triangle"
          )
        )
      )
    }
  }

  reference_dates <- as.Date(rownames(data))
  delays_unit <- get_delays_unit(data)

  .validate_rep_tri_args(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    delays_unit = delays_unit
  )

  return(invisible(data))
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @param validate Logical. If TRUE (default), validates the object. Set to
#'   FALSE only when called from functions that already validated.
#' @return Returns `NULL` invisibly. Throws an error if validation fails.
#' @family reporting_triangle
#' @examples
#' # Validate an example reporting triangle
#' assert_reporting_triangle(example_reporting_triangle)
#'
#' @export
assert_reporting_triangle <- function(data, validate = TRUE) {
  if (isTRUE(validate)) {
    validate_reporting_triangle(data)
  }
  return(NULL)
}

#' Assert object has reporting_triangle class
#'
#' Lightweight class check without full validation. Use when validation
#' will occur through other operations (e.g., subsetting via `[`).
#'
#' @param data Object to check for reporting_triangle class.
#' @return NULL
#' @keywords internal
#' @noRd
assert_rep_tri_class <- function(data) {
  if (!is_reporting_triangle(data)) {
    cli_abort(
      message = "data must have class 'reporting_triangle'"
    )
  }
  return(NULL)
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
