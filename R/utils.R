#' Replace the lower right triangle of the matrix with NAs
#'
#' @param matrix Matrix
#' @param structure Integer or vector specifying the reporting structure.
#'   If integer, divides columns evenly by that integer (with last possibly
#'   truncated).  If vector, must sum to number of columns.
#'   Default is 1 (standard triangular structure).
#' @returns A matrix of the same dimensions, with NAs for all unreported
#'  entries.
#' @export
#' @examples
#' @examples
#' # Define a reporting triangle with zeros
#' triangle_w_zeros <- matrix(
#'   c(
#'     1, 3, 5, 7, 9,
#'     4, 7, 8, 0, 12,
#'     9, 10, 0, 0, 15,
#'     3, 0, 0, 0, 0,
#'     6, 2, 0, 0, 0
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#'
#' # Standard triangular structure (default)
#' rep_tri <- replace_lower_right_with_NA(triangle_w_zeros)
#' rep_tri
#'
#' # Ragged structure with 2 columns per delay period
#' rep_ragged <- replace_lower_right_with_NA(triangle_w_zeros, 2)
#' rep_ragged
#'
#' # Custom structure with explicit column counts
#' rep_custom <- replace_lower_right_with_NA(triangle_w_zeros, c(1, 2, 1))
#' rep_custom
replace_lower_right_with_NA <- function(matrix, structure = 1) {
  # Get matrix dimensions
  rows <- nrow(matrix)
  cols <- ncol(matrix)

  # Create a copy of the input matrix
  result <- matrix

  # Process structure parameter
  if (length(structure) == 1) {
    structure_vec <- .expand_structure_vec(structure, cols)
  } else {
    # Vector case
    structure_vec <- structure

    if (sum(structure_vec) != (cols - 1)) {
      cli_abort(c(
        "Sum of structure vector must equal the number of columns in matrix",
        " minus 1"
      ))
    }
  }

  # For each row, determine which columns should be NA
  cutoff_cols <- cumsum(structure_vec) + 1

  for (i in seq_along(structure_vec)) {
    index_row <- rows - i + 1
    start_col <- cutoff_cols[i]
    result[index_row, start_col:cols] <- NA_real_
  }

  return(result)
}

.expand_structure_vec <- function(structure, cols) {
  if (structure <= 0) {
    cli_abort("Structure must be positive")
  }

  if (structure > cols) {
    cli_abort("Structure cannot be larger than number of columns")
  }
  adjusted_cols <- cols - 1

  n_complete_groups <- floor(adjusted_cols / structure)

  structure_vec <- rep(structure, n_complete_groups)

  remainder <- adjusted_cols %% structure

  if (remainder > 0) {
    structure_vec <- c(structure_vec, remainder)
  }

  return(structure_vec)
}

#' Check if matrix has valid NA pattern
#'
#' @param mat Matrix
#'
#' @returns Boolean indicating whether the matrix only contains NAs in the
#'    bottom right (TRUE if only in bottom right, FALSE if elsewhere).
#' @keywords internal
.check_na_bottom_right <- function(mat) {
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)

  for (i in 1:n_rows) {
    row_data <- mat[i, ]
    na_indices <- which(is.na(row_data))

    # If there are NAs in this row
    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      # Check that all entries from the first NA onwards are also NA
      if (!all(is.na(row_data[min_na_idx:n_cols]))) {
        return(FALSE)
      }
    }
  }

  # Check column consistency (if a cell is NA, all cells below it must be NA)
  for (j in 1:n_cols) {
    col_data <- mat[, j]
    na_indices <- which(is.na(col_data))

    if (length(na_indices) > 0) {
      min_na_idx <- min(na_indices)
      if (!all(is.na(col_data[min_na_idx:n_rows]))) {
        return(FALSE)
      }
      # Check that all entries above the first NA are not NA
      if (min_na_idx > 1 && anyNA(col_data[1:(min_na_idx - 1)])) {
        return(FALSE)
      }
    }
  }

  return(TRUE)
}
