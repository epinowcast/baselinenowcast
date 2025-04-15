#' Replace the lower right triangle of the matrix with NAs
#'
#' @param matrix Matrix
#' @returns A matrix of the same dimensions, with NAs for all the lower right
#'   entries.
#' @export
#' @examples
#' triangle_w_zeros <- matrix(
#'   c(
#'     1, 3, 5, 7,
#'     4, 7, 8, 0,
#'     9, 10, 0, 0,
#'     3, 0, 0, 0
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#'
#' rep_tri <- replace_lower_right_with_NA(triangle_w_zeros)
#' print(rep_tri)
#'
replace_lower_right_with_NA <- function(matrix) {
  # Get matrix dimensions
  rows <- nrow(matrix)
  cols <- ncol(matrix)

  # Create a copy of the input matrix
  result <- matrix

  # Replace the lower right triangle with NAs
  for (i in 1:rows) {
    for (j in 1:cols) {
      if (i + j > (rows + 1)) {
        # For other rows, replace the lower right triangle with NA
        result[i, j] <- NA
      }
    }
  }

  return(result)
}

#' Check if matrix only contains NAs in the bottom right
#'
#' @param mat Matrix
#'
#' @returns Boolean indicating whether the matrix only contains NAs in the
#'    bottom right (TRUE if only in bottom right, FALSE if elsewhere).
#' @keywords internal
.check_na_bottom_right <- function(mat) {
  # Create a logical mask for valid NA positions
  n_rows <- nrow(mat)
  mask <- matrix(FALSE, nrow = n_rows, ncol = ncol(mat))

  for (i in seq_len(n_rows)) {
    cutoff <- n_rows - i + 1
    if (cutoff < ncol(mat)) {
      mask[i, (cutoff + 1):ncol(mat)] <- TRUE
    }
  }

  # Check if any NAs exist outside valid region
  invalid_nas <- sum(is.na(mat) & !mask)
  return(invalid_nas == 0)
}

#' Check if matrix only contains zeros in the bottom right
#'
#' @param mat Matrix
#'
#' @returns Boolean indicating whether the matrix only contains zeros in the
#'    bottom right (if TRUE, entire bottom right is 0s)
#' @keywords internal
.check_zeros_bottom_right <- function(mat) {
  n_rows <- nrow(mat)
  mask <- matrix(FALSE, nrow = n_rows, ncol = ncol(mat))

  for (i in seq_len(n_rows)) {
    cutoff <- n_rows - i + 1
    if (cutoff < ncol(mat)) {
      mask[i, (cutoff + 1):ncol(mat)] <- TRUE
    }
  }

  if (any(mat == 0, na.rm = TRUE)) {
    bool_all_zeros <- sum((mat == 0 & mask)) == sum((mask))
  } else {
    bool_all_zeros <- FALSE
  }
  return(bool_all_zeros)
}

#' Check if matrix is a reporting triangle
#'
#' @param mat Matrix
#'
#' @returns Boolean indicating whether the matrix contains only NAs in the
#'    bottom right and not NAs everywhere else
#' @keywords internal
.is_reporting_triangle <- function(mat) {
  n_rows <- nrow(mat)
  mask <- matrix(FALSE, nrow = n_rows, ncol = ncol(mat))

  for (i in seq_len(n_rows)) {
    cutoff <- n_rows - i + 1
    if (cutoff < ncol(mat)) {
      mask[i, (cutoff + 1):ncol(mat)] <- TRUE
    }
  }

  bool_bottom_right_NA <- all(is.na(mat[mask])) # nolint
  bool_top_right_not_NA <- all(!is.na(mat[!mask])) # nolint
  bool_rep_tri <- bool_bottom_right_NA && bool_top_right_not_NA
  return(bool_rep_tri)
}
