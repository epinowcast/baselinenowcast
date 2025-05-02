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

#' Extract from one matrix only elements that are missing in another
#'
#' @param pt_nowcast_mat Matrix containing a mix of predicted and observed
#'    values
#' @param rep_mat Matrix containing only the observed elements of the
#'    reporting triangle
#'
#' @returns `pred_mat` Matrix containing the elements from `pt_nowcast_mat`
#'   for only the elements that are missing in `obs_mat`
#' @export
#'
#' @examples
#' pt_nowcast_mat <- matrix(
#'   c(
#'     1, 3, 5, 7,
#'     4, 7, 8, 9,
#'     9, 10, 3, 5,
#'     3, 4, 8, 5
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#'
#' reporting_matrix <- matrix(
#'   c(
#'     1, 3, 5, 7,
#'     4, 7, 8, NA,
#'     9, 10, NA, NA,
#'     3, NA, NA, NA
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#'
#' other_subset <- extract_predictions(pt_nowcast_mat, reporting_matrix)
#' other_subset
extract_predictions <- function(pt_nowcast_mat,
                                rep_mat) {
  assert_matrix(pt_nowcast_mat, any.missing = FALSE)
  assert_matrix(rep_mat, all.missing = FALSE)
  # Check that the observations are the same
  all_equal <- all(pt_nowcast_mat[!is.na(rep_mat)] == rep_mat[!is.na(rep_mat)]) # nolint
  if (isFALSE(all_equal)) {
    cli_abort(message = c(
      " `obs_mat` is not a subset of `pt_nowcast_mat`. Check to make sure that ",
      "the matrix combining predictions and observations aligns with the ",
      "matrix containing only the observed values in the reporting matrix. "
    ))
  }

  pred_mat <- pt_nowcast_mat
  pred_mat[!is.na(rep_mat)] <- NA
  return(pred_mat)
}
