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
      " `rep_mat` is not a subset of `pt_nowcast_mat`. Check to make sure ",
      "that the matrix combining predictions and observations aligns with the ",
      "matrix containing only the observed values in the reporting matrix. "
    ))
  }

  pred_mat <- pt_nowcast_mat
  pred_mat[!is.na(rep_mat)] <- NA
  return(pred_mat)
}
