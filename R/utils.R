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
#' @inheritParams get_nowcast_pred_draws
#' @returns Matrix containing the elements from `point_nowcast_matrix` for
#'    only the elements that are missing in `reporting_triangle`
#' @keywords internal
.extract_predictions <- function(point_nowcast_matrix,
                                 reporting_triangle) {
  assert_matrix(point_nowcast_matrix, any.missing = FALSE)
  assert_matrix(reporting_triangle, all.missing = FALSE)
  # Check that the observations are the same
  all_equal <- all(point_nowcast_matrix[!is.na(reporting_triangle)] == reporting_triangle[!is.na(reporting_triangle)]) # nolint
  if (isFALSE(all_equal)) {
    cli_abort(message = c(
      "`reporting_triangle` is not a subset of `point_nowcast_matrix`. Check to
       make sure that the matrix combining predictions and observations aligns
       with the matrix containing only the observed values in the reporting
       triangle. "
    ))
  }

  pred_mat <- point_nowcast_matrix
  pred_mat[!is.na(reporting_triangle)] <- NA
  return(pred_mat)
}
