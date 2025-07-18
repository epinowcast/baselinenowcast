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

#' Safe iterator
#'
#' @param fun Function to wrap around
#'
#' @returns Function that will return a NULL if an error occurs
.safelydoesit <- function(fun) {
  stopifnot(is.function(fun))
  return(
    function(...) {
      return(tryCatch(
        list(result = fun(...), error = NULL),
        error = function(e) {
          return(list(result = NULL, error = e))
        }
      ))
    }
  )
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
    cli_abort(
      message =
        "`reporting_triangle` is not a subset of `point_nowcast_matrix`. Check
        to make sure that the matrix combining predictions and observations
        aligns with the matrix containing only the observed values in the
        reporting triangle. "
    )
  }

  pred_mat <- point_nowcast_matrix
  pred_mat[!is.na(reporting_triangle)] <- NA
  return(pred_mat)
}

#' Check if there are non-zero-values on the LHS of NAs
#'
#' @param mat Matrix to check
#'
#' @returns Boolean indicating whether or not there are non-zero values on the
#'    LHS of the first NA (TRUE = has non-zeros, FALSE = only zeros)
.check_lhs_not_only_zeros <- function(mat) {
  # Find first NA
  first_na <- which(is.na(mat[nrow(mat), ]))[1]
  if (is.na(first_na)) {
    has_non_zeros <- TRUE
  } else if (first_na == 1) {
    has_non_zeros <- TRUE # No columns to check
  } else {
    mat_LHS <- mat[, 1:(first_na) - 1]
    has_non_zeros <- !all(mat_LHS == 0)
  }
  return(has_non_zeros)
}
