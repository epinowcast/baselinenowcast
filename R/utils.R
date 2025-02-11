#' Replace the lower right triangle of the matrix with NAs
#'
#' @param matrix Matrix
#' @returns A matrix of the same dimensions, with NAs for all the lower right
#'   entries
#' @keywords internal
.replace_lower_right_with_NA <- function(matrix) {
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
