#' Handle negative values in the reporting triangle
#' @description
#' Takes in a reporting triangle and returns a matrix in the same format
#' as the input triangle, but with negative values of reporting handled via
#' passing them to the subsequent days (from longer delay to shorter).
#' Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R #nolint
#' @param triangle the reporting triangle as a matrix, where rows are the
#' time points and columns are the delays, already truncated to the maximum
#' delay and the number of historical observations
#' @return pos_triangle a positive integer matrix with negative values of
#' reporting handled via passing them to the subsequent days delay
handle_neg_vals <- function(triangle) {
  integer_cols <- seq_len(ncol(triangle))
  pos_triangle <- triangle
  pos_triangle[is.na(pos_triangle)] <- 0 # Set NAs to 0
  for (i in seq_len(nrow(triangle))) {
    to_subtract <- 0
    row <- pos_triangle[i, ]
    # Loop over the columns starting from the last column back to max delay
    # column, and if there is a negative value, we add this to the
    # next day and set that one as 0.
    for (j in rev(integer_cols)) {
      value <- row[[j]]
      if (!is.na(value)) {
        # Either adds 0 or the previous days negative value
        value <- value + to_subtract
        if (value < 0) {
          # Want to subtract from subsequent day
          to_subtract <- value
          pos_triangle[i, j] <- 0 # Set the negative value in the RT to 0
        } else {
          pos_triangle[i, j] <- value
          to_subtract <- 0
        }
      }
    }
  }
  for (col in integer_cols) {
    pos_triangle[[col]] <- as.integer(pos_triangle[[col]])
  }
  return(pos_triangle)
}
