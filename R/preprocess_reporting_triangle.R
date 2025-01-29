#' Pre-process the reporting triangle
#' @description
#' Takes in a reporting triangle with the reference date as the first column,
#' and all subsequent columns named by their delay (unitless, so for example a
#' delay of 3 days or weeks would have the column name `3`), and values
#' indicating the number of new confirmed cases assigned to that reference
#' date with that particular delay. It returns a dataframe in the same format
#' as the input triangle, but truncated to include only the maximum delay
#' number of columns and with negative values of reporting handled via passing
#' them to the subsequent days delay.
#' Modified from https://github.com/KITmetricslab/RESPINOW-Hub/blob/main/code/baseline/functions.R #nolint
#' @param triangle the reporting triangle as a data.frame, with rows as the
#' reference date and columns as the delay, in any units. Assumes that the
#' delays will be in the format `{delay}` with no suffix for the unit
#' @param max_delay the maximum delay, in the delay units, to filter the
#' reporting triangle
#' @return trunc_triangle a dataframe in the same format as `triangle`, but
#' truncated to include only the maximum delay number of delay columns
#' and with negative values of reporting handled via passing them to the
#' subsequent days delay
preprocess_reporting_triangle <- function(triangle, max_delay) {
  # restrict to the first columns in the max
  cols_subset <- which(colnames(triangle) == max_delay)
  trunc_triangle <- triangle[, 1:cols_subset]
  # columns containing integer names:
  integer_cols <- which(colnames(trunc_triangle) %in% (grep("^\\d+$", names(trunc_triangle), value = TRUE))) # nolint
  # Loop over each row
  for (i in seq_len(nrow(trunc_triangle))) {
    to_subtract <- 0
    row <- trunc_triangle[i, ]
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
          trunc_triangle[i, j] <- 0 # Set the negative value in the RT to 0
        } else {
          trunc_triangle[i, j] <- value
          to_subtract <- 0
        }
      }
    }
  }
  # Convert 'value' columns to integer type
  for (col in integer_cols) {
    trunc_triangle[[col]] <- as.integer(trunc_triangle[[col]])
  }
  return(data.table::as.data.table(trunc_triangle))
}
