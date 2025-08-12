#' @title Create a long tidy dataframe from a reporting triangle
#'
#' @param data Reporting triangle to be converted to a long tidy dataframe
#'    with counts by reference and report date. Can either be a matrix or a
#'    dataframe but should be in wide format where rows are reference times and
#'    columns are delays plus any additional metadata.
#' @param reference_dates Vector of dates or character strings indicating the
#'    dates that each of the reference times correspond to, ordered from
#'    row 1 to the last row of the reporting matrix. Must be of the same length
#'    as the number of rows of the reporting matrix.
#' @param delays Vector of integers indicating the delays that each column of
#'    the reporting matrix corresponds to.
#' @param ... Additional arguments
#' @details
#'   The input needs to be a matrix or data.frame with numeric or NA entries
#'
#'
#' @returns Data.frame containing the counts by reference date and report
#'    date
#' @export
#'
#' @examples
#' rep_tri <- matrix(
#'   c(
#'     10, 2, 1,
#'     12, 3, NA,
#'     13, NA, NA
#'   ),
#'   byrow = TRUE,
#'   nrow = 3
#' )
#' reference_dates <- c("2025-01-01", "2025-01-02", "2025-01-03")
#' delays <- c(0, 1, 2)
#' as_data_frame(
#'   rep_tri,
#'   reference_dates,
#'   delays
#' )
as_data_frame <- function(data,
                          reference_dates = NULL,
                          delays = NULL,
                          ...) {
  UseMethod("as_data_frame")
}

#' @rdname as_data_frame
#'
#' @export
#' @importFrom lubridate ymd
#' @method as_data_frame matrix
as_data_frame.matrix <- function(
    data,
    reference_dates,
    delays,
    ...) {
  data_df <- data.frame(data)
  colnames(data_df) <- delays
  data_df$reference_date <- reference_dates

  # Get all column names except reference_date
  delay_cols <- setdiff(names(data_df), "reference_date")

  # Stack the delay columns
  long_data <- stack(data_df[delay_cols])
  names(long_data) <- c("count", "delay")
  long_data$reference_date <- rep(data_df$reference_date,
    times = length(delay_cols)
  )
  return(long_data)
}
