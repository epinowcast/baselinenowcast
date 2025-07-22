#' @title Create a reporting triangle from a dataframe
#' 
#' @details
#'  The input needs to be a data.frame or similar with the following columns:
#'  - `reference_date`: Column of type `date` or character with the dates of
#'     the reference event.
#'  - `report_date`: Column of type `date` or character with the dates of 
#'     report of the reference event.
#'  - `count`: Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date. 
#'
#'
#' @returns Matrix containing reference times as rows and delays as columns 
#'    with entries for the number of new counts at each reference time and 
#'    delay. 
#' @export
#'
#' @examples
#' long_data <- data.frame(
#' reference_date = c("2025-01-01", "2025-01-01", "2025-01-01",
#'   "2025-01-02", "2025-01-02", "2025-01-03"),
#' report_date = c("2025-01-01", "2025-01-02", "2025-01-03", 
#'    "2025-01-02", "2025-01-03", "2025-01-03"),
#' count = c(10, 2, 1, 12, 3, 13)
#'   )
#' as_reporting_triangle(long_data)
as_reporting_triangle <- function(data, ...) {
  UseMethod("as_reporting_triangle")
}


#' @rdname as_reporting_triangle
#' @param reference_date Name of the column in `data` that contains the
#'    reference date.
#' @param report_date Name of the column in `data` that contains the report
#'    date.
#' @param max_delay Integer indicating the maximum delay of the process which
#'    will dictate the number of columns in the resulting reporting triangle.
#' @param ... Additional arguments
#'
#' @export
#' @method as_reporting_triangle data.frame
as_reporting_triangle.data.frame <- function(
    data,
    reference_date = NULL,
    report_date = NULL,
    max_delay = NULL, 
    ...
){
  data$delay <- as.integer(data$report_date - data$reference_date)
  if (!is.null(max_delay)) {
    data <- data[data$delay <= max_delay, ]
  }
  wide_data <- reshape(
    data, 
    direction = "wide",
    idvar = "reference_date",
    timevar = "delay",
    v.names = "count"
  )
  cols_to_remove <- c("reference_date", "report_date")
  matrix_cols <- !names(wide_data) %in% cols_to_remove
  reporting_triangle <- as.matrix(wide_data[, ..matrix_cols])
  
  colnames(reporting_triangle) <- gsub("count\\.", "", colnames(reporting_triangle))
 
  return(reporting_triangle)
  
}