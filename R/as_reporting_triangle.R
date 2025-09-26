#' @title Create a reporting triangle from a dataframe
#'
#' @param data Data.frame to be converted to a reporting triangle matrix.
#'    Can either be in the long tidy format of counts by reference date
#'    and report date or line list data with individual observations
#'    indexed by their reference date and report date.
#' @param strata Character string indicating the metadata on the strata of this
#'    reporting triangle. Default is `NULL`.
#' @param max_delay Integer indicating the maximum delay of the process which
#'    will dictate the number of columns in the resulting reporting triangle.
#' @details
#'  The input needs to be a data.frame or similar with the following columns:
#'    - `reference_date`: Column of type `date` or character with the dates of
#'     the reference event.
#'    - `report_date`: Column of type `date` or character with the dates of
#'     report of the reference event.
#'    - `count`: Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date.
#'  Additional columns can be included but will not be used. The input
#'  dataframe for this function must contain only a single strata, there can
#'  be no repeated reference dates and report dates.
#'
#'
#' @returns `reporting_triangle` class object which is a list containing:
#'    - A matrix with which rows are reference times and columns are delays and
#'    entries are incident cases at each reference time and delay
#'    - A vector of the same length as the rows of the matrix indicating the
#'    dates corresponding to the reference times a
#'    - A character string indicating the strata.
#'    - A vector indicating the "structure" of the reporting triangle.
#' @export
#'
#' @examples
#' data_as_of_df <- syn_nssp_df |>
#'   filter(report_date <= "2026-04-01")
#' as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
as_reporting_triangle <- function(data, ...) {
  UseMethod("as_reporting_triangle")
}


#' @rdname as_reporting_triangle
#'
#' @export
#' @method as_reporting_triangle data.frame
as_reporting_triangle.data.frame <- function(
    data,
    max_delay,
    strata = NULL,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count") {
  # Create a named vector for renaming
  old_names <- c(reference_date, report_date, count)
  new_names <- c("reference_date", "report_date", "count")
  names(data)[names(data) %in% old_names] <- new_names[match(names(data)[names(data) %in% old_names], old_names)]

  # Compute delay
  data$delay <- as.integer(as.Date(data$report_date) - as.Date(data$reference_date))

  # Filter to delays less than maximum delay
  data <- data[data$delay <= max_delay, ]

  # Expand so that all delays from 0 to max delay are covered
  all_combos <- expand.grid(
    reference_date = unique(data$reference_date),
    delay = 0:max_delay
  ) |>
    merge(data,
      by = c("reference_date", "delay"),
      all.x = TRUE
    )

  # Fill in the 0s based on whether the report date has been observed
  ix <- is.na(all_combos$count)
  all_combos$count[ix] <- ifelse(
    all_combos$reference_date[ix] + days(all_combos$delay[ix]) >
      max(all_combos$reference_date),
    NA, 0
  )

  wide_data <- all_combos |>
    pivot_wider(
      id_cols = reference_date,
      names_from = delay,
      values_from = count
    )

  # Remove report date
  data$report_date <- NULL
  wide_data <- reshape(
    data,
    direction = "wide",
    idvar = "reference_date",
    timevar = "delay",
    v.names = "count"
  )
  cols_to_remove <- c("reference_date", "report_date")
  matrix_cols <- !names(wide_data) %in% cols_to_remove
  reporting_triangle <- as.matrix(wide_data[, matrix_cols])

  colnames(reporting_triangle) <- gsub("count.", "",
    colnames(reporting_triangle),
    fixed = TRUE
  )

  return(reporting_triangle)
}
