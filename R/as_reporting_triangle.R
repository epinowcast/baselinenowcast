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
#'    entries are incident cases at each reference time and delay.
#'    - An integer indicating the maximum delay used to create the reporting
#'    triangle
#'    - A vector of the same length as the rows of the matrix indicating the
#'    dates corresponding to the reference times in the rows of the reporting
#'    triangle.
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
  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  # Check for:
  # - multiple reference report date combinations
  # - max delay is specified
  # - all reference dates from min to max are available
  # - there are values missing in the bottom right of the reporting triangle

  # Compute delay
  data$delay <- as.integer(as.Date(data$report_date) -
    as.Date(data$reference_date))

  # Filter to delays less than maximum delay
  data <- data[data$delay <= max_delay, ]

  # Get the vector of reference dates inorder from oldest to most recent
  reference_dates <- sort(unique(data$reference_date))

  # Get only the columns you need
  select_data <- data[, c("reference_date", "count", "delay")]

  # Expand so that all delays from 0 to max delay are covered
  all_combos <- expand.grid(
    reference_date = reference_dates,
    delay = 0:max_delay
  ) |>
    merge(select_data,
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
  wide_data <- reshape(
    all_combos,
    idvar = "reference_date",
    timevar = "delay",
    v.names = "count",
    direction = "wide"
  )

  # Get only the reporting triangle
  rep_tri_mat <- as.matrix(wide_data[2:ncol(wide_data)])

  structure <- detect_structure(rep_tri_mat)

  reporting_triangle_obj <- structure(
    list(
      reporting_triangle_matrix = rep_tri_mat,
      reference_date = reference_dates,
      max_delay = max_delay,
      strata = strata,
      structure = structure
    ),
    class = "reporting_triangle"
  )

  return(reporting_triangle_obj)
}

#' Detect the structure of a reporting triangle
#'
#' @inheritParams estimate_delay
#'
#' @returns  Integer or vector specifying the reporting structure.
#'   If integer, divides columns evenly by that integer (with last possibly
#'   truncated).  If vector, the sum must not be greater than or equal to the
#'   number of columns. Default is 1 (standard triangular structure).
#' @export
#'
#' @examples
#' ragged_triangle <- matrix(
#'   c(
#'     1, 3, 5, 7, 9, 7,
#'     4, 5, 9, 4, NA, NA,
#'     1, 6, NA, NA, NA, NA,
#'     3, NA, NA, NA, NA, NA
#'   ),
#'   nrow = 4,
#'   byrow = TRUE
#' )
#' detected_structure <- detect_structure(ragged_triangle)
#' detected_structure
detect_structure <- function(reporting_triangle) {
  n_row_nas <- sum(is.na(rowSums(reporting_triangle)))
  n_prev_nas <- 0
  structure_long <- rep(NA, ncol(reporting_triangle))
  for (i in 1:n_row_nas) {
    n_nas <- sum(!is.na(reporting_triangle[nrow(reporting_triangle) - i + 1, ])) - n_prev_nas
    structure_long[i] <- n_nas
    n_prev_nas <- n_prev_nas + n_nas
  }
  structure <- structure_long[!is.na(structure_long)]

  # Check to see if this can be reduced to just a single number
  if (all(.expand_structure_vec(structure[1],
    cols = ncol(reporting_triangle)
  ) == structure)) {
    structure <- structure[1]
  }

  return(structure)
}
