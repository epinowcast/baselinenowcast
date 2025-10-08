#' @title Create a reporting triangle
#'
#' @param data Either a matrix of a reporting triangle or a
#'    data.frame to be converted to a reporting triangle matrix.
#'    Can either be in the long tidy format of counts by reference date
#'    and report date or line list data with individual observations
#'    indexed by their reference date and report date.
#' @param max_delay Integer indicating the maximum delay to estimate.
#' @param ... Additional arguments passed to methods. For data.frame method:
#'    `strata`, `reference_date_col_name`, `report_date_col_name`,
#'    `count_col_name`, `delays_unit`. For matrix method: `reference_dates`,
#'    `strata`, `delays_unit`.
#
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
#'    - A character string indicating the unit of the delays.
#' @export
#'
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#' @importFrom lubridate time_length days ymd
as_reporting_triangle <- function(data, max_delay, ...) {
  UseMethod("as_reporting_triangle")
}

#' @title Create a reporting triangle object from a data.frame
#' @param strata Character string indicating the column names for
#'   stratification e.g. location, age group, weekday. Must correspond
#'   to column names in `data`. Default is `NULL`.
#' @param reference_date_col_name Character string indicating the name of the
#'    column which represents the reference date, or the date of the primary
#'    event occurrence.
#' @param report_date_col_name Character string indicating the name of the
#'    column which represents the date the primary event was reported.
#' @param count_col_name Character string indicating the name of the column
#'    containing the number of incident cases on each reference and report date.
#' @param delays_unit Character string specifying the time units to use.
#'    Default is "days".
#' @details
#'  The input needs to be a data.frame or similar with the following columns:
#'    - Column of type `date` or character with the dates of
#'     the primary event occurrence (reference date).
#'    - Column of type `date` or character with the dates of
#'     report of the primary event.
#'    - Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date.
#'  Additional columns can be included but will not be used. The input
#'  dataframe for this function must contain only a single strata, there can
#'  be no repeated reference dates and report dates.
#'
#'
#' @rdname as_reporting_triangle
#'
#' @export
#' @method as_reporting_triangle data.frame
#' @importFrom checkmate check_integerish assert_character assert_choice
#' @importFrom stats reshape
as_reporting_triangle.data.frame <- function(
    data,
    max_delay,
    strata = NULL,
    reference_date_col_name = "reference_date",
    report_date_col_name = "report_date",
    count_col_name = "count",
    delays_unit = "days",
    ...) {
  assert_character(strata, null.ok = TRUE)
  assert_character(reference_date_col_name)
  assert_character(report_date_col_name)
  assert_character(count_col_name)
  assert_character(delays_unit)
  assert_choice(delays_unit, choices = c("days", "weeks", "months", "years"))

  # Create a named vector for renaming
  old_names <- c(reference_date_col_name, report_date_col_name, count_col_name)
  new_names <- c("reference_date", "report_date", "count")
  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]




  # Check for:
  # - multiple reference report date combinations
  # - max delay is specified
  # - all reference dates from min to max are available

  .validate_rep_tri_df(data, delays_unit)

  # Create the named list of strata
  if (!is.null(strata)) {
    if (!all(strata %in% colnames(data))) {
      cli_abort(
        message = c("`strata` specified are not columns in `data`.")
      )
    }
    strata_list <- lapply(data[c(strata)], unique)

    if (!all(sapply(strata_list, length) == 1)) {
      cli_abort(
        message = c("Multiple values found for the specified `strata` when trying to create a single `reporting_triangle` object.") # nolint
      )
    }
  } else {
    strata_list <- NULL
  }


  # Compute delay
  data$delay <- time_length(as.Date(data$report_date) -
    as.Date(data$reference_date), unit = delays_unit)
  if (!isTRUE(check_integerish(data$delay))) {
    cli_abort(
      message = c(
        "Delays must be integers, detected non-integer time difference between `reference_date` and `report_date` .", # nolint
        "i" = "Check that `delays_unit` is specified correctly." # nolint
      )
    )
  }

  if (max_delay > max(data$delay)) {
    cli_abort(
      message = c(
        "`max_delay` specified is larger than the maximum delay in the data."
      )
    )
  }

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

  # Fill in the 0s based on the whether the report date is after the
  # latest report date (this assumes data has been filtered before!)

  # Get appropriate lubridate function based on delay_units
  delay_fn <- get(delays_unit, envir = asNamespace("lubridate"))
  ix <- is.na(all_combos$count)
  all_combos$count[ix] <- ifelse(
    all_combos$reference_date[ix] + delay_fn(all_combos$delay[ix]) >
      max(data$report_date),
    NA, 0
  )
  wide_data <- reshape(
    all_combos,
    idvar = "reference_date",
    timevar = "delay",
    v.names = "count",
    direction = "wide"
  )

  # Get only the reporting triangle (remove reference date column)
  rep_tri_mat <- as.matrix(wide_data[, -1])
  dimnames(rep_tri_mat) <- NULL

  rep_tri <- as_reporting_triangle.matrix(
    data = rep_tri_mat,
    reference_dates = reference_dates,
    max_delay = max_delay,
    strata_list = strata_list,
    delays_unit = delays_unit
  )
  return(rep_tri)
}

#' @title Create a reporting triangle object from a matrix
#' @param reference_dates Vector of character strings indicating the reference
#'   dates corresponding to each row of the reporting triangle matrix (`data`).
#' @rdname as_reporting_triangle
#' @export
#' @method as_reporting_triangle matrix
as_reporting_triangle.matrix <- function(data,
                                         max_delay,
                                         reference_dates,
                                         strata_list = NULL,
                                         delays_unit = "days",
                                         ...) {
  .validate_triangle(
    triangle = data,
    max_delay = max_delay,
    n = nrow(data)
  )
  if (length(reference_dates) != nrow(data)) {
    cli_abort(
      message = c(
        "Length of `reference_dates` must equal number of rows in `reporting_triangle`" # noline
      )
    )
  }

  structure <- detect_structure(data)
  reporting_triangle_obj <- structure(
    list(
      reporting_triangle_matrix = data,
      reference_date = reference_dates,
      max_delay = max_delay,
      strata_list = strata_list,
      structure = structure,
      delays_unit = delays_unit
    ),
    class = "reporting_triangle"
  )

  return(reporting_triangle_obj)
}
