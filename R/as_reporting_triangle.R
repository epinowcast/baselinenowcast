#' @title Create a `reporting_triangle` object
#'
#' @param data Data to be nowcasted. Either a matrix of a reporting triangle or a
#'    data.frame to be converted to a reporting triangle matrix.
#'    Can either be in the long tidy format of counts by reference date
#'    and report date or line list data with individual observations
#'    indexed by their reference date and report date.
#' @param max_delay Integer indicating the maximum delay to estimate.
#' @param ... Additional arguments passed to methods.
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
#'
#'   See the corresponding `as_reporting_triangle.<data.type>` functions for
#'   more details on the required input formats.
#'
#' @export
#'
as_reporting_triangle <- function(data, max_delay, ...) {
  UseMethod("as_reporting_triangle")
}

#' @param data Data.frame in a long tidy format with counts by reference date
#'    and report date. Must contain the following columns:
#' .    - Column of type `date` or character with the dates of
#'     the primary event occurrence (reference date).
#'    - Column of type `date` or character with the dates of
#'     report of the primary event (report_date).
#'    - Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date (count).
#'  Additional columns can be included but will not be used. The input
#'  dataframe for this function must contain only a single strata, there can
#'  be no repeated reference dates and report dates.
#' @param strata Character string indicating the metadata on the strata of this
#'    reporting triangle. Default is `NULL`.
#' @param reference_date Character string indicating the name of the
#'    column which represents the reference date, or the date of the primary
#'    event occurrence.
#' @param report_date Character string indicating the name of the
#'    column which represents the date the primary event was reported.
#' @param count Character string indicating the name of the column
#'    containing the number of incident cases on each reference and report date.
#' @param delays_unit Character string specifying the time units to use.
#'    Default is "days".
#'
#'
#' @returns A `reporting_triangle` object.
#'
#' @export
#' @rdname as_reporting_triangle
#' @method as_reporting_triangle data.frame
#' @importFrom checkmate check_integerish
#' @importFrom stats reshape
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
as_reporting_triangle.data.frame <- function(
    data,
    max_delay,
    strata = NULL,
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    delays_unit = "days",
    ...) {
  # Create a named vector for renaming
  old_names <- c(reference_date, report_date, count)
  new_names <- c(
    deparse(substitute(reference_date)),
    deparse(substitute(report_date)),
    deparse(substitute(count))
  )
  setNames(new_names, old_names)
  # names(data)[names(data) %in% old_names] <- new_names[match(
  #   names(data)[names(data) %in% old_names], old_names
  # )]

  .validate_rep_tri_df(data, delays_unit)

  # Compute delay
  data$delay <- as.numeric(
    difftime(
      as.Date(data$report_date),
      as.Date(data$reference_date),
      unit = delays_unit
    )
  )
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

  data <- data[data$delay <= max_delay, ]
  reference_dates <- sort(unique(data$reference_date))
  select_data <- data[, c("reference_date", "count", "delay")]
  all_combos <- expand.grid(
    reference_date = reference_dates,
    delay = 0:max_delay
  ) |>
    merge(select_data,
      by = c("reference_date", "delay"),
      all.x = TRUE
    )

  ix <- is.na(all_combos$count)
  all_combos$count[ix] <- ifelse(
    as.numeric(difftime(
      as.Date(max(data$report_date)),
      as.Date(all_combos$reference_date[ix]),
      unit = delays_unit
    )) >= all_combos$delay[ix],
    0, all_combos$count[ix]
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
    strata = strata,
    delays_unit = delays_unit
  )
  return(rep_tri)
}


#' @param data Matrix of a reporting triangle where rows are reference times,
#'    columns are delays, and entries are the incident counts.
#' @param reference_dates Vector of character strings indicating the reference
#'   dates corresponding to each row of the reporting triangle matrix (`data`).
#' @rdname as_reporting_triangle
#' @export
#' @method as_reporting_triangle matrix
as_reporting_triangle.matrix <- function(data,
                                         max_delay,
                                         reference_dates,
                                         strata = NULL,
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
      strata = strata,
      structure = structure,
      delays_unit = delays_unit
    ),
    class = "reporting_triangle"
  )

  return(reporting_triangle_obj)
}
