#' Create a `reporting_triangle_df` object
#'
#' @param data Data to be converted to reporting_triangle_df format.
#' @param delays_unit Character string specifying the temporal granularity of
#'    the delays. Options are `"days"`, `"weeks"`, `"months"`, `"years"`.
#'    Default is `"days"`.
#' @param ... Additional arguments passed to methods.
#'
#' @return  A [reporting_triangle_df] object
#'
#' @family reporting_triangle_df
#' @export
as_reporting_triangle_df <- function(data,
                                     delays_unit = "days",
                                     ...) {
  UseMethod("as_reporting_triangle_df")
}

#' Create a `reporting_triangle_df` object from a data.frame
#'
#' This method takes a data.frame containing case counts indexed by reference
#' date and report date and creates a [reporting_triangle_df] object.
#'
#' @param data Data.frame in a long tidy format with counts by reference date
#'    and report date. Must contain the following columns:
#'    - Column of type `Date` or character with the dates of
#'     the primary event occurrence (reference date).
#'    - Column of type `Date` or character with the dates of
#'     report of the primary event (report_date).
#'    - Column of numeric or integer indicating the new confirmed counts
#'     pertaining to that reference and report date (count).
#'  Additional columns can be included and will be preserved if specified in
#'  the `by` parameter as strata columns.
#' @inheritParams as_reporting_triangle_df
#' @param reference_date Character string indicating the name of the
#'    column which represents the reference date, or the date of the primary
#'    event occurrence. Default is "reference_date".
#' @param report_date Character string indicating the name of the
#'    column which represents the date the primary event was reported.
#'    Default is "report_date".
#' @param count Character string indicating the name of the column
#'    containing the number of incident cases on each reference and report date.
#'    Default is "count".
#' @param by Character vector of strata column names. These columns define
#'   unique strata for nowcasting. NULL (default) indicates no strata.
#'   Stored internally as the "strata" attribute.
#' @param ... Additional arguments not used.
#'
#' @export
#' @return A [reporting_triangle_df] object
#' @method as_reporting_triangle_df data.frame
#' @family reporting_triangle_df
#' @importFrom checkmate assert_character assert_data_frame
#' @examples
#' # Single stratum (no strata)
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rt_df <- as_reporting_triangle_df(data = data_as_of_df)
#'
#' # Multiple strata
#' rt_df_strata <- as_reporting_triangle_df(
#'   germany_covid19_hosp,
#'   by = c("age_group", "location")
#' )
as_reporting_triangle_df.data.frame <- function(
    data,
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    by = NULL,
    ...) {
  assert_data_frame(data)
  assert_character(reference_date, len = 1)
  assert_character(report_date, len = 1)
  assert_character(count, len = 1)
  assert_delays_unit(delays_unit)

  if (!is.null(by)) {
    assert_character(by, min.len = 1)
  }

  # Rename columns to standard names
  data <- .rename_cols(data, old_names = c(reference_date, report_date, count))

  # Select only required columns
  if (!is.null(by)) {
    cols_to_keep <- c("reference_date", "report_date", "count", by)
  } else {
    cols_to_keep <- c("reference_date", "report_date", "count")
  }

  data <- data[, cols_to_keep, drop = FALSE]

  # Convert dates if needed
  assert_date(data$reference_date)
  assert_date(data$report_date)

  # Create the reporting_triangle_df object
  result <- new_reporting_triangle_df(
    data = data,
    delays_unit = delays_unit,
    strata = by
  )

  # Validate the constructed object
  assert_reporting_triangle_df(result)

  return(result)
}

#' Create a `reporting_triangle_df` from a reporting_triangle
#'
#' This method converts a [reporting_triangle] object to long format
#' [reporting_triangle_df].
#'
#' @param data A [reporting_triangle] object.
#' @inheritParams as_reporting_triangle_df
#' @param ... Additional arguments not used.
#'
#' @export
#' @return A [reporting_triangle_df] object
#' @method as_reporting_triangle_df reporting_triangle
#' @family reporting_triangle_df
#' @examples
#' # Convert reporting_triangle to reporting_triangle_df
#' rt <- as_reporting_triangle(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' rt_df <- as_reporting_triangle_df(rt)
as_reporting_triangle_df.reporting_triangle <- function(
    data,
    delays_unit = NULL,
    ...) {
  # Get delays_unit from the reporting_triangle if not provided
  if (is.null(delays_unit)) {
    delays_unit <- get_delays_unit(data)
  }

  # Convert to data.frame (which has method for reporting_triangle)
  df <- as.data.frame(data)

  # Remove the delay column as it's computed on-the-fly in reporting_triangle_df
  df$delay <- NULL

  # Create the reporting_triangle_df object (no strata from single triangle)
  result <- new_reporting_triangle_df(
    data = df,
    delays_unit = delays_unit,
    strata = NULL
  )

  # Validate the constructed object
  assert_reporting_triangle_df(result)

  return(result)
}
