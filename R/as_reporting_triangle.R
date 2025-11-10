#' Create a `reporting_triangle` object
#'
#' @param data Data to be nowcasted.
#' @param max_delay Integer indicating the maximum delay.
#' @param strata Character string indicating the strata. Default is NULL.
#' @param delays_unit Character string specifying the temporal granularity of
#'    the delays. Options are `"days"`, `"weeks"`, `"months"`, `"years"`.
#'    For the matrix method, this is simply passed as an item in the
#'    `reporting_triangle` object which will later be used to create a nowcast
#'    data.frame. For the data.frame method, this is used to compute the delay
#'    in terms of the specified unit, and to expand the combinations of
#'    reference dates and delays to the complete set of combinations in
#'    the reporting triangle.  Default is `"days"`.
#' @param ... Additional arguments passed to methods.
#
#' @return  A [reporting_triangle] object
#'
#' @family reporting_triangle
#' @export
as_reporting_triangle <- function(data,
                                  max_delay,
                                  strata = NULL,
                                  delays_unit = "days",
                                  ...) {
  UseMethod("as_reporting_triangle")
}

#' Create a `reporting_triangle` object from a data.frame
#'
#' This method takes a data.frame containing case counts indexed by reference
#' date and report date and creates a [reporting_triangle] object.
#' See [as_reporting_triangle.matrix()] for other data
#' input options.
#'
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
#' @param strata Vector or single character string indicating the name of the
#'    column(s) of `data` which indicate the strata associated with the data.
#'    Entries of that column must all be the same. Default is `NULL`, which
#'    does not assign metadata to this data.
#' @inheritParams as_reporting_triangle
#' @param reference_date Character string indicating the name of the
#'    column which represents the reference date, or the date of the primary
#'    event occurrence.
#' @param report_date Character string indicating the name of the
#'    column which represents the date the primary event was reported.
#' @param count Character string indicating the name of the column
#'    containing the number of incident cases on each reference and report date.
#' @param ... Additional arguments not used.
#'
#'
#'
#' @export
#' @return A \code{\link{reporting_triangle}} object
#' @method as_reporting_triangle data.frame
#' @family reporting_triangle
#' @importFrom checkmate check_integerish assert_date
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
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    ...) {
  assert_character(strata, null.ok = TRUE)
  assert_character(reference_date)
  assert_character(report_date)
  assert_character(count)
  assert_character(delays_unit)
  assert_choice(delays_unit, choices = c("days", "weeks", "months", "years"))
  data <- .rename_cols(data, old_names = c(reference_date, report_date, count))

  .validate_rep_tri_df(data, delays_unit)
  assert_date(data$reference_date)
  assert_date(data$report_date)

  # Compute delay
  data$delay <- as.numeric(
    difftime(
      as.Date(data$report_date),
      as.Date(data$reference_date),
      units = delays_unit
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
      message =
        "`max_delay` specified is larger than the maximum delay in the data."
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
      units = delays_unit
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


#' Create a `reporting_triangle` from a matrix
#'
#' This method takes a matrix in the format of a reporting triangle, with rows
#' as reference dates and columns as delays and elements as incident case
#' counts and creates a [reporting_triangle] object. See other
#' [as_reporting_triangle.data.frame()] for other data
#' input options.
#'
#' @param data Matrix of a reporting triangle where rows are reference times,
#'    columns are delays, and entries are the incident counts.
#' @inheritParams as_reporting_triangle
#' @param reference_dates Vector of Date objects or character strings indicating
#'   the reference dates corresponding to each row of the reporting triangle
#'   matrix (`data`). If NULL (default), dummy dates starting from 1900-01-01
#'   are generated with spacing determined by `delays_unit`.
#' @param ... Additional arguments not used.
#' @export
#' @return A \code{\link{reporting_triangle}} object
#' @method as_reporting_triangle matrix
#' @family reporting_triangle
#' @examples
#' rep_tri_mat <- matrix(
#'   c(
#'     1, 3, 5, 7, 9,
#'     4, 7, 8, 0, NA,
#'     9, 10, 0, NA, NA,
#'     3, 0, NA, NA, NA,
#'     6, NA, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#'
#' reference_dates <- seq(
#'   from = as.Date("2025-01-01"),
#'   to = as.Date("2025-01-05"),
#'   by = "day"
#' )
#' max_delay <- 4
#' rep_tri <- as_reporting_triangle(
#'   data = rep_tri_mat,
#'   reference_dates = reference_dates,
#'   max_delay = max_delay
#' )
#' rep_tri
as_reporting_triangle.matrix <- function(data,
                                         max_delay,
                                         strata = NULL,
                                         delays_unit = "days",
                                         reference_dates = NULL,
                                         ...) {
  .validate_triangle(
    triangle = data,
    max_delay = max_delay,
    n = nrow(data)
  )

  # Use dummy dates starting from 1900 if no reference_dates provided
  if (is.null(reference_dates)) {
    cli_alert_info(
      "No reference dates provided. Using dummy dates starting from 1900-01-01."
    )
    reference_dates <- seq(
      from = as.Date("1900-01-01"),
      by = delays_unit,
      length.out = nrow(data)
    )
  }

  if (length(reference_dates) != nrow(data)) {
    cli_abort(
      message =
        "Length of `reference_dates` must equal number of rows in `reporting_triangle`" # nolint
    )
  }

  struct <- detect_structure(data)

  reporting_triangle_obj <- new_reporting_triangle(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    max_delay = max_delay,
    strata = strata,
    structure = struct,
    delays_unit = delays_unit
  )
  return(reporting_triangle_obj)
}

#' Rename required columns
#'
#' @param data Data.frame with the original column names
#' @param old_names The names of the columns for, in order, reference date,
#'    report date, and count
#' @keywords internal
#' @returns Data.frame with columns renamed
.rename_cols <- function(data,
                         old_names) {
  new_names <- c("reference_date", "report_date", "count")
  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]
  return(data)
}
