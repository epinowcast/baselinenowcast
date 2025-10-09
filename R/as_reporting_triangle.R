#' Create a `reporting_triangle` object
#'
#' @param data Data to be nowcasted.
#' @param max_delay Integer indicating the maximum delay.
#' @param strata Character string indicating the metadata on the strata of this
#'    reporting triangle. Default is `NULL`.
#' @param delays_unit Character string specifying the time units to use.
#'    Default is `"days"`.
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
#' @seealso
#' \code{\link{as_reporting_triangle.data.frame}}
#' \code{\link{as_reporting_triangle.matrix}}
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
#' date and report date and creates a `reporting_triangle` object. See other
#' methods for other data input options.
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
#' @inheritParams as_reporting_triangle
#' @param reference_date Character string indicating the name of the
#'    column which represents the reference date, or the date of the primary
#'    event occurrence.
#' @param report_date Character string indicating the name of the
#'    column which represents the date the primary event was reported.
#' @param count Character string indicating the name of the column
#'    containing the number of incident cases on each reference and report date.
#' @param ... Additional arguments passed to methods.
#'
#'
#' @returns A `reporting_triangle` object.
#'
#' @export
#' @method as_reporting_triangle data.frame
#' @seealso
#' \code{\link{as_reporting_triangle}}
#' \code{\link{as_reporting_triangle.matrix}}
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
    delays_unit = "days",
    reference_date = "reference_date",
    report_date = "report_date",
    count = "count",
    ...) {
  # Create a named vector for renaming
  old_names <- c(reference_date, report_date, count)
  new_names <- c("reference_date", "report_date", "count")

  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  .validate_rep_tri_df(data, delays_unit)

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
#' counts and creates a `reporting_triangle` object. See other
#' methods for other data input options.
#'
#' @param data Matrix of a reporting triangle where rows are reference times,
#'    columns are delays, and entries are the incident counts.
#' @inheritParams as_reporting_triangle
#' @param reference_dates Vector of character strings indicating the reference
#'   dates corresponding to each row of the reporting triangle matrix (`data`).
#' @param ... Additional arguments passed to methods.
#' @export
#' @method as_reporting_triangle matrix
#' @seealso
#' \code{\link{as_reporting_triangle.data.frame}}
#' \code{\link{as_reporting_triangle}}
as_reporting_triangle.matrix <- function(data,
                                         max_delay,
                                         strata = NULL,
                                         delays_unit = "days",
                                         reference_dates,
                                         ...) {
  .validate_triangle(
    triangle = data,
    max_delay = max_delay,
    n = nrow(data)
  )
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

#' Class constructor for `reporting_triangle` objects
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle
#' @inheritParams as_reporting_triangle.matrix
#' @inheritParams construct_triangle
#' @inheritParams as_reporting_triangle
#'
#' @returns An object of class `reporting_triangle`
#'
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   structure,
                                   max_delay,
                                   delays_unit,
                                   strata = NULL) {
  assert_matrix(reporting_triangle_matrix)
  assert_date(reference_dates,
    unique = TRUE,
    null.ok = FALSE,
    min.len = 1,
    len = nrow(reporting_triangle_matrix)
  )
  assert_numeric(structure, lower = 1)
  assert_integerish(max_delay, min.len = 1)
  assert_character(delays_unit, len = 1)
  assert_character(strata, null.ok = TRUE, len = 1)
  assert_character(delays_unit, len = 1)
  assert_choice(delays_unit,
    choices = c("days", "weeks", "months", "years")
  )

  result <- structure(
    list(
      reporting_triangle_matrix = reporting_triangle_matrix,
      reference_dates = reference_dates,
      structure = structure,
      max_delay = max_delay,
      delays_unit = delays_unit,
      strata = strata
    ),
    class = "reporting_triangle"
  )
  return(result)
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data An object to check for validity.
#'
#' @param ... Additional arguments
#'
#' @return NULL
#'
#' @export
assert_reporting_triangle <- function(data, ...) {
  UseMethod("assert_reporting_triangle")
}

#' S3 method for reporting triangle assertion
#' @method assert_reporting_triangle reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice
assert_reporting_triangle.reporting_triangle <- function(data, ...) {
  assert_matrix(data$reporting_triangle_matrix)
  assert_date(data$reference_date,
    unique = TRUE,
    null.ok = FALSE,
    min.len = 1,
    len = nrow(data$reporting_triangle_matrix)
  )
  assert_integerish(data$structure,
    min.len = 1
  )
  if (sum(data$structure) > ncol(data$reporting_triangle_matrix)) {
    cli_abort(message = c(
      message = c(
        "Sum of `structure` must not be greater than or equal",
        "to the number of columns in matrix"
      )
    ))
  }
  assert_integerish(data$max_delay, lower = 1)
  assert_character(data$delays_unit, len = 1)
  assert_character(data$strata, null.ok = TRUE, len = 1)
  assert_character(data$delays_unit, len = 1)
  assert_choice(data$delays_unit,
    choices = c("days", "weeks", "months", "years")
  )
  return(NULL)
}
