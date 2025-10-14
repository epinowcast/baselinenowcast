#' Create a `reporting_triangle` object
#'
#' @param data Data to be nowcasted.
#' @param max_delay Integer indicating the maximum delay.
#' @param strata_map Named list where each name is the grouping
#'    variable (e.g. `age_group`) and each value is the stratum (e.g. "0-18").
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
#' @return  A \code{\link{reporting_triangle}} object
#'
#' @family reporting_triangle
#' @export
as_reporting_triangle <- function(data,
                                  max_delay,
                                  strata_map = NULL,
                                  delays_unit = "days",
                                  ...) {
  UseMethod("as_reporting_triangle")
}

#' Create a `reporting_triangle` object from a data.frame
#'
#' This method takes a data.frame containing case counts indexed by reference
#' date and report date and creates a A \code{\link{reporting_triangle}} object.
#' See \code{\link{as_reporting_triangle.matrix}} for other data
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
    strata_map = NULL,
    delays_unit = "days",
    strata = NULL,
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
  # Create a named vector for renaming
  old_names <- c(reference_date, report_date, count)
  new_names <- c("reference_date", "report_date", "count")

  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  .validate_rep_tri_df(data, delays_unit)
  if (!is.null(strata_map) && !is.null(strata)) {
    cli_abort(
      message = "Cannot specify both `strata_map` and `strata`. Use `strata` to derive `strata_map` from the columns in `data`." # nolint
    )
  }
  # Create the named list of strata
  if (!is.null(strata)) {
    if (!all(strata %in% colnames(data))) {
      cli_abort(
        message = "`strata` specified are not columns in `data`."
      )
    }
    strata_map <- lapply(data[c(strata)], unique)
    if (!all(lengths(strata_map) == 1)) {
      cli_abort(
        message = "Multiple values found for the specified `strata` when trying to create a single `reporting_triangle` object." # nolint
      )
    }
  }


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
    strata_map = strata_map,
    delays_unit = delays_unit
  )
  return(rep_tri)
}


#' Create a `reporting_triangle` from a matrix
#'
#' This method takes a matrix in the format of a reporting triangle, with rows
#' as reference dates and columns as delays and elements as incident case
#' counts and creates a \code{\link{reporting_triangle}} object. See other
#' \code{\link{as_reporting_triangle.data.frame}}for other data
#' input options.
#'
#' @param data Matrix of a reporting triangle where rows are reference times,
#'    columns are delays, and entries are the incident counts.
#' @inheritParams as_reporting_triangle
#' @param reference_dates Vector of character strings indicating the reference
#'   dates corresponding to each row of the reporting triangle matrix (`data`).
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
                                         strata_map = NULL,
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
    strata_map = strata_map,
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
#' @returns An object of class \code{\link{reporting_triangle}]
#'
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   structure,
                                   max_delay,
                                   delays_unit,
                                   strata_map = NULL) {
  .validate_rep_tri_args(
    reporting_triangle_matrix,
    reference_dates,
    structure,
    max_delay,
    delays_unit,
    strata_map
  )
  result <- structure(
    list(
      reporting_triangle_matrix = reporting_triangle_matrix,
      reference_dates = reference_dates,
      structure = structure,
      max_delay = max_delay,
      delays_unit = delays_unit,
      strata_map = strata_map
    ),
    class = "reporting_triangle"
  )
  return(result)
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A \code{\link{reporting_triangle}} object to check for validity.
#' @return NULL
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
assert_reporting_triangle <- function(data) {
  .validate_rep_tri_args(
    reporting_triangle_matrix = data$reporting_triangle_matrix,
    reference_dates = data$reference_dates,
    structure = data$structure,
    max_delay = data$max_delay,
    delays_unit = data$delays_unit,
    strata_map = data$strata_map
  )

  if (sum(data$structure) > ncol(data$reporting_triangle_matrix)) {
    cli_abort(message = c(
      message = c(
        "Sum of `structure` must not be greater than or equal",
        "to the number of columns in matrix"
      )
    ))
  }

  return(NULL)
}
