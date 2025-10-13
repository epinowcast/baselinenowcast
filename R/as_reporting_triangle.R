#' Create a `reporting_triangle` object
#'
#' @param data Data to be nowcasted.
#' @param max_delay Integer indicating the maximum delay.
#' @param strata Character string indicating the metadata on the strata of this
#'    reporting triangle. Default is `NULL`.
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
                                  strata = NULL,
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

  # Create a named vector for renaming
  old_names <- c(reference_date, report_date, count)
  new_names <- c("reference_date", "report_date", "count")

  names(data)[names(data) %in% old_names] <- new_names[match(
    names(data)[names(data) %in% old_names], old_names
  )]

  .validate_rep_tri_df(data, delays_unit)

  # Create the named list of strata
  if (!is.null(strata)) {
    if (!all(strata %in% colnames(data))) {
      cli_abort(
        message = "`strata` specified are not columns in `data`."
      )
    }
    strata_list <- lapply(data[c(strata)], unique)

    if (all(lengths(strata_list)) != 1) {
      cli_abort(
        message = "Multiple values found for the specified `strata` when trying to create a single `reporting_triangle` object." # nolint
      )
    }
  } else {
    strata_list <- NULL
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
    strata = strata_list,
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
#' @param strata Named list indicating where the name will correspond to
#'   the column name and the single entry will correspond to the variable within
#'   the column.
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
  assert_list(strata, names = "named", null.ok = TRUE)
  if ((all(lengths(strata) != 1)) && !is.null(strata)) {
    cli_abort(
      message = c("A single `reporting_triangle` object can only be made from one `strata`", # nolint
        "i" = "Check that the `strata` columns in `data` have a single set of unique entries." # nolint
      )
    )
  }
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
#' @param data A `reporting_triangle` object to check for validity.
#' @return NULL
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
assert_reporting_triangle <- function(data) {
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
  assert_list(data$strata, names = "named", null.ok = TRUE)
  if (lengths(data$strata) != 1 && !is.null(strata)) {
    cli_abort(
      message = c("Multiple values found for the specified `strata`.",
        "i" = "Objects of class `reporting_triangle` may only have a single strata." # nolint
      )
    )
  }
  assert_character(data$delays_unit, len = 1)
  assert_choice(data$delays_unit,
    choices = c("days", "weeks", "months", "years")
  )
  return(NULL)
}
