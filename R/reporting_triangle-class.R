#' Reporting Triangle Object
#' @name reporting_triangle-class
#' @aliases reporting_triangle
#' @family reporting_triangle
#'
#' @description
#' A `reporting_triangle` object contains the data and metadata needed for
#' nowcasting.
#'
#' @section Structure:
#' A `reporting_triangle` is a matrix with class
#' `c("reporting_triangle", "matrix")`:
#'
#' - Rows: Reference dates
#' - Columns: Delays (0, 1, 2, ...)
#' - Row names: Reference dates as character
#' - Column names: Delays as character
#'
#' Attributes:
#' - `delays_unit`: Character ("days", "weeks", "months", "years")
#' - `structure`: Integer vector indicating reporting pattern
#'
#' Reference dates are stored as row names and can be extracted using
#' [get_reference_dates()].
#' See the corresponding [as_reporting_triangle.matrix()] and
#' [as_reporting_triangle.data.frame()] functions
#' for more details on the required input formats to generate the object.
#'
#' @section Low-level function compatibility:
#' The matrix-based structure works seamlessly with low-level functions:
#'
#' - [fill_triangle()]: Fill missing values with zeros
#' - [estimate_delay()]: Extract delay distribution from triangle
#' - [apply_delay()]: Apply delay distribution for nowcasting
#' - [truncate_triangle()]: Remove most recent rows
#' - Standard matrix operations: `rowSums()`, `colSums()`, subsetting
#'
#' @examples
#' # Create a reporting triangle from data
#' data <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' data$age_group <- "00+"
#' rep_tri <- as_reporting_triangle(
#'   data = data,
#'   max_delay = 10,
#'   strata = "00+"
#' )
#'
#' # Use with low-level functions
#' filled <- fill_triangle(rep_tri)
#' delay_pmf <- estimate_delay(rep_tri)
#' nowcast <- apply_delay(rep_tri, delay_pmf)
#'
#' # Direct matrix operations
#' total_by_date <- rowSums(rep_tri, na.rm = TRUE)
#' total_by_delay <- colSums(rep_tri, na.rm = TRUE)
#'
#' # Subsetting and inspection
#' recent <- tail(rep_tri, n = 10)
#' summary(rep_tri)
NULL

#' Get reference dates from reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of Date objects
#' @family reporting_triangle
#' @export
get_reference_dates <- function(x) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  return(as.Date(rownames(x)))
}

#' Get maximum delay from reporting_triangle
#'
#' @param x A reporting_triangle object
#' @param non_zero Logical. If TRUE, returns the maximum delay where at least
#'   one observation is non-zero. Useful for identifying the actual extent of
#'   the delay distribution. Default FALSE.
#' @return Maximum delay (integer), or -1 if all zero when non_zero = TRUE
#' @family reporting_triangle
#' @export
get_max_delay <- function(x, non_zero = FALSE) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }

  if (non_zero) {
    col_sums <- colSums(x, na.rm = TRUE)
    non_zero_cols <- which(col_sums > 0)
    if (length(non_zero_cols) == 0) {
      return(-1L)
    }
    return(max(non_zero_cols) - 1L)
  }

  return(ncol(x) - 1L)
}

#' Get mean delay for each row of reporting_triangle
#'
#' @param x A reporting_triangle object
#' @return Vector of mean delays for each reference date (numeric)
#' @family reporting_triangle
#' @export
get_mean_delay <- function(x) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  delays <- 0:get_max_delay(x)
  mean_delays <- vapply(seq_len(nrow(x)), function(i) {
    row_counts <- x[i, ]
    total_counts <- sum(row_counts, na.rm = TRUE)
    if (total_counts == 0) return(NA_real_)
    sum(row_counts * delays, na.rm = TRUE) / total_counts
  }, numeric(1))
  return(mean_delays)
}

#' Class constructor for `reporting_triangle` objects
#'
#' @param reporting_triangle_matrix Matrix of reporting triangle
#' @inheritParams as_reporting_triangle.matrix
#' @inheritParams construct_triangle
#' @inheritParams as_reporting_triangle
#'
#' @returns An object of class [reporting_triangle]
#' @family reporting_triangle
#' @export
new_reporting_triangle <- function(reporting_triangle_matrix,
                                   reference_dates,
                                   structure,
                                   max_delay,
                                   delays_unit,
                                   strata = NULL) {
  .validate_rep_tri_args(
    reporting_triangle_matrix,
    reference_dates,
    structure,
    max_delay,
    delays_unit,
    strata
  )

  rownames(reporting_triangle_matrix) <- as.character(reference_dates)
  colnames(reporting_triangle_matrix) <- as.character(0:max_delay)

  result <- structure(
    reporting_triangle_matrix,
    class = c("reporting_triangle", "matrix"),
    delays_unit = delays_unit,
    structure = structure
  )
  return(result)
}

#' Validate a reporting_triangle object
#'
#' @param data A [reporting_triangle] object to validate
#' @return The validated object (invisibly) or throws error
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_matrix assert_date assert_numeric
#'    assert_character assert_choice assert_list
validate_reporting_triangle <- function(data) {
  if (!inherits(data, "matrix")) {
    cli_abort(message = "data must be a matrix")
  }
  if (!inherits(data, "reporting_triangle")) {
    cli_abort(message = "data must have class 'reporting_triangle'")
  }

  reference_dates <- as.Date(rownames(data))
  tri_structure <- attr(data, "structure")
  delays_unit <- attr(data, "delays_unit")
  max_delay <- ncol(data) - 1

  .validate_rep_tri_args(
    reporting_triangle_matrix = data,
    reference_dates = reference_dates,
    structure = tri_structure,
    max_delay = max_delay,
    delays_unit = delays_unit,
    strata = NULL
  )

  if (sum(tri_structure) > ncol(data)) {
    cli_abort(message = c(
      "Sum of `structure` must not be greater than or equal",
      "to the number of columns in matrix"
    ))
  }

  invisible(data)
}

#' Assert validity of `reporting_triangle` objects
#'
#' @param data A [reporting_triangle] object to check for validity.
#' @return NULL
#' @family reporting_triangle
#' @export
assert_reporting_triangle <- function(data) {
  validate_reporting_triangle(data)
  return(NULL)
}

#' Check if an object is a reporting_triangle
#'
#' @param x An object to check.
#' @return Logical indicating whether the object is a reporting_triangle.
#' @family reporting_triangle
#' @export
is_reporting_triangle <- function(x) {
  return(inherits(x, "reporting_triangle"))
}

#' Subset a reporting_triangle object
#'
#' @param x A [reporting_triangle] object.
#' @param ... Indices for subsetting.
#' @return A subsetted reporting_triangle object.
#' @family reporting_triangle
#' @export
#' @method [ reporting_triangle
`[.reporting_triangle` <- function(x, ...) {
  # Store attributes before subsetting
  old_class <- class(x)
  old_delays_unit <- attr(x, "delays_unit")
  old_structure <- attr(x, "structure")

  out <- NextMethod()

  # Skip validation if object unchanged
  if (identical(x, out) && ...length() == 1) {
    return(out)
  }

  # Restore class and attributes
  class(out) <- old_class
  attr(out, "delays_unit") <- old_delays_unit
  attr(out, "structure") <- old_structure

  # Try to validate the subsetted object
  validation <- try(
    validate_reporting_triangle(out),
    silent = TRUE
  )

  if (inherits(validation, "try-error")) {
    cli_warn(c(
      "!" = "Subsetting produced an invalid reporting_triangle object.",
      "i" = "The object structure may be corrupted."
    ))
  }

  return(out)
}

#' Get first rows of a reporting_triangle
#'
#' @param x A [reporting_triangle] object.
#' @param ... Additional arguments passed to [utils::head()].
#' @return First rows as a reporting_triangle.
#' @family reporting_triangle
#' @export
#' @importFrom utils head
#' @method head reporting_triangle
head.reporting_triangle <- function(x, ...) {
  # Get default n if not specified
  dots <- list(...)
  n <- if (!is.null(dots[["n"]])) dots[["n"]] else 6L
  n <- min(n, nrow(x))
  return(x[seq_len(n), , drop = FALSE])
}

#' Get last rows of a reporting_triangle
#'
#' @param x A [reporting_triangle] object.
#' @param ... Additional arguments passed to [utils::tail()].
#' @return Last rows as a reporting_triangle.
#' @family reporting_triangle
#' @export
#' @importFrom utils tail
#' @method tail reporting_triangle
tail.reporting_triangle <- function(x, ...) {
  # Get default n if not specified
  dots <- list(...)
  n <- if (!is.null(dots[["n"]])) dots[["n"]] else 6L
  n <- min(n, nrow(x))
  return(x[seq.int(to = nrow(x), length.out = n), , drop = FALSE])
}

#' Print a reporting_triangle object
#'
#' @param x A [reporting_triangle] object to print.
#' @param ... Additional arguments passed to print methods.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @importFrom cli cli_text cli_rule
#' @method print reporting_triangle
print.reporting_triangle <- function(x, ...) {
  cli_text("{.strong Reporting Triangle}")
  cli_rule()
  ref_dates <- get_reference_dates(x)
  cli_text("Delays unit: {attr(x, 'delays_unit')}")
  cli_text(
    "Reference dates: {paste(format(range(ref_dates)), collapse = ' to ')}"
  )
  cli_text("Max delay: {ncol(x) - 1}")
  cli_text("Structure: {toString(attr(x, 'structure'))}")
  cli_text("")
  print(unclass(x), ...)
  return(invisible(x))
}

#' Summarize a reporting_triangle object
#'
#' @param object A [reporting_triangle] object to summarize.
#' @param ... Additional arguments not used.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @importFrom cli cli_text cli_rule
#' @method summary reporting_triangle
summary.reporting_triangle <- function(object, ...) {
  cli_text("{.strong Reporting Triangle Summary}")
  cli_rule()
  ref_dates <- get_reference_dates(object)
  cli_text("Dimensions: {nrow(object)} x {ncol(object)}")
  cli_text(
    "Reference period: {paste(format(range(ref_dates)), collapse = ' to ')}"
  )
  cli_text("Max delay: {ncol(object) - 1} {attr(object, 'delays_unit')}")
  cli_text("Structure: {toString(attr(object, 'structure'))}")

  # Find most recent complete date and calculate mean delays
  row_has_na <- apply(object, 1, anyNA)
  complete_rows <- which(!row_has_na)
  mean_delays <- NULL
  if (length(complete_rows) > 0) {
    most_recent_complete_idx <- max(complete_rows)
    most_recent_complete_date <- ref_dates[most_recent_complete_idx]
    most_recent_complete_count <- sum(
      object[most_recent_complete_idx, ],
      na.rm = TRUE
    )
    cli_text(
      "Most recent complete date: {format(most_recent_complete_date)} ",
      "({most_recent_complete_count} cases)"
    )

    # Calculate mean delay for complete rows
    delays <- 0:(ncol(object) - 1)
    mean_delays <- vapply(complete_rows, function(i) {
      row_counts <- object[i, ]
      total_counts <- sum(row_counts)
      if (total_counts == 0) return(NA_real_)
      sum(row_counts * delays) / total_counts
    }, numeric(1))

    mean_delays <- mean_delays[!is.na(mean_delays)]
  }

  # Count dates requiring nowcast
  dates_need_nowcast <- sum(row_has_na)
  cli_text("Dates requiring nowcast: {dates_need_nowcast}")

  # Count rows with negatives
  row_has_negative <- apply(object, 1, function(x) any(x < 0, na.rm = TRUE))
  rows_with_negatives <- sum(row_has_negative)
  cli_text("Rows with negatives: {rows_with_negatives}")

  # Count zeros
  num_zeros <- sum(object == 0, na.rm = TRUE)
  cli_text("Number of zeros: {num_zeros}")

  # Show summary of mean delays for complete rows
  if (!is.null(mean_delays) && length(mean_delays) > 0) {
    cli_text("")
    cli_text("{.strong Mean delay summary (complete rows):}")
    print(summary(mean_delays))
  }

  return(invisible(object))
}
