#' @title Subset reporting_triangle objects
#'
#' @description
#' Extract or replace parts of a reporting_triangle object while preserving its
#' attributes.
#'
#' @param x A [reporting_triangle] object
#' @param ... Indices for subsetting
#' @return A [reporting_triangle] object with the subset data
#' @family reporting_triangle
#' @method [ reporting_triangle
#' @export
`[.reporting_triangle` <- function(x, ...) {
  # Store attributes before subsetting
  old_class <- class(x)
  old_delays_unit <- get_delays_unit(x)

  out <- NextMethod()

  # If result is not a matrix (e.g., single row/column as vector),
  # return as-is
  if (!is.matrix(out)) {
    return(out)
  }

  # Restore class and attributes
  class(out) <- old_class
  attr(out, "delays_unit") <- old_delays_unit

  # Validate the subsetted result
  assert_reporting_triangle(out)

  return(out)
}

#' @title Subset assignment for reporting_triangle objects
#'
#' @description
#' Assignment method that allows modification of reporting_triangle values
#' while validating the result remains a valid reporting_triangle.
#'
#' @param x A [reporting_triangle] object.
#' @param ... Row and column indices for assignment.
#' @param value Values to assign.
#' @return The modified reporting_triangle object.
#' @family reporting_triangle
#' @export
#' @method [<- reporting_triangle
`[<-.reporting_triangle` <- function(x, ..., value) {
  # Perform the assignment using NextMethod
  x_modified <- NextMethod()

  # Preserve class and attributes if still a matrix
  if (is.matrix(x_modified)) {
    class(x_modified) <- class(x)
    attr(x_modified, "delays_unit") <- get_delays_unit(x)

    # Validate the modified result
    assert_reporting_triangle(x_modified)
  }

  return(x_modified)
}

#' Convert reporting_triangle to plain matrix
#'
#' Returns a plain matrix representation of a reporting_triangle object,
#' removing the reporting_triangle class and custom attributes while
#' preserving row and column names.
#'
#' @param x A [reporting_triangle] object.
#' @param ... Additional arguments (not used).
#' @return A plain matrix without reporting_triangle class or attributes.
#' @family reporting_triangle
#' @export
#' @method as.matrix reporting_triangle
#' @examples
#' # Convert reporting_triangle to plain matrix
#' plain_mat <- as.matrix(example_downward_corr_rt)
#' class(plain_mat)  # "matrix" "array"
as.matrix.reporting_triangle <- function(x, ...) {
  # Store row and column names
  rn <- rownames(x)
  cn <- colnames(x)

  # Remove class and attributes to get plain matrix
  result <- unclass(x)
  attributes(result) <- NULL

  # Restore dimensions and dimnames
  dim(result) <- dim(x)
  rownames(result) <- rn
  colnames(result) <- cn

  return(result)
}

#' Convert reporting_triangle to data.frame
#'
#' @param x A [reporting_triangle] object to convert.
#' @param row.names NULL or character vector giving row names for the data
#'   frame. Missing values are not allowed.
#' @param optional Logical. If TRUE, setting row names and converting column
#'   names is optional.
#' @param ... Additional arguments to be passed to or from methods.
#' @return A data.frame with columns reference_date, report_date, delay, count
#' @family reporting_triangle
#' @export
#' @method as.data.frame reporting_triangle
#' @examples
#' # Convert reporting triangle to data frame
#' df <- as.data.frame(example_reporting_triangle)
#' head(df)
as.data.frame.reporting_triangle <- function(
    x, row.names = NULL, optional = FALSE, ...) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }

  reference_dates <- get_reference_dates(x)
  delays_unit <- get_delays_unit(x)

  # Create grid of all non-NA positions
  mat <- as.matrix(x)
  non_na <- !is.na(mat)
  row_indices <- row(mat)[non_na]
  col_indices <- col(mat)[non_na]
  counts <- mat[non_na]
  delays <- col_indices - 1
  ref_dates <- reference_dates[row_indices]

  # Compute report dates using unit-aware date arithmetic
  report_dates <- get_report_dates(ref_dates, delays, delays_unit)

  # Build data.frame efficiently
  result <- data.frame(
    reference_date = ref_dates,
    report_date = report_dates,
    delay = delays,
    count = counts,
    stringsAsFactors = FALSE
  )

  rownames(result) <- row.names

  return(result)
}

#' Get first rows of a reporting_triangle
#'
#' @param x A [reporting_triangle] object.
#' @param n Integer indicating the number of rows to return. Default is 6.
#' @param ... Additional arguments (not currently used).
#' @return First rows as a reporting_triangle.
#' @family reporting_triangle
#' @export
#' @importFrom utils head
#' @method head reporting_triangle
#' @examples
#' # Get first 3 rows
#' head(example_reporting_triangle, n = 3)
head.reporting_triangle <- function(x, n = 6L, ...) {
  n <- min(n, nrow(x))
  # Convert to matrix, subset, then restore class
  mat <- as.matrix(x)
  subset_mat <- mat[seq_len(n), , drop = FALSE]
  # .update_triangle_matrix will extract reference_dates from rownames
  return(.update_triangle_matrix(x, subset_mat))
}

#' Get last rows of a reporting_triangle
#'
#' @param x A [reporting_triangle] object.
#' @param n Integer indicating the number of rows to return. Default is 6.
#' @param ... Additional arguments (not currently used).
#' @return Last rows as a reporting_triangle.
#' @family reporting_triangle
#' @export
#' @importFrom utils tail
#' @method tail reporting_triangle
#' @examples
#' # Get last 3 rows
#' tail(example_reporting_triangle, n = 3)
tail.reporting_triangle <- function(x, n = 6L, ...) {
  n <- min(n, nrow(x))
  # Convert to matrix, subset, then restore class
  mat <- as.matrix(x)
  subset_mat <- mat[seq.int(to = nrow(mat), length.out = n), , drop = FALSE]
  # .update_triangle_matrix will extract reference_dates from rownames
  return(.update_triangle_matrix(x, subset_mat))
}

#' Get formatted reference date range string
#'
#' Internal helper to format the reference date range for display.
#'
#' @param x A [reporting_triangle] object.
#' @return Character string with formatted date range.
#' @keywords internal
.format_reference_date_range <- function(x) {
  ref_dates <- get_reference_dates(x)
  return(paste(format(range(ref_dates)), collapse = " to "))
}

#' Get triangle dimension information
#'
#' Internal helper to extract key dimension information from a triangle.
#'
#' @param x A [reporting_triangle] object.
#' @return List with components: n_rows, n_cols, max_delay, delays_unit,
#'   structure.
#' @keywords internal
.get_triangle_info <- function(x) {
  return(list(
    n_rows = nrow(x),
    n_cols = ncol(x),
    max_delay = get_max_delay(x),
    delays_unit = get_delays_unit(x),
    structure = toString(get_reporting_structure(x))
  ))
}

#' Display basic triangle information
#'
#' Internal helper to print common triangle metadata used by both print and
#' summary methods.
#'
#' @param x A [reporting_triangle] object.
#' @param show_dimensions Logical. If TRUE, displays dimensions (rows x cols).
#'   Default FALSE.
#' @return NULL (displays information via cli).
#' @keywords internal
#' @importFrom cli cli_text
.display_triangle_basics <- function(x, show_dimensions = FALSE) {
  info <- .get_triangle_info(x)

  if (show_dimensions) {
    cli_text("Dimensions: {info$n_rows} x {info$n_cols}")
    cli_text("Reference period: {(.format_reference_date_range(x))}")
    cli_text("Max delay: {info$max_delay} {info$delays_unit}")
  } else {
    cli_text("Delays unit: {info$delays_unit}")
    cli_text("Reference dates: {(.format_reference_date_range(x))}")
    cli_text("Max delay: {info$max_delay}")
  }
  cli_text("Structure: {info$structure}")

  return(invisible(NULL))
}

#' Compute complete row statistics
#'
#' @param object A [reporting_triangle] object.
#' @param mat Matrix representation of the triangle.
#' @param ref_dates Vector of reference dates.
#' @return List with most_recent_complete_idx, most_recent_complete_date,
#'   most_recent_complete_count, mean_delays, and complete_rows (or NULL if
#'   no complete rows).
#' @keywords internal
.compute_complete_row_stats <- function(object, mat, ref_dates) {
  row_has_na <- apply(mat, 1, anyNA)
  complete_rows <- which(!row_has_na)

  if (length(complete_rows) == 0) {
    return(NULL)
  }

  most_recent_complete_idx <- max(complete_rows)
  most_recent_complete_date <- ref_dates[most_recent_complete_idx]
  most_recent_complete_count <- sum(
    mat[most_recent_complete_idx, ],
    na.rm = TRUE
  )

  all_mean_delays <- get_mean_delay(object)
  mean_delays <- all_mean_delays[complete_rows]
  mean_delays <- mean_delays[!is.na(mean_delays)]

  return(list(
    most_recent_complete_idx = most_recent_complete_idx,
    most_recent_complete_date = most_recent_complete_date,
    most_recent_complete_count = most_recent_complete_count,
    mean_delays = mean_delays,
    complete_rows = complete_rows
  ))
}

#' Compute zero value statistics
#'
#' @param mat Matrix representation of the triangle.
#' @return List with num_zeros, pct_zeros, and zeros_per_row.
#' @keywords internal
.compute_zero_stats <- function(mat) {
  num_zeros <- sum(mat == 0, na.rm = TRUE)
  num_non_na <- sum(!is.na(mat))
  pct_zeros <- if (num_non_na > 0) {
    round(100 * num_zeros / num_non_na, 1)
  } else {
    0
  }
  zeros_per_row <- apply(mat, 1, function(x) sum(x == 0, na.rm = TRUE))

  return(list(
    num_zeros = num_zeros,
    pct_zeros = pct_zeros,
    zeros_per_row = zeros_per_row
  ))
}

#' Compute nowcast requirement statistics
#'
#' @param mat Matrix representation of the triangle.
#' @return List with dates_need_nowcast and dates_complete.
#' @keywords internal
.compute_nowcast_stats <- function(mat) {
  row_has_na <- apply(mat, 1, anyNA)
  dates_need_nowcast <- sum(row_has_na)
  dates_complete <- nrow(mat) - dates_need_nowcast

  return(list(
    dates_need_nowcast = dates_need_nowcast,
    dates_complete = dates_complete
  ))
}

#' Count rows with negative values
#'
#' @param mat Matrix representation of the triangle.
#' @return Integer count of rows with at least one negative value.
#' @keywords internal
.count_negative_rows <- function(mat) {
  row_has_negative <- apply(mat, 1, function(x) any(x < 0, na.rm = TRUE))
  return(sum(row_has_negative))
}

#' Compute quantile delay statistics for complete rows
#'
#' @param object A [reporting_triangle] object.
#' @param complete_rows Indices of complete rows.
#' @param p Quantile probability (default 0.99).
#' @return Vector of quantile delays for complete rows, or NULL if none.
#' @keywords internal
.compute_quantile_delay_stats <- function(object, complete_rows,
                                          p = 0.99) {
  if (length(complete_rows) == 0) {
    return(NULL)
  }

  quantile_delays <- get_quantile_delay(object, p = p)
  quantile_complete <- quantile_delays[complete_rows]
  quantile_complete <- quantile_complete[!is.na(quantile_complete)]

  if (length(quantile_complete) == 0) {
    return(NULL)
  }

  return(quantile_complete)
}

#' Print a reporting_triangle object
#'
#' @param x A [reporting_triangle] object to print.
#' @param n_rows Maximum number of rows to display. If the triangle has more
#'   rows, only the last `n_rows` rows are shown. Default is 10.
#'   Set to NULL to display all rows.
#' @param n_cols Maximum number of columns to display. If the triangle has more
#'   columns, only the first `n_cols` columns are shown. Default is 10.
#'   Set to NULL to display all columns.
#' @param ... Additional arguments passed to print methods.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @importFrom cli cli_text
#' @method print reporting_triangle
print.reporting_triangle <- function(x, n_rows = 10, n_cols = 10, ...) {
  cli_text("{.strong Reporting Triangle}")
  .display_triangle_basics(x, show_dimensions = FALSE)
  cli_text("")

  to_print <- x
  total_rows <- nrow(x)
  total_cols <- ncol(x)
  row_subset <- NULL
  col_subset <- NULL

  if (!is.null(n_rows) && total_rows > n_rows) {
    row_subset <- seq(total_rows - n_rows + 1, total_rows)
    to_print <- to_print[row_subset, , drop = FALSE]
    cli_text("Showing last {n_rows} of {total_rows} rows")
  }

  if (!is.null(n_cols) && total_cols > n_cols) {
    col_subset <- seq_len(n_cols)
    to_print <- to_print[, col_subset, drop = FALSE]
    cli_text("Showing first {n_cols} of {total_cols} columns")
  }

  if (!is.null(row_subset) || !is.null(col_subset)) {
    cli_text("")
  }

  print(as.matrix(to_print), ...)

  if (!is.null(row_subset) || !is.null(col_subset)) {
    cli_text("")
    cli_text(
      "Use print(x, n_rows = NULL, n_cols = NULL) to see all data"
    )
  }

  return(invisible(x))
}

#' Summarize a reporting_triangle object
#'
#' @param object A [reporting_triangle] object to summarize.
#' @param ... Additional arguments not used.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @importFrom cli cli_text
#' @method summary reporting_triangle
#' @examples
#' # Display summary statistics
#' summary(example_reporting_triangle)
summary.reporting_triangle <- function(object, ...) {
  cli_text("{.strong Reporting Triangle Summary}")
  .display_triangle_basics(object, show_dimensions = TRUE)
  ref_dates <- get_reference_dates(object)

  # Convert to plain matrix for internal operations
  mat <- as.matrix(object)

  # Compute complete row statistics
  complete_stats <- .compute_complete_row_stats(object, mat, ref_dates)
  if (!is.null(complete_stats)) {
    # nolint start: object_usage_linter
    most_recent_complete_date <- complete_stats$most_recent_complete_date
    most_recent_complete_count <- complete_stats$most_recent_complete_count
    # nolint end
    cli_text(
      "Most recent complete date: {format(most_recent_complete_date)} ",
      "({most_recent_complete_count} cases)"
    )
  }

  # Compute and display nowcast requirement statistics
  nowcast_stats <- .compute_nowcast_stats(mat)
  # nolint start: object_usage_linter
  dates_need_nowcast <- nowcast_stats$dates_need_nowcast
  dates_complete <- nowcast_stats$dates_complete
  # nolint end
  cli_text(
    "Dates requiring nowcast: {dates_need_nowcast} ",
    "(complete: {dates_complete})"
  )

  # Count and display rows with negatives
  rows_with_negatives <- .count_negative_rows(mat) # nolint: object_usage_linter
  cli_text("Rows with negatives: {rows_with_negatives}")

  # Compute and display zero statistics
  zero_stats <- .compute_zero_stats(mat)
  num_zeros <- zero_stats$num_zeros # nolint: object_usage_linter
  pct_zeros <- zero_stats$pct_zeros # nolint: object_usage_linter
  zeros_per_row <- zero_stats$zeros_per_row
  cli_text("Zeros: {num_zeros} ({pct_zeros}% of non-NA values)")
  if (length(zeros_per_row) > 0) {
    cli_text("Zeros per row summary:")
    print(summary(zeros_per_row))
  }

  # Show summary of mean delays for complete rows
  if (!is.null(complete_stats) && length(complete_stats$mean_delays) > 0) {
    cli_text("")
    cli_text("{.strong Mean delay summary (complete rows):}")
    print(summary(complete_stats$mean_delays))
  }

  # Show 99% quantile delay statistics
  if (!is.null(complete_stats)) {
    quantile_99_complete <- .compute_quantile_delay_stats(
      object,
      complete_stats$complete_rows,
      p = 0.99
    )
    if (!is.null(quantile_99_complete)) {
      cli_text("")
      cli_text("{.strong 99% quantile delay (complete rows):}")
      print(summary(quantile_99_complete))
    }
  }

  return(invisible(object))
}
