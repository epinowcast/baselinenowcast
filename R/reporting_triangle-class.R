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
  as.Date(rownames(x))
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
  x_mat <- unclass(x)
  mean_delays <- vapply(seq_len(nrow(x_mat)), function(i) {
    row_counts <- x_mat[i, ]
    total_counts <- sum(row_counts, na.rm = TRUE)
    if (total_counts == 0) return(NA_real_)
    return(sum(row_counts * delays, na.rm = TRUE) / total_counts)
  }, numeric(1))
  return(mean_delays)
}

#' Get quantile delay for each row of reporting_triangle
#'
#' @param x A reporting_triangle object
#' @param p Numeric value between 0 and 1 indicating the quantile to compute.
#'   For example, p = 0.99 returns the delay at which 99% of cases have been
#'   reported. Default is 0.99.
#' @return Vector of quantile delays for each reference date (integer). Returns
#'   NA for rows with no observations.
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_numeric
get_quantile_delay <- function(x, p = 0.99) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  assert_numeric(p, lower = 0, upper = 1, len = 1)

  delays <- 0:get_max_delay(x)
  x_mat <- unclass(x)
  quantile_delays <- vapply(seq_len(nrow(x_mat)), function(i) {
    row_counts <- x_mat[i, ]
    total_counts <- sum(row_counts, na.rm = TRUE)
    if (total_counts == 0) return(NA_integer_)

    cumulative_counts <- cumsum(row_counts)
    cumulative_prop <- cumulative_counts / total_counts
    quantile_idx <- which(cumulative_prop >= p)[1]

    if (is.na(quantile_idx)) {
      return(delays[length(delays)])
    }
    return(delays[quantile_idx])
  }, integer(1))
  return(quantile_delays)
}

#' Truncate reporting_triangle to quantile-based maximum delay
#'
#' Automatically determines an appropriate maximum delay based on when a
#' specified proportion of cases have been reported (CDF cutoff). This is useful
#' for reducing computational burden when most cases are reported within a
#' shorter delay window.
#'
#' @param x A reporting_triangle object
#' @param p Numeric value between 0 and 1 indicating the quantile cutoff.
#'   For example, p = 0.99 truncates to the delay at which 99% of cases have
#'   been reported. Default is 0.99.
#' @return A reporting_triangle object truncated to the maximum quantile delay,
#'   or the original object if no truncation is needed
#' @family reporting_triangle
#' @export
#' @importFrom checkmate assert_numeric
#' @importFrom cli cli_alert_info
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' data_as_of_df$age_group <- "00+"
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25,
#'   strata = "00+"
#' )
#'
#' # Original has 26 columns (delays 0-25)
#' ncol(rep_tri)
#'
#' # Truncate to 99th percentile of reporting
#' rep_tri_trunc <- truncate_to_quantile(rep_tri, p = 0.99)
#' ncol(rep_tri_trunc)
#'
#' # More aggressive truncation
#' rep_tri_trunc90 <- truncate_to_quantile(rep_tri, p = 0.90)
#' ncol(rep_tri_trunc90)
truncate_to_quantile <- function(x, p = 0.99) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }
  assert_numeric(p, lower = 0, upper = 1, len = 1)

  # Get quantile delays for each reference date
  quantile_delays <- get_quantile_delay(x, p = p)

  # Find maximum delay needed across all reference dates
  max_delay_needed <- suppressWarnings(max(quantile_delays, na.rm = TRUE))

  # If max_delay_needed is invalid (NA, -Inf, or -1), return original
  if (is.na(max_delay_needed) || is.infinite(max_delay_needed) ||
    max_delay_needed < 0) {
    cli_alert_info(
      text = "Could not determine quantile delay, returning original triangle."
    )
    return(x)
  }

  # Ensure max_delay is at least 1 (for validation requirement)
  max_delay_needed <- max(1L, as.integer(max_delay_needed))

  current_max_delay <- get_max_delay(x)

  # Only truncate if we can reduce the max_delay
  if (max_delay_needed < current_max_delay) {
    cli_alert_info(
      text = "Truncating from max_delay = {current_max_delay} to {max_delay_needed} based on {p * 100}% quantile." # nolint
    )

    # Subset columns
    trunc_mat <- unclass(x)[, 1:(max_delay_needed + 1), drop = FALSE]

    # Create new reporting_triangle with updated max_delay
    result <- new_reporting_triangle(
      reporting_triangle_matrix = trunc_mat,
      reference_dates = get_reference_dates(x),
      structure = attr(x, "structure"),
      max_delay = max_delay_needed,
      delays_unit = attr(x, "delays_unit")
    )

    return(result)
  } else {
    cli_alert_info(
      text = "No truncation needed: {p * 100}% of cases reported within existing max_delay = {current_max_delay}." # nolint
    )
    return(x)
  }
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

  return(invisible(data))
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

  # If result is not a matrix (e.g., single row/column), return as-is
  if (!is.matrix(out)) {
    return(out)
  }

  # Restore class and attributes
  class(out) <- old_class
  attr(out, "delays_unit") <- old_delays_unit
  attr(out, "structure") <- old_structure

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
#' @param n_rows Maximum number of rows to display. If the triangle has more
#'   rows, only the first and last `n_rows/2` rows are shown. Default is 10.
#'   Set to NULL to display all rows.
#' @param n_cols Maximum number of columns to display. If the triangle has more
#'   columns, only the first `n_cols` columns are shown. Default is 10.
#'   Set to NULL to display all columns.
#' @param ... Additional arguments passed to print methods.
#' @return Invisibly returns the object.
#' @family reporting_triangle
#' @export
#' @importFrom cli cli_text cli_rule
#' @method print reporting_triangle
print.reporting_triangle <- function(x, n_rows = 10, n_cols = 10, ...) {
  cli_text("{.strong Reporting Triangle}")
  cli_rule()
  ref_dates <- get_reference_dates(x) # nolint: object_usage_linter
  cli_text("Delays unit: {attr(x, 'delays_unit')}")
  cli_text(
    "Reference dates: {paste(format(range(ref_dates)), collapse = ' to ')}"
  )
  cli_text("Max delay: {get_max_delay(x)}")
  cli_text("Structure: {toString(attr(x, 'structure'))}")
  cli_text("")

  to_print <- x
  total_rows <- nrow(x)
  total_cols <- ncol(x)
  row_subset <- NULL
  col_subset <- NULL

  if (!is.null(n_rows) && total_rows > n_rows) {
    n_show <- floor(n_rows / 2)
    row_subset <- c(seq_len(n_show), seq(total_rows - n_show + 1, total_rows))
    to_print <- to_print[row_subset, , drop = FALSE]
    cli_text("Showing first and last {n_show} of {total_rows} rows")
  }

  if (!is.null(n_cols) && total_cols > n_cols) {
    col_subset <- seq_len(n_cols)
    to_print <- to_print[, col_subset, drop = FALSE]
    cli_text("Showing first {n_cols} of {total_cols} columns")
  }

  if (!is.null(row_subset) || !is.null(col_subset)) {
    cli_text("")
  }

  print(unclass(to_print), ...)
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
  max_delay <- get_max_delay(object) # nolint: object_usage_linter
  cli_text("Max delay: {max_delay} {attr(object, 'delays_unit')}")
  cli_text("Structure: {toString(attr(object, 'structure'))}")

  # Use unclassed version for internal matrix operations to avoid warnings
  mat <- unclass(object)

  # Find most recent complete date and calculate mean delays
  row_has_na <- apply(mat, 1, anyNA)
  complete_rows <- which(!row_has_na)
  mean_delays <- NULL
  if (length(complete_rows) > 0) {
    most_recent_complete_idx <- max(complete_rows)
    # nolint start: object_usage_linter
    most_recent_complete_date <- ref_dates[most_recent_complete_idx]
    most_recent_complete_count <- sum(
      mat[most_recent_complete_idx, ],
      na.rm = TRUE
    )
    # nolint end
    cli_text(
      "Most recent complete date: {format(most_recent_complete_date)} ",
      "({most_recent_complete_count} cases)"
    )

    # Calculate mean delay for complete rows
    all_mean_delays <- get_mean_delay(object)
    mean_delays <- all_mean_delays[complete_rows]
    mean_delays <- mean_delays[!is.na(mean_delays)]
  }

  # Count dates requiring nowcast
  dates_need_nowcast <- sum(row_has_na) # nolint: object_usage_linter
  cli_text("Dates requiring nowcast: {dates_need_nowcast}")

  # Count rows with negatives
  row_has_negative <- apply(mat, 1, function(x) any(x < 0, na.rm = TRUE))
  rows_with_negatives <- sum(row_has_negative) # nolint: object_usage_linter
  cli_text("Rows with negatives: {rows_with_negatives}")

  # Count zeros (percentage and row-wise)
  num_zeros <- sum(mat == 0, na.rm = TRUE)
  num_non_na <- sum(!is.na(mat))
  # nolint start: object_usage_linter
  pct_zeros <- if (num_non_na > 0) {
    round(100 * num_zeros / num_non_na, 1)
  } else {
    0
  }
  zeros_per_row <- apply(mat, 1, function(x) sum(x == 0, na.rm = TRUE))
  # nolint end
  cli_text("Zeros: {num_zeros} ({pct_zeros}% of non-NA values)")
  if (length(zeros_per_row) > 0) {
    cli_text("Zeros per row summary:")
    print(summary(zeros_per_row))
  }

  # Show summary of mean delays for complete rows
  if (!is.null(mean_delays) && length(mean_delays) > 0) {
    cli_text("")
    cli_text("{.strong Mean delay summary (complete rows):}")
    print(summary(mean_delays))
  }

  # Show 99% quantile delay (when 99% of reporting has occurred)
  if (length(complete_rows) > 0) {
    quantile_99_delays <- get_quantile_delay(object, p = 0.99)
    quantile_99_complete <- quantile_99_delays[complete_rows]
    quantile_99_complete <- quantile_99_complete[!is.na(quantile_99_complete)]

    if (length(quantile_99_complete) > 0) {
      cli_text("")
      cli_text("{.strong 99% quantile delay (complete rows):}")
      print(summary(quantile_99_complete))
    }
  }

  return(invisible(object))
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
as.data.frame.reporting_triangle <- function(
    x, row.names = NULL, optional = FALSE, ...) {
  if (!is_reporting_triangle(x)) {
    cli_abort(message = "x must have class 'reporting_triangle'")
  }

  reference_dates <- get_reference_dates(x)
  delays_unit <- attr(x, "delays_unit")

  result_list <- list()
  idx <- 1

  for (i in seq_len(nrow(x))) {
    ref_date <- reference_dates[i]

    for (j in seq_len(ncol(x))) {
      delay <- j - 1
      count <- x[i, j]

      if (!is.na(count)) {
        report_date <- ref_date + delay * switch(
          delays_unit,
          days = 1,
          weeks = 7,
          months = 30,
          years = 365
        )

        result_list[[idx]] <- data.frame(
          reference_date = ref_date,
          report_date = report_date,
          delay = delay,
          count = count,
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
  }

  result <- do.call(rbind, result_list)
  rownames(result) <- row.names

  return(result)
}
