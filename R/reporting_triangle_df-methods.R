#' @title Subset reporting_triangle_df objects
#'
#' @description
#' Extract parts of a reporting_triangle_df object while preserving its
#' attributes.
#'
#' @param x A [reporting_triangle_df] object
#' @param ... Indices for subsetting
#' @return A [reporting_triangle_df] object with the subset data
#' @family reporting_triangle_df
#' @method [ reporting_triangle_df
#' @export
`[.reporting_triangle_df` <- function(x, ...) {
  # Store attributes before subsetting
  old_class <- class(x)
  old_delays_unit <- attr(x, "delays_unit")
  old_strata <- attr(x, "strata")

  # Perform subsetting using NextMethod
  out <- NextMethod()

  # If result is not a data.frame, return as-is
  if (!is.data.frame(out)) {
    return(out)
  }

  # Restore class and attributes
  class(out) <- old_class
  attr(out, "delays_unit") <- old_delays_unit
  attr(out, "strata") <- old_strata

  # Only validate if all required columns are present
  # (subsetting by columns might remove required columns)
  required_cols <- c("reference_date", "report_date", "count")
  if (all(required_cols %in% names(out))) {
    assert_reporting_triangle_df(out)
  }

  return(out)
}

#' Convert reporting_triangle_df to plain data.frame
#'
#' Returns a plain data.frame representation of a reporting_triangle_df object,
#' removing the reporting_triangle_df class and custom attributes.
#'
#' @param x A [reporting_triangle_df] object.
#' @param row.names NULL or character vector giving row names. Default NULL.
#' @param optional Logical. If TRUE, setting row names and converting column
#'   names is optional. Default FALSE.
#' @param ... Additional arguments (not used).
#' @return A plain data.frame without reporting_triangle_df class or attributes.
#' @family reporting_triangle_df
#' @export
#' @method as.data.frame reporting_triangle_df
#' @examples
#' # Convert reporting_triangle_df to plain data.frame
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' plain_df <- as.data.frame(rt_df)
#' class(plain_df) # "data.frame"
as.data.frame.reporting_triangle_df <- function(
    x, row.names = NULL, optional = FALSE, ...) {
  # Remove class and attributes to get plain data.frame
  result <- as.data.frame(unclass(x))
  attr(result, "delays_unit") <- NULL
  attr(result, "strata") <- NULL

  if (!is.null(row.names)) {
    rownames(result) <- row.names
  }

  return(result)
}

#' Get first rows of a reporting_triangle_df
#'
#' @param x A [reporting_triangle_df] object.
#' @param n Integer indicating the number of rows to return. Default is 6.
#' @param ... Additional arguments (not currently used).
#' @return First rows as a reporting_triangle_df.
#' @family reporting_triangle_df
#' @export
#' @importFrom utils head
#' @method head reporting_triangle_df
#' @examples
#' # Get first 3 rows
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' head(rt_df, n = 3)
head.reporting_triangle_df <- function(x, n = 6L, ...) {
  n <- min(n, nrow(x))
  result <- x[seq_len(n), , drop = FALSE]
  return(result)
}

#' Get last rows of a reporting_triangle_df
#'
#' @param x A [reporting_triangle_df] object.
#' @param n Integer indicating the number of rows to return. Default is 6.
#' @param ... Additional arguments (not currently used).
#' @return Last rows as a reporting_triangle_df.
#' @family reporting_triangle_df
#' @export
#' @importFrom utils tail
#' @method tail reporting_triangle_df
#' @examples
#' # Get last 3 rows
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' tail(rt_df, n = 3)
tail.reporting_triangle_df <- function(x, n = 6L, ...) {
  n <- min(n, nrow(x))
  result <- x[seq.int(to = nrow(x), length.out = n), , drop = FALSE]
  return(result)
}

#' Print a reporting_triangle_df object
#'
#' @param x A [reporting_triangle_df] object to print.
#' @param n Maximum number of rows to display. Default is 10.
#'   Set to NULL to display all rows.
#' @param ... Additional arguments passed to print methods.
#' @return Invisibly returns the object.
#' @family reporting_triangle_df
#' @export
#' @importFrom cli cli_text
#' @method print reporting_triangle_df
print.reporting_triangle_df <- function(x, n = 10, ...) {
  # Capture output to prevent default print from showing first
  # Display metadata
  delays_unit <- attr(x, "delays_unit")
  strata <- attr(x, "strata")

  cat("Reporting Triangle DataFrame\n")
  cat("Delays unit:", delays_unit, "\n")

  if (!is.null(strata)) {
    cat("Strata:", paste(strata, collapse = ", "), "\n")
    # Count unique strata combinations
    n_strata <- nrow(unique(x[, strata, drop = FALSE]))
    cat("Number of strata:", n_strata, "\n")
  } else {
    cat("Strata: None (single stratum)\n")
  }

  # Compute and display reference date range
  ref_dates <- x$reference_date
  cat("Reference dates:", format(min(ref_dates)), "to", format(max(ref_dates)), "\n")

  # Display dimensions
  cat("Dimensions:", nrow(x), "rows\n\n")

  # Print data
  if (!is.null(n) && nrow(x) > n) {
    cat("Showing first", n, "of", nrow(x), "rows\n\n")
    print(as.data.frame(head(x, n)), ...)
    cat("\nUse print(x, n = NULL) to see all data\n")
  } else {
    print(as.data.frame(x), ...)
  }

  return(invisible(x))
}

#' Summarize a reporting_triangle_df object
#'
#' @param object A [reporting_triangle_df] object to summarize.
#' @param ... Additional arguments not used.
#' @return Invisibly returns the object.
#' @family reporting_triangle_df
#' @export
#' @importFrom cli cli_text
#' @method summary reporting_triangle_df
#' @examples
#' # Display summary statistics
#' rt_df <- as_reporting_triangle_df(syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ])
#' summary(rt_df)
summary.reporting_triangle_df <- function(object, ...) {
  # Display metadata
  delays_unit <- attr(object, "delays_unit")
  strata <- attr(object, "strata")

  cat("Reporting Triangle DataFrame Summary\n")
  cat("Delays unit:", delays_unit, "\n")

  if (!is.null(strata)) {
    cat("Strata columns:", paste(strata, collapse = ", "), "\n")
    # Count unique strata combinations
    strata_combos <- unique(object[, strata, drop = FALSE])
    n_strata <- nrow(strata_combos)
    cat("Number of unique strata:", n_strata, "\n")
  } else {
    cat("Strata: None (single stratum)\n")
  }

  # Display dimensions and date ranges
  cat("Dimensions:", nrow(object), "rows x", ncol(object), "columns\n")
  ref_dates <- object$reference_date
  report_dates <- object$report_date
  cat("Reference date range:", format(min(ref_dates)), "to", format(max(ref_dates)), "\n")
  cat("Report date range:", format(min(report_dates)), "to", format(max(report_dates)), "\n")

  # Compute and display delay statistics
  delays <- get_delays_from_dates(
    object$report_date,
    object$reference_date,
    delays_unit
  )
  cat("Delay range:", min(delays), "to", max(delays), delays_unit, "\n")

  # Count statistics
  total_count <- sum(object$count, na.rm = TRUE)
  cat("Total count:", total_count, "\n\n")

  # Display count summary
  cat("Count summary:\n")
  print(summary(object$count))

  return(invisible(object))
}
