#' Convert reporting_triangle to reviser vintages format
#'
#' This function converts a [reporting_triangle] object to a vintages object
#' in the wide format used by the
#' [reviser](https://CRAN.R-project.org/package=reviser) package.
#' `reviser` provides tools for analysing data revisions and vintages,
#' including state space nowcasting models and methods for assessing when
#' releases become stable.
#' Converting to reviser format enables use of those methods alongside
#' baselinenowcast's nowcasting functionality.
#'
#' @param x A [reporting_triangle] object to convert.
#' @param ... Additional arguments passed to [reviser::vintages_wide()].
#'
#' @return A tibble of class `tbl_pubdate` with a `time` column for the
#'   reference dates and one column per publication date containing the
#'   cumulative reported value as known on that publication date.
#'
#' @details
#' A `reporting_triangle` stores *incremental* counts at each delay column,
#' while a reviser vintages object stores the *cumulative* reported value at
#' each publication date.
#' The conversion takes the row-wise cumulative sum of the reporting triangle
#' and maps each (reference date, delay) cell to a publication date
#' `reference_date + delay` in the triangle's delay unit.
#' The long form is then pivoted to wide format using
#' [reviser::vintages_wide()].
#'
#' `NA` values in the triangle propagate through the cumulative sum, so any
#' unobserved cells in the original triangle remain `NA` in the vintages
#' output.
#'
#' To convert back to a [reporting_triangle] object, use
#' [as_reporting_triangle.tbl_pubdate()].
#'
#' @seealso
#' - [as_reporting_triangle.tbl_pubdate()] for converting back
#' - [reviser package documentation](https://github.com/cynkra/reviser)
#'
#' @family reporting_triangle
#' @export
#'
#' @examplesIf requireNamespace("reviser", quietly = TRUE)
#' # Create a reporting triangle from synthetic NSSP data
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data = data_as_of_df)
#'
#' # Convert to reviser vintages (wide) format
#' vintages <- as_reviser_vintages(rep_tri)
#' print(vintages)
as_reviser_vintages <- function(x, ...) {
  if (!requireNamespace("reviser", quietly = TRUE)) { # nolint: missing_package_linter
    cli::cli_abort(
      c(
        "Package {.pkg reviser} is required to convert to reviser vintages
        format.",
        i = "Install it with: {.code install.packages(\"reviser\")}"
      )
    )
  }

  assert_reporting_triangle(x)

  long_df <- as.data.frame(x)
  long_df <- long_df[
    order(long_df$reference_date, long_df$delay), ,
    drop = FALSE
  ]
  long_df$value <- unlist(
    by(long_df$count, long_df$reference_date, cumsum, simplify = FALSE),
    use.names = FALSE
  )
  long_df <- data.frame(
    time = long_df$reference_date,
    pub_date = long_df$report_date,
    value = long_df$value
  )

  return(reviser::vintages_wide(long_df, ...)) # nolint: namespace_linter
}

#' Convert reviser vintages to reporting_triangle format
#'
#' This S3 method converts a reviser vintages object (wide format, class
#' `tbl_pubdate`) to a [reporting_triangle] object, enabling use of
#' baselinenowcast's nowcasting methods.
#'
#' @param data A reviser vintages object in wide format (class `tbl_pubdate`),
#'   with a `time` column and one column per publication date.
#' @param ... Additional arguments passed to
#'   [as_reporting_triangle.data.frame()].
#'
#' @return A [reporting_triangle] object.
#'   See [reporting_triangle-class] for details on the structure.
#'
#' @param delays_unit Character string specifying the temporal granularity of
#'   the delays, one of `"days"`, `"weeks"`, `"months"`, or `"years"`. If
#'   `NULL` (default), the unit is inferred from the smallest non-zero
#'   `pub_date - time` gap in `data` (1 day -> `"days"`, 7 days -> `"weeks"`).
#'   Pass it explicitly to override or for monthly/yearly triangles.
#'
#' @details
#' Reviser vintages store cumulative reported values at each publication date.
#' The conversion takes differences between successive publication dates per
#' time point to recover incremental counts, and uses the delay implied by
#' `pub_date - time` (in `delays_unit`) to build the reporting triangle.
#'
#' A `tbl_pubdate` carries no record of which delay unit was used when the
#' vintages were created. By default this function infers `delays_unit` from
#' the realised delays (the gaps between each `pub_date` and its `time`),
#' which means weekly reference dates with daily delays are correctly
#' inferred as `"days"` and vice versa. Pass `delays_unit` explicitly to
#' override the inference or for monthly/yearly triangles.
#'
#' The reviser package must be installed to use this function.
#'
#' @seealso
#' - [as_reviser_vintages()] for converting to reviser vintages
#' - [as_reporting_triangle.data.frame()] for the underlying method
#'
#' @family reporting_triangle
#' @export
#' @method as_reporting_triangle tbl_pubdate
#'
#' @examplesIf requireNamespace("reviser", quietly = TRUE)
#' # Create a reporting triangle
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data = data_as_of_df)
#'
#' # Convert to reviser vintages
#' vintages <- as_reviser_vintages(rep_tri)
#'
#' # Convert back to reporting_triangle; `delays_unit` is inferred from the
#' # spacing of the `time` column.
#' rep_tri_2 <- as_reporting_triangle(data = vintages)
#' print(rep_tri_2)
as_reporting_triangle.tbl_pubdate <- function(data,
                                              delays_unit = NULL,
                                              ...) {
  if (!requireNamespace("reviser", quietly = TRUE)) { # nolint: missing_package_linter
    cli::cli_abort(
      c(
        "Package {.pkg reviser} is required to convert from reviser vintages
        format.",
        i = "Install it with: {.code install.packages(\"reviser\")}"
      )
    )
  }

  long_df <- reviser::vintages_long(data, keep_na = FALSE) # nolint: namespace_linter
  long_df <- as.data.frame(long_df)
  long_df <- long_df[order(long_df$time, long_df$pub_date), , drop = FALSE]
  long_df$time <- as.Date(long_df$time)
  long_df$pub_date <- as.Date(long_df$pub_date)

  if (is.null(delays_unit)) {
    delays_unit <- .infer_delays_unit(long_df$pub_date, long_df$time)
    # pub_date is reviser's name for what the rest of the package calls
    # report_date; they are the same thing here.
  }
  assert_delays_unit(delays_unit)

  long_df$count <- unlist(
    by(
      long_df$value,
      long_df$time,
      function(v) c(v[1], diff(v)),
      simplify = FALSE
    ),
    use.names = FALSE
  )

  return(as_reporting_triangle.data.frame(
    data = long_df,
    delays_unit = delays_unit,
    reference_date = "time",
    report_date = "pub_date",
    ...
  ))
}

#' Infer `delays_unit` from `report_date - reference_date` gaps
#'
#' Looks at the realised delays (the gaps between each report date and its
#' reference date, in days) and returns the smallest unit that divides every
#' gap evenly. This correctly handles cases where reference dates are weekly
#' but delays are daily (or vice versa).
#'
#' @param report_dates Date vector of report dates.
#' @param reference_dates Date vector of reference dates (same length as
#'   `report_dates`).
#' @returns A character string, one of `"days"` or `"weeks"`.
#' @keywords internal
.infer_delays_unit <- function(report_dates, reference_dates) {
  gaps <- as.integer(as.Date(report_dates) - as.Date(reference_dates))
  positive_gaps <- gaps[gaps > 0]
  if (length(positive_gaps) == 0L) {
    cli::cli_abort(
      c(
        "Cannot infer {.arg delays_unit}: no positive `report_date - reference_date` gaps.", # nolint
        "i" = "Pass {.arg delays_unit} explicitly." # nolint
      )
    )
  }
  min_gap <- min(positive_gaps)
  if (any(positive_gaps %% min_gap != 0L)) {
    cli::cli_abort(
      c(
        "Cannot infer {.arg delays_unit}: `report_date - reference_date` gaps are not multiples of the smallest gap.", # nolint
        "i" = "Smallest gap (days): {.val {min_gap}}.", # nolint
        "i" = "Pass {.arg delays_unit} explicitly." # nolint
      )
    )
  }
  unit <- switch(as.character(min_gap),
    "1" = "days",
    "7" = "weeks",
    NULL
  )
  if (is.null(unit)) {
    cli::cli_abort(
      c(
        "Cannot infer {.arg delays_unit} from a smallest gap of {min_gap} day{?s}.", # nolint
        "i" = "Pass {.arg delays_unit} explicitly (one of 'days', 'weeks', 'months', 'years')." # nolint
      )
    )
  }
  return(unit)
}
