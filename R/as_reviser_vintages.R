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
#' @inheritParams as_reporting_triangle
#' @param ... Additional arguments passed to
#'   [as_reporting_triangle.data.frame()].
#'
#' @return A [reporting_triangle] object.
#'   See [reporting_triangle-class] for details on the structure.
#'
#' @details
#' Reviser vintages store cumulative reported values at each publication date.
#' The conversion takes differences between successive publication dates per
#' time point to recover incremental counts, and uses the delay implied by
#' `pub_date - time` (in `delays_unit`) to build the reporting triangle.
#'
#' A `tbl_pubdate` carries no record of which delay unit was used when the
#' vintages were created.
#' If you produced the vintages from a non-daily [reporting_triangle] via
#' [as_reviser_vintages()] (for example a weekly triangle), pass the
#' matching `delays_unit` here to recover an equivalent triangle.
#' The default (`"days"`) will silently produce a daily triangle with
#' anomalous date spacings if the original triangle was not daily.
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
#' # Convert back to reporting_triangle (round-trip).
#' # Pass `delays_unit` explicitly to match the original triangle,
#' # since the vintages object does not record this.
#' rep_tri_2 <- as_reporting_triangle(
#'   data = vintages,
#'   delays_unit = attr(rep_tri, "delays_unit")
#' )
#' print(rep_tri_2)
as_reporting_triangle.tbl_pubdate <- function(data,
                                              delays_unit = "days",
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

  increments <- unlist(
    by(
      long_df,
      long_df$time,
      function(g) c(g$value[1], diff(g$value)),
      simplify = FALSE
    ),
    use.names = FALSE
  )

  out_df <- data.frame(
    reference_date = long_df$time,
    report_date = long_df$pub_date,
    count = increments
  )

  return(as_reporting_triangle.data.frame(
    data = out_df,
    delays_unit = delays_unit,
    ...
  ))
}
