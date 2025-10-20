#' Nowcast Data.frame Object
#' @name baselinenowcast_df-class
#' @aliases baselinenowcast_df
#' @family baselinenowcast_df
#'
#' @description
#' A `baselinenowcast_df` object which contains point or probabilistic nowcasts
#'   alongside reference dates and any additional metadata, in tidy data format.
#'   Nowcasts are presented aggregated across delays, by reference date.
#'
#' @section Structure:
#' A `baselinenowcast_df` is a data.frame with the following columns:
#' \describe{
#'  \item{reference_date}{Dates corresponding to the reference times of the
#'  nowcast.}
#'  \item{pred_count}{Numeric indicating the estimated total counts aggregated
#'    across delays at each reference date.}
#'  \item{draw}{Integer indexing the sample from the probabilistic nowcast
#'    distribution. If `output_type = "point"`, this will be set to 1.}
#'  \item{output_type}{Character string indicating whether the `pred_count`
#'   represents a probabilistic draw from the observation model indicated by
#'   `"samples"` or whether the `pred_count` is a point estimate indicated by
#'   `"point"`.}
#' }
#' See the corresponding \code{\link{reporting_triangle}} and
#' \code{\link{baselinenowcast}} function
#' for more details on the required inputs to generate the object.
NULL

#' Combine data from a nowcast dataframe, strata, and reference dates
#' @description Combines data from a nowcast dataframe, a named list of the
#'    strata associated with the nowcast dataframe, and a vector of reference
#'    dates corresponding to the time column in the `baselinenowcast_df`
#'
#' @param baselinenowcast_df Data.frame containing information for multiple
#'  draws with columns for the reference time (`time`), the predicted counts
#'  (`pred_count`), and the draw number (`draw`).
#' @param reference_dates Vector of reference dates corresponding to the
#'    reference times in the `baselinenowcast_df`.
#' @inheritParams baselinenowcast
#'
#' @returns An object of class \code{\link{baselinenowcast_df}}
#' @export
new_baselinenowcast_df <- function(baselinenowcast_df,
                                   reference_dates,
                                   output_type) {
  assert_choice(output_type, choices = c("samples", "point"))
  spine_df <- data.frame(
    reference_date = reference_dates,
    time = seq_along(reference_dates)
  )

  baselinenowcast_df_dates <- merge(baselinenowcast_df,
    spine_df,
    by = "time",
    all.x = TRUE
  )

  baselinenowcast_df_dates$time <- NULL
  baselinenowcast_df_dates$output_type <- output_type
  baselinenowcast_df_ordered <- baselinenowcast_df_dates[order(
    baselinenowcast_df_dates$reference_date,
    baselinenowcast_df_dates$draw
  ), ]

  result <- structure(
    data.frame(baselinenowcast_df_ordered),
    class = c("baselinenowcast_df", class(baselinenowcast_df_ordered))
  )

  return(result)
}

#' Assert validity of `baselinenowcast_df` objects
#'
#' @param data A \code{\link{baselinenowcast_df}} object to check for validity.
#' @return NULL
#' @export
assert_baselinenowcast_df <- function(data) {
  assert_data_frame(data)

  required_cols <- c("reference_date", "pred_count")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    cli_abort(
      message = c(
        "Required columns missing from data",
        "x" = "Missing: {.val {missing_cols}}" # nolint
      )
    )
  }

  assert_date(data$reference_date)
  # Check for duplicated reference dates
  cols_to_check <- names(data)[names(data) %in% c("reference_date", "draw")]

  dups <- duplicated(data[, c(cols_to_check)])
  if (any(dups)) {
    cli_abort(
      message = c(
        "Data contains multiple `reference_date`s", # nolint
        "x" = "Found {sum(dups)} duplicate `reference_date`{?s}", # nolint
        "i" = "`baselinenowcast_df` objects should only contain a single estimate for each reference date." # nolint
      )
    )
  }

  return(NULL)
}
