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
#' See the corresponding [reporting_triangle] and
#' [baselinenowcast()] function
#' for more details on the required inputs to generate the object.
#'
#' @return A `baselinenowcast_df` object. This is a data.frame subclass
#'   containing nowcast results. See the Structure section for details on
#'   the required columns.
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
#' @family baselinenowcast_df
#' @export
new_baselinenowcast_df <- function(baselinenowcast_df,
                                   reference_dates,
                                   output_type) {
  assert_choice(output_type, choices = c("samples", "point"))

  baselinenowcast_df$output_type <- output_type
  baselinenowcast_df_ordered <- baselinenowcast_df[order(
    baselinenowcast_df$reference_date,
    baselinenowcast_df$draw
  ), ]

  result <- structure(
    data.frame(baselinenowcast_df_ordered),
    class = c("baselinenowcast_df", class(baselinenowcast_df_ordered))
  )

  return(result)
}
