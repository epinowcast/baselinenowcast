#' Nowcast Dta.frame Object
#' @name nowcast_df-class
#' @aliases nowcast_df
#' @family nowcast_df
#'
#' @description
#' A `nowcast_df` object which contains point or probabilistic nowcasts
#'   alongside reference dates and any additional metadata, in tidy data format.
#'   Nowcasts are presented aggregated across delays, by reference date.
#'
#' @section Structure:
#' A `nowcast_df` is a data.frame with the following columns:
#' \describe{
#'  \item{reporting_triangle_matrix}{Matrix with which rows are reference
#'    times and columns are delays and
#'    entries are incident cases at each reference time and delay.}
#'  \item{reference_date}{Dates corresponding to the reference times of the
#'  nowcast.}
#'  \item{time}{Integer indicating the time index}
#'  \item{pred_count}{Numeric indicating the estimated total counts aggregated
#'    across delays at each reference date.}
#'  \item{draw}{Integer indexing the sample from the probabilistic nowcast
#'    distribution. Only present if `output_type = "samples"`.}
#'  \item{...}{Additional columns from `strata_map`}
#' }
#' See the corresponding \code{\link{reporting_triangle}} and
#' \code{\link{baselinenowcast}} function
#' for more details on the required inputs to generate the object.
NULL
