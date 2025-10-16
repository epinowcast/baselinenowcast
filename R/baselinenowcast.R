#' @title Generate a nowcast from a reporting triangle
#'
#' @param data Reporting triangle to be nowcasted
#' @inheritParams sample_nowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @param output_type Character string indicating whether the output should be
#'   samples (`"samples"`)from the estimate with full uncertainty or whether to
#'   return the point estimate (`"point"`). Default is `"samples"`.
#' @param draws Integer indicating the number of probabilistic draws to include
#'    if `output_type` is `"samples"`. Default is 1000.
#' @param ... Additional arguments passed to methods.
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @family nowcast_df
#' @export
baselinenowcast <- function(data,
                            scale_factor = 3,
                            prop_delay = 0.5,
                            output_type = c("samples", "point"),
                            draws = 1000,
                            uncertainty_model = fit_by_horizon,
                            uncertainty_sampler = sample_nb,
                            ...) {
  UseMethod("baselinenowcast")
}

#' @title Create a dataframe of nowcast results from a single reporting triangle
#'
#' @description This function ingests a single
#'    \code{\link{reporting_triangle}} object and generates a nowcast in the
#'    form of a \code{\link{baselinenowcast_df}} object. This function will by
#'    default estimate uncertainty using past retrospective nowcast errors and
#'    generate probabilistic nowcasts, which are samples from the predictive
#'    distribution of the estimated final case count at each reference date.
#'    This method specifically computes a nowcast for a single reporting
#'    triangle. See documentation for the arguments of this function which
#'    can be used to set the model specifications (things like number of
#'    reference times for delay and uncertainty estimation, the observation
#'    model, etc.).
#'
#' @param data \code{\link{reporting_triangle}} class object to be nowcasted.
#' @param delay_pmf Vector of delays assumed to be indexed starting at the
#'   first delay column in `data$reporting_triangle_matrix`. Default is NULL,
#'   which will estimate the delay from the reporting triangle matrix in `data`,
#'   See \code{\link{estimate_delay}} for more details.
#' @param uncertainty_params Vector of uncertainty parameters ordered from
#'   horizon 1 to the maximum horizon. Default is `NULL`, which will
#'   result in computing the uncertainty parameters from the `data`.
#' @param ... Additional arguments passed to
#'    \code{\link{estimate_uncertainty}}
#'    and \code{\link{sample_nowcast}}.
#' @inheritParams baselinenowcast
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcast
#' @inheritParams allocate_reference_times
#' @family nowcast_df
#' @export
#' @method baselinenowcast reporting_triangle
#' @returns Data.frame of class \code{\link{baselinenowcast_df}}
#' @examples
#' data_as_of_df <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(
#'   data = data_as_of_df,
#'   max_delay = 25
#' )
#' nowcast_df <- baselinenowcast(rep_tri)
#' nowcast_df
baselinenowcast.reporting_triangle <- function(
    data,
    scale_factor = 3,
    prop_delay = 0.5,
    output_type = "samples",
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    delay_pmf = NULL,
    uncertainty_params = NULL,
    ...) {
  tri <- data$reporting_triangle_matrix
  assert_choice(output_type, choices = c("samples", "point"))
  assert_integerish(draws, null.ok = TRUE)

  tv <- allocate_reference_times(tri,
    scale_factor = scale_factor,
    prop_delay = prop_delay
  )

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = data$reporting_triangle_matrix,
      n = tv$n_history_delay
    )
  }
  # check for delay pmf being the right length/format
  .validate_delay(tri, delay_pmf)

  pt_nowcast <- apply_delay(tri, delay_pmf)

  if (output_type == "point") {
    nowcast_df <- data.frame(
      time = seq_len(nrow(pt_nowcast)),
      pred_count = rowSums(pt_nowcast)
    )
    if (!is.null(uncertainty_params)) {
      cli_warn(
        message =
          "`uncertainty_params` passed in but point estimate was specified as an output type. `uncertainty params` will not be used." # nolint
      )
    }
    # early return
    result_df <- new_baselinenowcast_df(nowcast_df,
      reference_dates = data$reference_dates
    )
    return(result_df)
  }

  if (is.null(uncertainty_params)) {
    # estimate uncertainty or sample from passed in uncertainty
    nowcast_df <- estimate_and_apply_uncertainty(
      point_nowcast_matrix = pt_nowcast,
      reporting_triangle = tri,
      n_history_delay = tv$n_history_delay,
      n_retrospective_nowcasts = tv$n_retrospective_nowcasts,
      max_delay = ncol(tri) - 1,
      draws = draws,
      uncertainty_model = uncertainty_model,
      uncertainty_sampler = uncertainty_sampler,
      ...
    )
  } else { # uncertainty parameters passed in and not a point estimate
    # check for uncertainty params being the right length/format
    .validate_uncertainty(tri, uncertainty_params)
    nowcast_df <- sample_nowcasts(
      point_nowcast_matrix = pt_nowcast,
      reporting_triangle = tri,
      uncertainty_params = uncertainty_params,
      draws = draws,
      uncertainty_sampler = uncertainty_sampler,
      ...
    )
  }

  result_df <- new_baselinenowcast_df(nowcast_df,
    reference_dates = data$reference_dates
  )

  return(result_df)
}
