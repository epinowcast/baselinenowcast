#' Estimate and apply uncertainty to a point nowcast matrix
#'
#' @inheritParams estimate_delay
#' @inheritParams sample_nowcasts
#' @param n_retrospective_nowcasts Integer indicating the number of reference
#'    times to use as retrospective nowcast time. Default is 1.5* the maximum
#'    delay
#' @param error_model Character string indicating what error model to use.
#'    Default is `"negative binomial"`
#' @param aggregator Function to aggregate predictions for error estimation.
#'    Default is `sum`.
#' @param ... Additional arguments
#'
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   max_delay = 3,
#'   n = 4
#' )
#' nowcast_draws_df <- estimate_and_apply_uncertainty(
#'   pt_nowcast_matrix,
#'   triangle,
#'   max_delay,
#'   n_retrospective_nowcasts = 1
#' )
#' nowcast_draws_df
estimate_and_apply_uncertainty <- function(
    point_nowcast_matrix,
    reporting_triangle,
    max_delay,
    n_history_delay = round(1.5 * max_delay),
    n_retrospective_nowcasts = 3 * max_delay - n_history_delay,
    draws = 100,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
    ...) {
  # Estimate uncertainty from the reporting triangle passed in.
  trunc_rep_tris <- truncate_triangles(reporting_triangle,
    n = n_retrospective_nowcasts
  )

  retro_rep_tris <- construct_triangles(trunc_rep_tris)

  retro_pt_nowcasts <- fill_triangles(retro_rep_tris,
    max_delay = max_delay,
    n = n_history_delay
  )

  uncertainty_params <- estimate_uncertainty(
    point_nowcast_matrices = retro_pt_nowcasts,
    truncated_reporting_triangles = trunc_rep_tris,
    retro_reporting_triangles = retro_rep_tris,
    n = n_retrospective_nowcasts,
    uncertainty_model = uncertainty_model,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )
  nowcast_draws <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params = uncertainty_params,
    draws = draws,
    uncertainty_sample = uncertainty_sampler,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )
  return(nowcast_draws)
}
