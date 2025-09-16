#' Estimate and apply uncertainty to a point nowcast matrix
#'
#' @inheritParams estimate_delay
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_prediction
#' @inheritParams sample_nowcasts
#' @param n_history_delay Integer indicating the number of reference times
#'   (observations) to be used in the estimate of the reporting delay, always
#'    starting from the most recent reporting delay. Default is NULL, which will
#'    be set internally based on the defaults in `allocate_reference_times()`
#' @param n_retrospective_nowcasts Integer indicating the number of
#'   retrospective nowcast times to use for uncertainty estimation. Default is
#'   NULL, which will be set internally based on the defaults in
#'   `allocate_reference_times()`
#'
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @export
#' @importFrom cli cli_abort
#' @autoglobal
#' @examples
#' triangle <- matrix(
#'   c(
#'     40, 10, 20, 5,
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 6,
#'   byrow = TRUE
#' )
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   n = 4
#' )
#' # Need to tell uncertainty estimator to also use 4 reference times for
#' # delay estimation, the remaining 2 will then be used for
#' # uncertainty estimation.
#' nowcast_draws_df <- estimate_and_apply_uncertainty(
#'   pt_nowcast_matrix,
#'   triangle,
#'   n_history_delay = 4
#' )
#' nowcast_draws_df
estimate_and_apply_uncertainty <- function(
    point_nowcast_matrix,
    reporting_triangle,
    max_delay = ncol(reporting_triangle) - 1,
    n_history_delay = NULL,
    n_retrospective_nowcasts = NULL,
    draws = 1000,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE)) {
  .validate_multiple_inputs(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    max_delay = max_delay
  )

  # Logic to decide on how to allocate the training volume
  n_ref_times <- nrow(reporting_triangle)
  min_ref_times_delay <- sum(is.na(rowSums(reporting_triangle))) + 1
  if (is.null(n_history_delay) && is.null(n_retrospective_nowcasts)) {
    tv <- allocate_reference_times(
      reporting_triangle
    )
    n_history_delay <- tv$n_history_delay
    n_retrospective_nowcasts <- tv$n_retrospective_nowcasts
  }

  if (is.null(n_history_delay & !is.null(n_retrospective_nowcasts))) {
    n_history_delay <- min(3 * max_delay, n_ref_times) - n_retrospective_nowcasts # nolint
    message("`n_history_delay` was not specified, {n_history_delay} reference times will be used for delay estimation.") # nolint
  }
  if (!is.null(n_history_delay & is.null(n_retrospective_nowcasts))) {
    n_retrospective_nowcasts <- min(3 * max_delay, n_ref_times) - n_history_delay # nolint
    message("`n_retrospectove_nowcasts` was not specified, {n_retrospective} reference times will be used as retrospective nowcast times for uncertainty estimation.") # nolint
  }

  # Validate that the inputs or defaults or combinations are valid.
  .validate_ref_time_allocation(
    size_min_ref_times_delay = min_ref_times_delay,
    n_ref_times = n_ref_times,
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts
  ) # nolint

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
    uncertainty_sampler = uncertainty_sampler,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )
  return(nowcast_draws)
}
