#' Estimate and apply uncertainty to a point nowcast matrix
#'
#' @description
#' Generates probabilistic nowcasts by estimating uncertainty parameters from
#'   retrospective nowcasts and applying them to a point nowcast matrix.
#'
#' This function combines:
#' \enumerate{
#'   \item [estimate_uncertainty_retro()] - Estimates uncertainty parameters
#'     using retrospective nowcasts
#'   \item [sample_nowcasts()] - Applies uncertainty to generate draws
#' }
#'
#' To obtain estimates of uncertainty parameters, use
#'   [estimate_uncertainty_retro()]. For full control over individual steps
#'   (e.g., custom matrix preparation, alternative aggregation), use the
#'   low-level functions ([truncate_triangles()], [construct_triangles()],
#'   [fill_triangles()], [estimate_uncertainty()]) directly.
#'
#' @inheritParams estimate_delay
#' @inheritParams construct_triangles
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcasts
#' @inheritParams sample_prediction
#' @inheritParams fill_triangles
#' @param n_history_delay Integer indicating the number of reference times
#'   (observations) to be used in the estimate of the reporting delay, always
#'    starting from the most recent reporting delay.
#' @param n_retrospective_nowcasts Integer indicating the number of
#'   retrospective nowcasts to use for uncertainty estimation.
#' @param ... Additional arguments to [estimate_uncertainty_retro()] and
#'   [sample_nowcasts()].
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @export
#' @importFrom cli cli_abort
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
#'   n_history_delay = 4,
#'   n_retrospective_nowcasts = 2
#' )
#' nowcast_draws_df
estimate_and_apply_uncertainty <- function(
    point_nowcast_matrix,
    reporting_triangle,
    n_history_delay,
    n_retrospective_nowcasts,
    max_delay = ncol(reporting_triangle) - 1,
    draws = 1000,
    structure = 1,
    delay_pmf = NULL,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    ...) {
  .validate_multiple_inputs(
    point_nowcast_matrix = point_nowcast_matrix,
    reporting_triangle = reporting_triangle,
    max_delay = max_delay
  )

  uncertainty_params <- estimate_uncertainty_retro(
    reporting_triangle = reporting_triangle,
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts,
    max_delay = max_delay,
    structure = structure,
    delay_pmf = delay_pmf,
    uncertainty_model = uncertainty_model,
    ...
  )
  nowcast_draws <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params = uncertainty_params,
    draws = draws,
    uncertainty_sampler = uncertainty_sampler,
    ...
  )
  return(nowcast_draws)
}
