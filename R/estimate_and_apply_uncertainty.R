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
#'   low-level functions ([truncate_to_rows()],
#'    [apply_reporting_structure()],
#'   [fill_triangles()], [estimate_uncertainty()]) directly.
#'
#' @inheritParams estimate_delay
#' @inheritParams apply_reporting_structure
#' @inheritParams estimate_uncertainty
#' @inheritParams sample_nowcasts
#' @inheritParams sample_prediction
#' @inheritParams fill_triangles
#' @param n_history_delay Integer indicating the number of reference times
#'   (observations) to be used in the estimate of the reporting delay, always
#'    starting from the most recent reporting delay.
#' @param n_retrospective_nowcasts Integer indicating the number of
#'   retrospective nowcast times to use for uncertainty estimation.
#' @param ... Additional arguments to `estimate_uncertainty()` and
#'    `sample_prediction()`.
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_abort
#' @examples
#' # Use package data truncated to appropriate size
#' data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' triangle <- as_reporting_triangle(data_as_of) |>
#'   truncate_to_delay(max_delay = 25)
#'
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   n = 75
#' )
#' # Use 75 reference times for delay estimation and 40 for uncertainty
#' nowcast_draws_df <- estimate_and_apply_uncertainty(
#'   pt_nowcast_matrix,
#'   triangle,
#'   n_history_delay = 75,
#'   n_retrospective_nowcasts = 40,
#'   draws = 100
#' )
#' head(nowcast_draws_df)
estimate_and_apply_uncertainty <- function(
    point_nowcast_matrix,
    reporting_triangle,
    n_history_delay,
    n_retrospective_nowcasts,
    structure = get_reporting_structure(reporting_triangle),
    draws = 1000,
    delay_pmf = NULL,
    uncertainty_model = fit_by_horizon,
    uncertainty_sampler = sample_nb,
    validate = TRUE,
    ...) {
  assert_reporting_triangle(point_nowcast_matrix, validate)
  assert_reporting_triangle(reporting_triangle, validate)

  # Check that both inputs have the same max_delay
  max_delay_point <- get_max_delay(point_nowcast_matrix)
  max_delay_rt <- get_max_delay(reporting_triangle)
  if (max_delay_point != max_delay_rt) {
    cli_abort(c(
      "x" = "`point_nowcast_matrix` and `reporting_triangle` must have the same max_delay.", # nolint
      "i" = "Got max_delay of {max_delay_point} and {max_delay_rt} respectively." # nolint
    ))
  }

  uncertainty_params <- estimate_uncertainty_retro(
    reporting_triangle = reporting_triangle,
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts,
    structure = structure,
    delay_pmf = delay_pmf,
    uncertainty_model = uncertainty_model,
    validate = FALSE,
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
