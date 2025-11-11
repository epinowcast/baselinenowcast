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
#' @inheritParams construct_triangle
#' @inheritParams sample_nowcasts
#' @inheritParams sample_prediction
#' @inheritParams fill_triangles
#' @param n_history_delay Integer indicating the number of reference times
#'   (observations) to be used in the estimate of the reporting delay, always
#'    starting from the most recent reporting delay.
#' @param n_retrospective_nowcasts Integer indicating the number of
#'   retrospective nowcast times to use for uncertainty estimation.
#' @param uncertainty An object of class `uncertainty_opts` created by
#'   [uncertainty_opts()]. Specifies the uncertainty model and aggregation
#'   functions. Default uses negative binomial with by-horizon fitting.
#' @param uncertainty_model (Deprecated) Use `uncertainty` parameter instead.
#' @param uncertainty_sampler (Deprecated) Use `uncertainty` parameter instead.
#' @param ... Additional arguments to `estimate_uncertainty()` and
#'    `sample_prediction()`.
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_abort cli_warn
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
    structure = detect_structure(reporting_triangle),
    draws = 1000,
    delay_pmf = NULL,
    uncertainty = uncertainty_opts(),
    uncertainty_model = NULL,
    uncertainty_sampler = NULL,
    ...) {
  # Handle deprecated parameters
  if (!is.null(uncertainty_model) || !is.null(uncertainty_sampler)) {
    cli_warn(
      c(
        "!" = "Direct parameter specification is deprecated.",
        i = "Use {.arg uncertainty = uncertainty_opts()} instead.",
        "See {.help uncertainty_opts} for details."
      ),
      .frequency = "once",
      .frequency_id = "estimate_and_apply_uncertainty_deprecated_params"
    )

    # Only handle fit_by_horizon and sample_nb
    if (!is.null(uncertainty_model) &&
        !identical(uncertainty_model, fit_by_horizon)) {
      cli_abort(c(
        "Cannot automatically convert custom {.arg uncertainty_model}",
        i = "Please use {.fn uncertainty_opts} directly"
      ))
    }
    if (!is.null(uncertainty_sampler) &&
        !identical(uncertainty_sampler, sample_nb)) {
      cli_abort(c(
        "Cannot automatically convert custom {.arg uncertainty_sampler}",
        i = "Please use {.fn uncertainty_opts} directly"
      ))
    }

    # Use default model if parameters specified but match defaults
    uncertainty <- uncertainty_opts()
  }

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
    uncertainty = uncertainty,
    ...
  )
  nowcast_draws <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    uncertainty_params = uncertainty_params,
    draws = draws,
    uncertainty = uncertainty,
    ...
  )
  return(nowcast_draws)
}
