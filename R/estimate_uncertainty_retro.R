#' Estimate uncertainty parameters using retrospective nowcasts
#'
#' @description
#' Estimates uncertainty parameters for nowcasting by creating a series of
#'   retrospective datasets from the input reporting triangle, generating point
#'   nowcasts for those datasets, and calibrating uncertainty parameters based
#'   on retrospective nowcast performance.
#'
#' This function chains the retrospective nowcasting workflow:
#' \enumerate{
#'   \item [truncate_triangles()] - Create retrospective snapshots
#'   \item [apply_reporting_structures()] - Generate retrospective reporting
#'     triangles
#'   \item [fill_triangles()] - Generate point nowcasts
#'   \item [estimate_uncertainty()] - Estimate uncertainty parameters
#' }
#'
#' For full probabilistic nowcasts (uncertainty estimation + sampling), use
#'   [estimate_and_apply_uncertainty()].
#'
#' For more control over individual steps (e.g., custom matrix preparation,
#'   alternative aggregation), use the low-level functions directly.
#'
#' @inheritParams estimate_delay
#' @inheritParams apply_reporting_structures
#' @inheritParams estimate_and_apply_uncertainty
#' @inheritParams fill_triangles
#' @param ... Additional arguments passed to [estimate_uncertainty()].
#'
#' @returns A numeric vector of uncertainty parameters with length equal to
#'   one less than the number of columns in the reporting triangle, with each
#'   element representing the estimate of the uncertainty parameter for each
#'   horizon. Returns NULL if insufficient data is available for estimation.
#'
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_warn
#'
#' @examples
#' # Create a reporting triangle from syn_nssp_df
#' data_as_of <- syn_nssp_df[syn_nssp_df$report_date <= "2026-04-01", ]
#' rep_tri <- as_reporting_triangle(data_as_of) |>
#'   truncate_to_delay(max_delay = 25)
#'
#' uncertainty_params <- estimate_uncertainty_retro(
#'   rep_tri,
#'   n_history_delay = 30,
#'   n_retrospective_nowcasts = 10
#' )
#' uncertainty_params
estimate_uncertainty_retro <- function(
    reporting_triangle,
    n_history_delay,
    n_retrospective_nowcasts,
    structure = get_reporting_structure(reporting_triangle),
    delay_pmf = NULL,
    preprocess = preprocess_negative_values,
    validate = TRUE,
    ...) {
  assert_reporting_triangle(reporting_triangle, validate)

  n_ref_times <- nrow(reporting_triangle)
  min_ref_times_delay <- sum(is.na(rowSums(reporting_triangle))) + 1
  .validate_inputs_uncertainty(
    n_min_delay = min_ref_times_delay,
    n_ref_times = n_ref_times,
    n_history_delay = n_history_delay,
    n_retrospective_nowcasts = n_retrospective_nowcasts
  )

  trunc_rep_tri_list <- truncate_triangles(
    reporting_triangle = reporting_triangle,
    n = n_retrospective_nowcasts,
    validate = FALSE
  )

  reporting_triangle_list <- apply_reporting_structures(
    truncated_reporting_triangles = trunc_rep_tri_list,
    structure = structure,
    validate = FALSE
  )

  pt_nowcast_mat_list <- fill_triangles(
    retro_reporting_triangles = reporting_triangle_list,
    n = n_history_delay,
    delay_pmf = delay_pmf,
    preprocess = preprocess,
    validate = FALSE
  )

  if (is.null(pt_nowcast_mat_list) ||
    all(vapply(pt_nowcast_mat_list, is.null, logical(1)))) {
    cli_warn(
      message = c(
        "Insufficient data to generate point nowcasts",
        i = "Returning NULL for uncertainty parameter"
      )
    )
    return(NULL)
  }

  uncertainty_params <- estimate_uncertainty(
    point_nowcast_matrices = pt_nowcast_mat_list,
    truncated_reporting_triangles = trunc_rep_tri_list,
    retro_reporting_triangles = reporting_triangle_list,
    n = n_retrospective_nowcasts,
    validate = FALSE,
    ...
  )

  return(uncertainty_params)
}
