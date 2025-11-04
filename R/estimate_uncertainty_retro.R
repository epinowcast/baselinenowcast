#' Estimate uncertainty parameters using retrospective validation
#'
#' @description
#' Estimates uncertainty parameters for nowcasting by performing retrospective
#'   validation: creating historical snapshots of the reporting triangle,
#'   generating point nowcasts for those snapshots, and calibrating uncertainty
#'   parameters based on retrospective nowcast performance.
#'
#' This function chains the retrospective validation workflow:
#' \enumerate{
#'   \item [truncate_triangles()] - Create retrospective snapshots
#'   \item [construct_triangles()] - Generate retrospective reporting triangles
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
#' @inheritParams construct_triangles
#' @inheritParams estimate_uncertainty
#' @inheritParams estimate_and_apply_uncertainty
#' @param delay_pmf Numeric vector representing a discrete probability mass
#'   function for the delay distribution. If NULL (default), the delay
#'   distribution is estimated from the data.
#'
#' @returns A numeric vector of uncertainty parameters with length equal to
#'   one less than the number of columns in the reporting triangle, with each
#'   element representing the estimate of the uncertainty parameter for each
#'   horizon. Returns NULL if insufficient data is available for estimation.
#'
#' @export
#' @importFrom cli cli_warn
#'
#' @examples
#' # Create example reporting triangle
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )
#'
#' # Estimate uncertainty parameters
#' uncertainty_params <- estimate_uncertainty_retro(
#'   triangle,
#'   n_history_delay = 5,
#'   n_retrospective_nowcasts = 2
#' )
#' uncertainty_params
#'
#' # Estimate with custom parameters
#' uncertainty_params_custom <- estimate_uncertainty_retro(
#'   triangle,
#'   n_history_delay = 4,
#'   n_retrospective_nowcasts = 2,
#'   max_delay = 3
#' )
#' uncertainty_params_custom
estimate_uncertainty_retro <- function(
    reporting_triangle,
    n_history_delay,
    n_retrospective_nowcasts,
    max_delay = ncol(reporting_triangle) - 1,
    delay_pmf = NULL,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
    structure = 1,
    uncertainty_model = fit_by_horizon) {
  # Validate input triangle
  .validate_triangle(reporting_triangle)

  # Validate inputs are sufficient for uncertainty estimation
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
    n = n_retrospective_nowcasts
  )

  reporting_triangle_list <- construct_triangles(
    truncated_reporting_triangles = trunc_rep_tri_list,
    structure = structure
  )

  pt_nowcast_mat_list <- fill_triangles(
    retro_reporting_triangles = reporting_triangle_list,
    max_delay = max_delay,
    n = n_history_delay,
    delay_pmf = delay_pmf
  )

  if (is.null(pt_nowcast_mat_list) ||
    all(sapply(pt_nowcast_mat_list, is.null))) {
    cli_warn(
      message = c(
        "Insufficient data to generate point nowcasts",
        "i" = "Returning NULL for uncertainty parameter"
      )
    )
    return(NULL)
  }

  uncertainty_params <- estimate_uncertainty(
    point_nowcast_matrices = pt_nowcast_mat_list,
    truncated_reporting_triangles = trunc_rep_tri_list,
    retro_reporting_triangles = reporting_triangle_list,
    n = n_retrospective_nowcasts,
    uncertainty_model = uncertainty_model,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )

  return(uncertainty_params)
}
