#' Estimate uncertainty parameters for nowcasting
#'
#' @description
#' Estimates uncertainty parameters for uncertainty quantification in nowcasts
#'   by generating retrospective snapshots, creating reporting triangles,
#'   producing point nowcasts, and estimating uncertainty from the nowcast
#'   performance.
#'
#' This function wraps the workflow of:
#' \enumerate{
#'   \item [truncate_triangles()] - Create retrospective snapshots
#'   \item [construct_triangles()] - Generate retrospective reporting triangles
#'   \item [fill_triangles()] - Generate point nowcasts
#'   \item [estimate_uncertainty()] - Estimate uncertainty parameters
#' }
#'
#' @param reporting_triangle A reporting triangle matrix with reference dates
#'   as rows and delays as columns. Should be the output of
#'   [construct_triangle()] or a compatible matrix structure with NAs in the
#'   bottom right triangle.
#' @param n_delay Integer. Number of past observations (rows) to use for
#'   estimating the delay distribution in each retrospective snapshot.
#'   Default is to use all available rows in the smallest retrospective
#'   triangle.
#' @param n_retro Integer. Number of retrospective snapshots to generate for
#'   uncertainty estimation. Required parameter with no default.
#' @param max_delay Integer. Maximum delay to consider in days. Default is
#'   `ncol(reporting_triangle) - 1`.
#' @param delay_pmf Numeric vector or NULL. Optional custom delay probability
#'   mass function. If NULL (default), the delay distribution is estimated
#'   from the data.
#' @param ref_time_aggregator Function that operates along the rows (reference
#'   times) of the retrospective point nowcast matrix before it has been
#'   aggregated across columns (delays). Default is `identity` which does not
#'   aggregate across reference times.
#' @param delay_aggregator Function that operates along the columns (delays)
#'   of the retrospective point nowcast matrix after it has been aggregated
#'   across reference times. Default is `function(x) rowSums(x, na.rm = TRUE)`.
#' @param structure Integer or vector specifying the reporting structure for
#'   retrospective triangles. If integer, divides columns evenly. If vector,
#'   must sum to number of columns minus 1. Default is 1 (standard triangular
#'   structure).
#' @param uncertainty_model Function that ingests a matrix of observations and
#'   a matrix of predictions and returns a vector that can be used to apply
#'   uncertainty using the same error model. Default is [fit_by_horizon].
#'
#' @returns A numeric vector of uncertainty parameters with length equal to
#'   one less than the number of columns in the reporting triangle, with each
#'   element representing the estimate of the uncertainty parameter for each
#'   horizon. Returns NULL if insufficient data is available for estimation.
#'
#' @export
#' @importFrom checkmate assert_integerish assert_function
#' @importFrom cli cli_warn cli_abort
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
#' uncertainty_params <- estimate_uncertainty_parameters(
#'   triangle,
#'   n_retro = 2
#' )
#'
#' # Estimate with custom parameters
#' uncertainty_params <- estimate_uncertainty_parameters(
#'   triangle,
#'   n_delay = 5,
#'   n_retro = 3,
#'   max_delay = 3
#' )
estimate_uncertainty_parameters <- function(
    reporting_triangle,
    n_delay = NULL,
    n_retro,
    max_delay = ncol(reporting_triangle) - 1,
    delay_pmf = NULL,
    ref_time_aggregator = identity,
    delay_aggregator = function(x) rowSums(x, na.rm = TRUE),
    structure = 1,
    uncertainty_model = fit_by_horizon) {
  # Validate input triangle
  .validate_triangle(reporting_triangle)

  # Set default n_delay if not provided (will be calculated after truncation)
  if (is.null(n_delay)) {
    use_default_n_delay <- TRUE
  } else {
    use_default_n_delay <- FALSE
    assert_integerish(n_delay, lower = 1, len = 1, any.missing = FALSE)
  }

  # Validate integer parameters
  assert_integerish(n_retro, lower = 1, len = 1, any.missing = FALSE)
  assert_integerish(max_delay, lower = 0, len = 1, any.missing = FALSE)
  if (length(structure) == 1) {
    assert_integerish(structure, lower = 1, len = 1, any.missing = FALSE)
  } else {
    assert_integerish(structure, lower = 1, any.missing = FALSE)
  }

  # Validate delay_pmf if provided
  if (!is.null(delay_pmf)) {
    checkmate::assert_numeric(
      delay_pmf,
      lower = 0,
      upper = 1,
      any.missing = FALSE,
      min.len = 1
    )
    if (!isTRUE(all.equal(sum(delay_pmf), 1))) {
      cli_abort(
        message = c(
          "delay_pmf must sum to 1",
          "x" = "Sum is {sum(delay_pmf)}"
        )
      )
    }
  }

  # Validate aggregation functions
  assert_function(ref_time_aggregator)
  assert_function(delay_aggregator)
  assert_function(uncertainty_model)

  # Step 1: Create retrospective snapshots by truncating the triangle
  trunc_rep_tri_list <- truncate_triangles(
    reporting_triangle = reporting_triangle,
    n = n_retro
  )

  # Step 2: Generate retrospective reporting triangles with structure
  reporting_triangle_list <- construct_triangles(
    truncated_reporting_triangles = trunc_rep_tri_list,
    structure = structure
  )

  # Calculate default n_delay if needed (minimum rows across retro triangles)
  if (use_default_n_delay) {
    n_delay <- min(sapply(reporting_triangle_list, nrow))
  }

  # Step 3: Generate point nowcasts
  pt_nowcast_mat_list <- fill_triangles(
    retro_reporting_triangles = reporting_triangle_list,
    max_delay = max_delay,
    n = n_delay,
    delay_pmf = delay_pmf
  )

  # Handle NULL return or all NULL elements from fill_triangles
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

  # Step 4: Estimate uncertainty parameters
  uncertainty_params <- estimate_uncertainty(
    point_nowcast_matrices = pt_nowcast_mat_list,
    truncated_reporting_triangles = trunc_rep_tri_list,
    retro_reporting_triangles = reporting_triangle_list,
    n = n_retro,
    uncertainty_model = uncertainty_model,
    ref_time_aggregator = ref_time_aggregator,
    delay_aggregator = delay_aggregator
  )

  return(uncertainty_params)
}
