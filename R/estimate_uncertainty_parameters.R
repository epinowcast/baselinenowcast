#' Estimate uncertainty parameters for nowcasting
#'
#' @description
#' Estimates dispersion parameters for uncertainty quantification in nowcasts
#'   by generating retrospective snapshots, creating reporting triangles,
#'   producing point nowcasts, and estimating dispersion from the nowcast
#'   performance.
#'
#' This function wraps the workflow of:
#' \enumerate{
#'   \item [truncate_triangles()] - Create retrospective snapshots
#'   \item [generate_triangles()] - Generate retrospective reporting triangles
#'   \item [generate_pt_nowcast_mat_list()] - Generate point nowcasts
#'   \item [estimate_dispersion()] - Estimate dispersion parameters
#' }
#'
#' @param reporting_triangle A reporting triangle matrix with reference dates
#'   as rows and delays as columns. Should be the output of
#'   [generate_triangle()] or a compatible matrix structure with NAs in the
#'   bottom right triangle.
#' @param n_delay Integer. Number of past observations (rows) to use for
#'   estimating the delay distribution in each retrospective snapshot.
#'   Default is to use all available rows in the smallest retrospective
#'   triangle.
#' @param n_retro Integer. Number of retrospective snapshots to generate for
#'   dispersion estimation. Default is 2.
#' @param max_delay Integer. Maximum delay to consider in days. Default is
#'   `ncol(reporting_triangle) - 1`.
#' @param delay_pmf Numeric vector or NULL. Optional custom delay probability
#'   mass function. If NULL (default), the delay distribution is estimated
#'   from the data.
#' @param fun_to_aggregate Function. Function to aggregate counts when
#'   estimating dispersion. Default is `sum`. Currently only `sum` is
#'   supported.
#' @param k Integer. Number of reference times to apply the
#'   `fun_to_aggregate` over to create target used to compute the nowcast
#'   errors. Default is 1.
#' @param structure Integer or vector specifying the reporting structure for
#'   retrospective triangles. If integer, divides columns evenly. If vector,
#'   must sum to number of columns minus 1. Default is 1 (standard triangular
#'   structure).
#'
#' @returns A numeric vector of dispersion parameters with length equal to
#'   one less than the number of columns in the reporting triangle, with each
#'   element representing the estimate of the dispersion parameter for each
#'   delay d, starting at delay d=1. Returns NULL if insufficient data is
#'   available for estimation.
#'
#' @export
#' @importFrom checkmate assert_integerish assert_numeric assert_function
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
#' # Estimate uncertainty parameters with defaults
#' dispersion <- estimate_uncertainty_parameters(triangle)
#'
#' # Estimate with custom parameters
#' dispersion <- estimate_uncertainty_parameters(
#'   triangle,
#'   n_delay = 5,
#'   n_retro = 3,
#'   max_delay = 3,
#'   k = 2
#' )
estimate_uncertainty_parameters <- function(
    reporting_triangle,
    n_delay = NULL,
    n_retro = 2,
    max_delay = ncol(reporting_triangle) - 1,
    delay_pmf = NULL,
    fun_to_aggregate = sum,
    k = 1,
    structure = 1) {
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
  assert_integerish(k, lower = 1, len = 1, any.missing = FALSE)
  if (length(structure) == 1) {
    assert_integerish(structure, lower = 1, len = 1, any.missing = FALSE)
  } else {
    assert_integerish(structure, lower = 1, any.missing = FALSE)
  }

  # Validate delay_pmf if provided
  if (!is.null(delay_pmf)) {
    assert_numeric(
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

  # Validate fun_to_aggregate
  assert_function(fun_to_aggregate)
  .validate_aggregation_function(fun_to_aggregate)

  # Step 1: Create retrospective snapshots by truncating the triangle
  trunc_rep_tri_list <- truncate_triangles(
    reporting_triangle = reporting_triangle,
    n = n_retro
  )

  # Step 2: Generate retrospective reporting triangles with structure
  reporting_triangle_list <- generate_triangles(
    trunc_rep_tri_list = trunc_rep_tri_list,
    structure = structure
  )

  # Calculate default n_delay if needed (minimum rows across retro triangles)
  if (use_default_n_delay) {
    n_delay <- min(sapply(reporting_triangle_list, nrow))
  }

  # Step 3: Generate point nowcasts
  pt_nowcast_mat_list <- generate_pt_nowcast_mat_list(
    reporting_triangle_list = reporting_triangle_list,
    max_delay = max_delay,
    n = n_delay,
    delay_pmf = delay_pmf
  )

  # Handle NULL return or all NULL elements from generate_pt_nowcast_mat_list
  if (is.null(pt_nowcast_mat_list) ||
    all(sapply(pt_nowcast_mat_list, is.null))) {
    cli_warn(
      message = c(
        "Insufficient data to generate point nowcasts",
        "i" = "Returning NULL for dispersion parameter"
      )
    )
    return(NULL)
  }

  # Step 4: Estimate dispersion parameters
  dispersion <- estimate_dispersion(
    pt_nowcast_mat_list = pt_nowcast_mat_list,
    trunc_rep_tri_list = trunc_rep_tri_list,
    reporting_triangle_list = reporting_triangle_list,
    n = n_retro,
    fun_to_aggregate = fun_to_aggregate,
    k = k
  )

  return(dispersion)
}
