#' Estimate and apply delay from a reporting triangle
#'
#' This function generates a point nowcast by estimating a delay distribution
#'   from the reporting triangle and applying it to complete the triangle. If a
#'   delay distribution is specified, this will be used to generate the
#'   nowcast, otherwise, a delay distribution will be estimated from the
#'   `reporting_triangle`.
#'
#' @inheritParams estimate_delay
#' @inheritParams assert_reporting_triangle
#' @param delay_pmf Vector of delays assumed to be indexed
#'    starting at the first delay column in `reporting_triangle`.
#'    Default is `NULL`, which will estimate a delay from the
#'    `reporting_triangle`.
#' @param ... Additional arguments passed to the `estimate_delay` function
#'
#' @returns `pt_nowcast_matrix` A `reporting_triangle` object of point nowcasts
#'   with the same structure as the input
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_abort
#' @examples
#' # Estimate and apply delay using default parameters
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' pt_nowcast_matrix
#'
#' # Use downward correction example with specific rows for delay estimation
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_downward_corr_rt,
#'   n = 5
#' )
#' pt_nowcast_matrix
#'
#' # Provide a pre-computed delay PMF
#' delay_pmf <- estimate_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_reporting_triangle,
#'   delay_pmf = delay_pmf
#' )
#' pt_nowcast_matrix
estimate_and_apply_delay <- function(reporting_triangle,
                                     n = nrow(reporting_triangle),
                                     delay_pmf = NULL,
                                     validate = TRUE,
                                     ...) {
  assert_reporting_triangle(reporting_triangle, validate)

  if (n > nrow(reporting_triangle)) {
    cli_abort(
      message = c(
        "The number of observations (rows) specified for delay estimation ",
        "must be less than or equal to the number of rows of the reporting ",
        "triangle. Either remove the reporting triangles that do ",
        "not contain sufficient data, or lower `n`."
      )
    )
  }

  tri_mat <- tail(reporting_triangle, n = n)
  has_complete_row <- any(rowSums(is.na(tri_mat)) == 0)
  if (isFALSE(has_complete_row)) {
    cli_abort(
      message = c(
        "The rows used for delay estimation in the reporting triangle must ",
        "contain at least one row with no missing observations."
      )
    )
  }

  if (is.null(delay_pmf)) {
    delay_pmf <- estimate_delay(
      reporting_triangle = reporting_triangle,
      n = n,
      validate = FALSE,
      ...
    )
  }

  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf,
    validate = FALSE
  )

  return(point_nowcast_matrix)
}
