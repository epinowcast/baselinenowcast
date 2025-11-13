#' Estimate and apply delay from a reporting triangle
#'
#' @inheritParams estimate_delay
#' @param ... Additional arguments passed to the `estimate_delay` function
#'
#' @returns `pt_nowcast_matrix` A `reporting_triangle` object of point nowcasts
#'   with the same structure as the input
#' @family workflow_wrappers
#' @export
#' @importFrom cli cli_alert_info
#' @examples
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = example_reporting_triangle
#' )
#' pt_nowcast_matrix
estimate_and_apply_delay <- function(reporting_triangle,
                                     validate = TRUE,
                                     ...) {
  assert_reporting_triangle(reporting_triangle, validate)

  delay_pmf <- estimate_delay(
    reporting_triangle,
    validate = FALSE,
    ...
  )

  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf,
                                      validate = FALSE)

  return(point_nowcast_matrix)
}
