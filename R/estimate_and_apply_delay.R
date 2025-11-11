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
#' triangle_mat <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' ref_dates <- seq(
#'   from = as.Date("2025-01-01"),
#'   by = "day",
#'   length.out = nrow(triangle_mat)
#' )
#' triangle <- as_reporting_triangle(
#'   data = triangle_mat,
#'   reference_dates = ref_dates
#' ) |>
#'   truncate_to_delay(max_delay = 3)
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   n = 4
#' )
#' pt_nowcast_matrix
estimate_and_apply_delay <- function(reporting_triangle,
                                     ...) {
  assert_reporting_triangle(reporting_triangle)

  delay_pmf <- estimate_delay(
    reporting_triangle,
    ...
  )

  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf)

  return(point_nowcast_matrix)
}
