#' Estimate and apply delay from a reporting triangle
#'
#' @inheritParams estimate_delay
#' @param ... Additional arguments passed to the `estimate_delay` function
#'
#' @returns `pt_nowcast_matrix` Matrix of point nowcasts
#' @export
#' @importFrom cli cli_alert_info
#' @examples
#' triangle <- matrix(
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
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   max_delay = 3,
#'   n = 4
#' )
#' pt_nowcast_matrix
estimate_and_apply_delay <- function(reporting_triangle,
                                     max_delay = ncol(reporting_triangle) - 1,
                                     ...) {
  reporting_triangle <- .check_to_filter_to_max_delay(
    reporting_triangle,
    max_delay
  )

  delay_pmf <- estimate_delay(
    reporting_triangle,
    max_delay = max_delay,
    ...
  )

  # This is going to return a matrix that is truncated at the maximum delay
  # if the user specified a maximum delay less than the number of columns in
  # the reporting triangle -1
  trunc_reporting_triangle <- reporting_triangle[, 1:(max_delay + 1)]

  if (ncol(trunc_reporting_triangle) < ncol(reporting_triangle)) {
    cli_alert_info(
      text = "Only the first {ncol(trunc_reporting_triangle)} delays are being nowcasted." # nolint
    )
  }

  point_nowcast_matrix <- apply_delay(trunc_reporting_triangle, delay_pmf)

  return(point_nowcast_matrix)
}
