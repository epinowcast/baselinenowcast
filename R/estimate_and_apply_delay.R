#' Estimate and apply delay from a reporting triangle
#'
#' @inheritParams estimate_delay
#' @param n_history_delay Integer indicating number of reference times used for
#'    delay estimation.
#'
#' @returns `pt_nowcast_matrix` Matrix of point nowcasts
#' @export
#'
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
                                     n_history_delay = round(1.5 * max_delay)) {
  delay_pmf <- estimate_delay(reporting_triangle,
    max_delay,
    n = n_history_delay
  )
  point_nowcast_matrix <- apply_delay(reporting_triangle, delay_pmf)

  return(point_nowcast_matrix)
}
