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
  if (max_delay > ncol(reporting_triangle) - 1) {
    message(sprintf(
      "The maximum delay must be less than the number of columns in the reporting triangle. The maximum delay will be set to %d.", # nolint
      ncol(reporting_triangle) - 1
    ))
    max_delay <- ncol(reporting_triangle) - 1
    n_history_delay <- round(1.5 * max_delay)
  }

  delay_pmf <- estimate_delay(
    reporting_triangle,
    max_delay = max_delay,
    n = n_history_delay
  )

  # This is going to return a matrix that is truncated at the maximum delay
  # if the user specified a maximum delay less than the number of columns in
  # the reporting triangle -1
  trunc_reporting_triangle <- reporting_triangle[, 1:(max_delay + 1)]

  if (ncol(trunc_reporting_triangle) < ncol(reporting_triangle)) {
    message(sprintf(
      "Only the first %d delays are being nowcasted.",
      ncol(trunc_reporting_triangle)
    )) # nolint
  }

  point_nowcast_matrix <- apply_delay(trunc_reporting_triangle, delay_pmf)

  return(point_nowcast_matrix)
}
