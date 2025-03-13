#' Add observation error to a point nowcast
#'
#' The function ingests an estimate of a point nowcast, in the form of a
#'   reporting rectangle nd a vector of dispersion parameters, and adds
#'   observation error to generate an expected observed nowcast
#'
#' @param nowcast Matrix containing both observed and estimated values of the
#'    reporting triangle. Bottom right entries will estimates generated from
#'    applying the delay distribution to the nowcast
#' @param disp Vector of dispersion parameters of a negative binomial, for
#'    delays from 1 through the maximu delay
#'
#' @returns Matrix containing the same upper left values as the `nowcast`
#'    input matrix, with the bottom right containing expected observed counts
#'    of values assuming a negative binomial observation model with a mean
#'    given by the lower right entries in `nowcast` and dispersion parameters
#'    from `disp`
#' @export
#'
#' @examples
#' point_nowcast <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, 17.8,
#'     80, 40, 23.2, 15.9,
#'     70, 35, 20.3, 19.9
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#'
#' exp_obs_nowcast <- add_obs_error_to_nowcast(
#'   nowcast = point_nowcast,
#'   disp = c(8, 1.4, 4)
#' )
#' print(exp_obs_nowcast)
add_obs_error_to_nowcast <- function(nowcast,
                                     disp) {
  nowcast_w_obs_error <- .replace_lower_right_with_NA(nowcast)

  for (i in seq_along(disp)) {
    max_t <- nrow(nowcast)
    max_d <- length(disp) + 1
    mean_vals <- nowcast[(max_t - i + 1):max_t, i + 1]
    nowcast_w_obs_error[(max_t - i - 1):max_t, i + 1] <- rnbinom(1,
      size = disp[i],
      mu = mean_vals
    )
  }

  return(nowcast_w_obs_error)
}
