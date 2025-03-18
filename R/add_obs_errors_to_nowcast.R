#' Add observation error to a point nowcast
#'
#' The function ingests an estimate of a point nowcast, in the form of a
#'   reporting rectangle and a vector of dispersion parameters, and adds
#'   observation error to generate an expected observed nowcast
#'
#' @param comp_rep_square Matrix containing both observed and point estimated
#'    values of the reporting triangle, i.e. the complete
#'    reporting square
#' @param disp Vector of dispersion parameters of a negative binomial, for
#'    delays from 1 through the maximum delay
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_numeric
#'
#' @returns `nowcast_w_obs_error` Matrix containing the same upper left values
#'    as the `comp_rep_square` input matrix, with the bottom right containing
#'    expected observed counts of values assuming a negative binomial
#'    observation model with a mean given by the lower right entries in
#'    `comp_rep_square` and dispersion parameters
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
#'   comp_rep_square = point_nowcast,
#'   disp = c(8, 1.4, 4)
#' )
#' print(exp_obs_nowcast)
add_obs_error_to_nowcast <- function(comp_rep_square,
                                     disp) {
  if (anyNA(comp_rep_square)) {
    cli_abort(message = c(
      "`comp_rep_square` contains NA values. It should only contain ",
      "observations or point estimates of mean expected values."
    ))
  }
  nowcast_w_obs_error <- .replace_lower_right_with_NA(comp_rep_square)

  if (ncol(comp_rep_square) - 1 != length(disp)) {
    cli_abort(
      message =
        c(
          "`disp` vector should be of length one less than the number ",
          "of columns in `comp_rep_square`"
        )
    )
  }
  # Make sure dispersion values greater than 0
  sapply(disp, assert_numeric, lower = 1e-5)

  for (i in seq_along(disp)) {
    max_t <- nrow(comp_rep_square)
    # Start at second column, move left to right
    mean_vals <- comp_rep_square[(max_t - i + 1):max_t, i + 1]
    nowcast_w_obs_error[(max_t - i + 1):max_t, i + 1] <- rnbinom(
      n = length(mean_vals),
      size = disp[i],
      mu = mean_vals
    )
  }

  return(nowcast_w_obs_error)
}
