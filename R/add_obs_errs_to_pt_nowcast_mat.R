#' Add observation errors to nowcasts
#'
#' This function ingests a point nowcast matrix  and a vector of dispersion
#'     parameters. It generates a list of `n_draws` nowcast matrices.
#'
#' @param point_nowcast_matrix Matrix of both observations and point estimates
#'     of counts at each reference time and delay.
#' @param disp Vector of dispersion parameters indexed starting at delay = 1,
#' @importFrom checkmate assert_integerish
#' @param n_draws Integer indicating the number of expected observed reporting
#'    squares to generate. Default is `1000`.
#'
#' @returns `nowcast_matrix_list` List of `n_draws` matrices of probabilistic
#'     nowcast matrix draws.
#' @export
#'
#' @examples
#' point_nowcast_matrix <- matrix(
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
#' list_of_exp_obs_nowcast <- add_obs_errs_to_pt_nowcast_mat(
#'   point_nowcast_matrix = point_nowcast_matrix,
#'   disp = c(8, 1.4, 4),
#'   n_draws = 10
#' )
#' print(list_of_exp_obs_nowcast[[1]])
add_obs_errs_to_pt_nowcast_mat <- function(point_nowcast_matrix,
                                           disp,
                                           n_draws = 1000) {
  assert_integerish(n_draws, lower = 1)
  list_of_exp_obs_nowcasts <- lapply(1:n_draws, function(i) {
    return(add_obs_err_to_pt_nowcast_mat(point_nowcast_matrix, disp))
  })
  return(list_of_exp_obs_nowcasts)
}


#' Add observation error to a point nowcast
#'
#' The function ingests an estimate of a point nowcast, in the form of a
#'   reporting rectangle and a vector of dispersion parameters, and adds
#'   observation error to generate an expected observed nowcast
#' @inheritParams add_obs_errs_to_pt_nowcast_mat
#' @importFrom cli cli_abort
#' @importFrom checkmate assert_numeric
#'
#' @returns `nowcast_w_obs_error` Matrix containing the same upper left values
#'    as the `point_nowcast_matrix`, with the bottom right containing
#'    probabilistic draws assuming a negative binomial
#'    observation model with a mean given by the lower right entries in
#'    `point_nowcast_matrix` and dispersion parameters
#'    from `disp`
#' @export
#'
#' @examples
#' point_nowcast_matrix <- matrix(
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
#' exp_obs_nowcast <- add_obs_err_to_pt_nowcast_mat(
#'   point_nowcast_matrix = point_nowcast_matrix,
#'   disp = c(8, 1.4, 4)
#' )
#' print(exp_obs_nowcast)
add_obs_err_to_pt_nowcast_mat <- function(point_nowcast_matrix,
                                          disp) {
  if (anyNA(point_nowcast_matrix)) {
    cli_abort(message = c(
      "`point_nowcast_matrix` contains NA values. It should only contain ",
      "observations or point estimates of mean expected values."
    ))
  }
  nowcast_w_obs_error <- replace_lower_right_with_NA(point_nowcast_matrix)

  if (ncol(point_nowcast_matrix) - 1 != length(disp)) {
    cli_abort(
      message =
        c(
          "`disp` vector should be of length one less than the number ",
          "of columns in `point_nowcast_matrix`"
        )
    )
  }
  # Make sure dispersion values greater than 0
  sapply(disp, assert_numeric, lower = 1e-5)
  if (!is.matrix(point_nowcast_matrix)) {
    cli_abort(message = "`point_nowcast_matrix` is not a matrix.")
  }

  for (i in seq_along(disp)) {
    max_t <- nrow(point_nowcast_matrix)
    # Start at second column, move left to right
    mean_vals <- point_nowcast_matrix[(max_t - i + 1):max_t, i + 1]
    nowcast_w_obs_error[(max_t - i + 1):max_t, i + 1] <- rnbinom(
      n = length(mean_vals),
      size = disp[i],
      mu = mean_vals
    )
  }

  return(nowcast_w_obs_error)
}
