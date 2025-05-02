#' Get a draw of only the predicted elements of the nowcast vector
#'
#' @param point_nowcast_pred_matrix Matrix containing only the elements that
#'    have not yet been observed as of the final reference date
#' @param disp Vector of dispersion parameters indexed by horizon from minus
#'    one to the maximum delay
#'
#' @returns Vector of predicted draws at each reference time
#' @export
#'
#' @examples
#' point_nowcast_pred_matrix <-
#'   matrix(
#'     c(
#'       NA, NA, NA, NA,
#'       NA, NA, NA, NA,
#'       NA, NA, NA, 16.8,
#'       NA, NA, 21.2, 19.5,
#'       NA, 34.5, 15.4, 9.1
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draw <- get_nowcast_pred_draw(
#'   point_nowcast_pred_matrix,
#'   disp
#' )
#' nowcast_pred_draw
get_nowcast_pred_draw <- function(point_nowcast_pred_matrix,
                                  disp) {
  n_horizons <- length(disp)
  max_t <- nrow(point_nowcast_pred_matrix)
  mean_pred <- rowSums(point_nowcast_pred_matrix, na.rm = TRUE)[1:n_horizons]
  # Nowcast predictions only (these are reversed, first element is horizon 0)
  draw_pred <- rnbinom(n = n_horizons, size = rev(disp), mu = mean_pred)
  # Pad with 0s for the fully observed rows, which are before
  # the max_t - n_horizons
  draw_pred_long <- c(rep(0, max_t - n_horizons), draw_pred)
  return(draw_pred)
}

#' Get a dataframe of multiple draws of only the predicted elements of the
#'    nowcast vector
#'
#' @param point_nowcast_pred_matrix Matrix containing only the elements that
#'    have not yet been observed as of the final reference date
#' @param disp Vector of dispersion parameters indexed by horizon from minus
#'    one to the maximum delay
#' @param n_draws Integer indicating the number of draws of the predicted
#'    nowcast vector to generate. Default is `1000`.
#'
#' @returns Dataframe containing the predicted point nowcast vectors indexed by
#'    reference time (`pred_count`), reference time (`time`), and the draw index
#'    (`draw`).
#' @export
#'
#' @examples
#' point_nowcast_pred_matrix <-
#'   matrix(
#'     c(
#'       NA, NA, NA, NA,
#'       NA, NA, NA, NA,
#'       NA, NA, NA, 16.8,
#'       NA, NA, 21.2, 19.5,
#'       NA, 34.5, 15.4, 9.1
#'     ),
#'     nrow = 4,
#'     byrow = TRUE
#'   )
#' disp <- c(0.8, 12.4, 9.1)
#' nowcast_pred_draws_df <- get_nowcast_pred_draw(
#'   point_nowcast_pred_matrix,
#'   disp,
#'   500
#' )
#' nowcast_pred_draws_df
get_nowcast_pred_draws <- function(point_nowcast_pred_matrix,
                                   disp,
                                   n_draws = 1000) {
  assert_integerish(n_draws, lower = 1)

  for (i in 1:n_draws) {
    pred_nowcast_vec <- get_nowcast_pred_draw(
      point_nowcast_pred_matrix,
      disp
    )
    df_i <- data.frame(
      pred_count = pred_nowcast_vec,
      time = 1:length(pred_nowcast_vec),
      draw = i
    )
    if (i == 1) {
      df_pred_draws <- df_i
    } else {
      df_pred_draws <- rbind(df_pred_draws, df_i)
    }
  }

  return(df_pred_draws)
}
