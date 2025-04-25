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
