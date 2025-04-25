get_nowcast_pred_draw <- function(point_nowcast_pred_matrix,
                                  reporting_triangle,
                                  disp) {
  n_horizons <- length(disp)
  max_t <- nrow(reporting_triangle)
  mean_pred <- rowSums(point_nowcast_pred_matrix, na.rm = TRUE)[1:n_horizons]
  # Nowcast predictions only (these are reversed, first element is horizon 0)
  draw_pred <- rnbinom(n = n_horizons, size = rev(disp), mu = mean_pred)
  # Pad with 0s for the fully observed rows, which are before
  # the max_t - n_horizons
  draw_pred_long <- c(rep(0, max_t - n_horizons), draw_pred)
  return(draw_pred)
}
