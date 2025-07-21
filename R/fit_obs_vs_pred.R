fit_obs_vs_pred <- function(obs,
                            pred,
                            density_function = dnbinom) {
  n_horizons <- ncol(obs)
  param_vector <- vector(length = n_horizons)


  for (i in seq_len(n_horizons)) {
    obs_this_horizon <- obs[, i]
    pred_this_horizon <- obs[, i]
    nllik <- function(disp_var) {
      nll <- -sum(
        density_function(obs_this_horizon,
          pred_this_horizon,
          disp_var,
          log = TRUE
        ),
        na.rm = TRUE
      )
      return(nll)
    }
    param_vector[i] <- optimize(nllik, c(0.1, 1000))$minimum
  }
  return(param_vector)
}
