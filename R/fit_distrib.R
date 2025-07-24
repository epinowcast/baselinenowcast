#' Fit observations vs predictions to an observation model
#'
#' @param obs Matrix of observations with retrospective nowcast dates as rows
#'    and horizons as columns.
#' @param pred Matrix of predictions with retrospective nowcast dates as rows
#'    and horizons as columns.
#' @param observation_model Character string indicating the choice of
#'   observation model to fit to the predicted nowcasts versus the
#'   observations. Default is `negative binomial`.
#' @importFrom stats dnorm dnbinom dgamma
#'
#' @returns Vector of parameters corresponding to the chosen
#'    `observation_model` of length of the number of columns in `obs` and
#'    `pred`, with each element corresponding to a horizon.
#' disp <- .fit_distrib(obs, pred, observation_model = "negative binomial")
.fit_distrib <- function(obs,
                         pred,
                         observation_model = "dnbinom") {
  n_horizons <- ncol(obs)
  param_vector <- vector(length = n_horizons)

  # convert to character
  if (!is.character(observation_model)) {
    observation_model <- as.character(observation_model)
  }
  for (i in seq_len(n_horizons)) {
    obs_this_horizon <- obs[, i]
    pred_this_horizon <- pred[, i]
    objective_func <- function(disp_var, ...) {
      if (observation_model %in% c(
        "dnbinom", "negative binomial", "neg_binom",
        "negative_binomial", "nbinom",
        "Negative Binomial", "Negative binomial"
      )) {
        nll <- -sum(
          dnbinom(
            x = obs_this_horizon,
            mu = pred_this_horizon,
            size = disp_var[1],
            log = TRUE
          ),
          na.rm = TRUE
        )
      } else if (observation_model %in% c(
        "dnorm", "Normal",
        "normal", "norm"
      )) {
        nll <- -sum(
          dnorm(
            x = obs_this_horizon,
            mean = pred_this_horizon,
            sd = disp_var[1],
            log = TRUE
          ),
          na.rm = TRUE
        )
      } else if (observation_model %in% c("dgamma", "Gamma", "gamma")) {
        nll <- -sum(
          dgamma(
            x = obs_this_horizon,
            shape = pred_this_horizon^2 / disp_var[1]^2,
            scale = disp_var[1]^2 / pred_this_horizon,
            log = TRUE
          ),
          na.rm = TRUE
        )
      }
      return(nll)
    }
    param_vector[i] <- optimize(objective_func, c(0.1, 1000))$minimum
  }
  return(param_vector)
}
