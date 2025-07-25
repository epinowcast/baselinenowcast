#' Fit observations vs predictions to an observation model
#'
#' @param pred Matrix or vector of predictions.
#' @param uncertainty_params Vector of uncertainty parameters.
#' @param observation_model_name Character string indicating the choice of
#'   observation model to sample to the predicted nowcasts.
#'   Default is `"negative binomial"`.
#' @importFrom stats rnorm rnbinom rgamma
#'
#' @returns `sampled_pred` Object of the same dimensions as `pred` representing
#'    a single draw from the `observation_model_name` distribution
#'    with the specified `uncertainty params`.
#' @examples
#' pred <- matrix(c(
#'   3.7, 6.1,
#'   5.2, 10.4
#' ), byrow = 2, nrow = 2)
#' sampled_preds <- sample_distribution(pred,
#'   uncertainty_params = c(50, 100),
#'   observation_model_name = "negative binomial"
#' )
#' sampled_preds
sample_distribution <- function(pred,
                                uncertainty_params,
                                observation_model_name = "dnbinom") {
  # convert to character
  if (!is.character(observation_model_name)) {
    observation_model <- as.character(observation_model_name)
  }

  if (observation_model_name %in% c(
    "rnbinom", "negative binomial", "neg_binom",
    "negative_binomial", "nbinom",
    "Negative Binomial", "Negative binomial"
  )) {
    sampled_pred <- rnbinom(
      n = length(pred),
      size = uncertainty_params,
      mu = pred
    )
  } else if (observation_model_name %in% c(
    "rnorm", "Normal",
    "normal", "norm"
  )) {
    sampled_pred <- rnorm(
      n = length(pred),
      mean = pred,
      sd = uncertainty_params
    )
  } else if (observation_model_name %in% c("rgamma", "Gamma", "gamma")) {
    sampled_pred <- rgamma(
      n = length(pred),
      shape = pred^2 / uncertainty_params^2,
      scale = uncertainty_params^2 / pred
    )
  }
  return(sampled_pred)
}
