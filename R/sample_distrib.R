#' Sample from negative binomial model given a set of predictions
#'
#' @param pred Vector of predictions.
#' @param uncertainty_params Vector of uncertainty parameters.
#' @importFrom stats rnbinom
#' @export
#' @returns `sampled_pred` Object of the same dimensions as `pred` representing
#'    a single draw from the negative binomial distribution
#'    with the specified `uncertainty params`.
#' @examples
#' pred <- c(3.2, 4.6)
#' sampled_preds <- sample_nb(pred,
#'   uncertainty_params = c(50, 100)
#' )
#' sampled_preds
sample_nb <- function(pred, uncertainty_params) {
  if (!is.null(pred)) {
    sampled_pred <- rnbinom(
      n = length(pred),
      size = uncertainty_params,
      mu = pred
    )
  } else {
    sampled_pred <- NULL
  }
  return(sampled_pred)
}
