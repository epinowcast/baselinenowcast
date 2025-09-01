#' Sample from negative binomial model given a set of predictions
#'
#' @param pred Vector of predictions.
#' @param uncertainty_params Vector of uncertainty parameters.
#' @importFrom stats rnbinom
#' @importFrom cli cli_abort
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
  if (!is.numeric(pred)) {
    cli_abort("`pred` must be numeric (vector or matrix).")
  }
  if (!is.numeric(uncertainty_params)) {
    cli_abort("`uncertainty_params` must be numeric (scalar or vector).")
  }
  n <- length(pred)
  if (!(length(uncertainty_params) %in% c(1L, n))) {
    cli_abort("`uncertainty_params` must have length 1 or match length(pred).")
  }
  if (!is.null(pred)) {
    sampled_pred <- rnbinom(
      n = length(pred),
      size = uncertainty_params,
      mu = as.numeric(pred)
    )
    if (is.matrix(pred)) {
      dim(sampled_pred) <- dim(pred)
      dimnames(sampled_pred) <- dimnames(pred)
    }
  } else {
    sampled_pred <- NULL
  }
  return(sampled_pred)
}
