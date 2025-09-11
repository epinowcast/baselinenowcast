#' Validate triangle
#' Various checks to make sure that the reporting triangle passed in to
#'   [estimate_delay()] is formatted properly.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @inheritParams estimate_delay
#' @returns NULL, invisibly
#' @keywords internal
.validate_triangle <- function(
    triangle,
    max_delay = ncol(triangle) - 1,
    n = nrow(triangle)) {
  # Make sure the input triangle is of the correct class, and n and max_delay
  # are integers
  assert_class(triangle, "matrix")
  assert_integerish(max_delay)
  assert_integerish(n)
  assert_matrix(triangle, all.missing = FALSE)

  # Check if the triangle has a valid structure
  # Ensure each column has at least one non-NA value
  if (any(colSums(!is.na(triangle)) == 0)) {
    cli_abort(
      message = c(
        "Invalid reporting triangle structure. Each column must have",
        "at least one non-NA value."
      )
    )
  }

  # For ragged triangles (e.g. weekly reporting of daily data),
  # we need to ensure the triangle has proper structure
  if (!.check_na_bottom_right(triangle)) {
    cli_abort(
      message = c(
        "Invalid reporting triangle structure. NA values should only",
        "appear in the bottom right portion of the triangle."
      )
    )
  }

  if (nrow(triangle) < n) {
    cli_abort(
      message = c(
        "Number of observations in input reporting triangle is insufficient",
        "for the user specified number of historical observations to use",
        "for delay estimaton."
      )
    )
  }

  if (ncol(triangle) < (max_delay + 1)) {
    cli_abort(
      message = c(
        "Number of delays in input data not sufficient for",
        "user specified maximum delay"
      )
    )
  }

  if ((max_delay < 1 || n < 1)) {
    cli_abort(
      message = c(
        "Insufficient `max_delay` or `n`, must be greater than ",
        "or equal to 1."
      )
    )
  }

  if (!.check_na_bottom_right(triangle)) {
    cli_abort(
      message = c(
        "Reporting triangle contains NA values in elements other than ",
        "the bottom right of the matrix. Cannot produce nowcasts from this ",
        "triangle."
      )
    )
  }

  n_rows <- nrow(triangle)
  has_complete_row <- any(
    rowSums(is.na(triangle[(n_rows - n + 1):n_rows, ])) == 0
  )
  if (isFALSE(has_complete_row)) {
    cli_abort(
      message = c(
        "The rows used for delay estimation in the reporting triangle must ",
        "contain at least one row with no missing observations. Consider ",
        "increasing `n` to ensure a complete row of the reporting triangle is ",
        "being used for delay estimation."
      )
    )
  }

  if (isFALSE(.check_lhs_not_only_zeros(triangle[(n_rows - n + 1):n_rows, ]))) { # nolint
    cli_abort(
      message = c(
        "The values for the recent reference times and delays only contain 0s,",
        "which means the method to iteratively complete the reporting triangle",
        "and estimate a delay PMF will be invalid. Consider increasing `n` to ",
        "capture reference times that contain observations for early delays",
        "or truncating to an earlier reference time to ensure a nowcast, ",
        "not a forecast, is being produced. "
      )
    )
  }
  first_na <- which(is.na(triangle[nrow(triangle), ]))[1]
  if (!is.na(first_na) && first_na == 1) {
    cli_abort(
      message = c(
        "The last row of the reporting triangle contains an NA in the first",
        "column. There must be at least an entry for the first delay at the",
        "latest reference time."
      )
    )
  }

  return(NULL)
}

#' Validate triangle to nowcast and delay PMF together
#' Various checks to make sure that the reporting triangle  and the delay PMF
#'   passed in to [apply_delay()] are formatted properly and compatible.
#' @param triangle Matrix of values with rows indicating the time points and
#'   columns indicating the delays.
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @returns NULL, invisibly
#' @keywords internal
.validate_delay_and_triangle <- function(triangle, delay_pmf) {
  # Check that the inputs are the correct type
  assert_class(triangle, "matrix")
  assert_class(delay_pmf, "numeric")
  assert_matrix(triangle, all.missing = FALSE)

  # Make sure the triangle has the same number of columns as the delay
  if ((ncol(triangle) != length(delay_pmf))) {
    cli_abort(
      message = c(
        "Length of the delay PMF is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay PMF using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }

  # Make sure that nowcasts can be generated iteratively, ensuring delay_pmf[1]
  # !=0 while the triangle is a reporting triangle (NAs all in bottom right)
  if (is.na(triangle[nrow(triangle), 2]) && delay_pmf[1] == 0) {
    cli_abort(
      message = c(
        "Value of delay PMF at delay = 0 is 0, and the latest reference time ",
        "in the reporting matrix only contains a value at delay = 0. There is",
        "insufficient information to generate a point nowcast for the latest ",
        "reference time. Consider truncating to an earlier reference time to ",
        "ensure a nowcast, not a forecast, is being produced."
      )
    )
  }

  return(NULL)
}

#' Validate the inputs to `estimate_and_apply_uncertainty()` to ensure that
#'    the reporting triangle, point nowcast matrix, and specified maximum delay
#'    are correct.
#'
#' @inheritParams estimate_and_apply_uncertainty
#'
#' @returns NULL, invisibly
#' @keywords internal
.validate_multiple_inputs <- function(
    point_nowcast_matrix,
    reporting_triangle,
    max_delay) {
  # Basic input validation
  if (!is.matrix(point_nowcast_matrix)) {
    cli_abort("`point_nowcast_matrix` must be a matrix.")
  }
  if (!is.matrix(reporting_triangle)) {
    cli_abort("`reporting_triangle` must be a matrix.")
  }

  if (ncol(point_nowcast_matrix) != ncol(reporting_triangle)) {
    cli_abort(c(
      "x" = "`point_nowcast_matrix` and `reporting_triangle` must have the same number of columns.", # nolint
      "i" = "Got {ncol(point_nowcast_matrix)} and {ncol(reporting_triangle)} respectively." # nolint
    ))
  }

  if (ncol(reporting_triangle) != (max_delay + 1)) {
    cli_abort(c(
      "x" = "Inconsistent `max_delay`.",
      "i" = "`ncol(reporting_triangle)` = {ncol(reporting_triangle)} but `max_delay + 1` = {max_delay + 1}." # nolint
    ))
  }

  if (ncol(point_nowcast_matrix) != (max_delay + 1)) {
    cli_abort(c(
      "x" = "Inconsistent `max_delay`.",
      "i" = "`ncol(point_nowcast_matrix)` = {ncol(point_nowcast_matrix)} but `max_delay + 1` = {max_delay + 1}." # nolint
    ))
  }


  return(NULL)
}

#' Check observations and predictions are compatible
#'
#' @param obs Matrix or vector of observations.
#' @param pred Matrix or vector of predictions.
#'
#' @returns NULL, invisibly
.check_obs_and_pred <- function(obs, pred) {
  if (is.null(obs) || is.null(pred)) {
    cli_abort("Missing `obs` and/or `pred`") # nolint
  }
  # Coerce vectors/data.frames to matrices and validate numeric
  obs <- as.matrix(obs)
  pred <- as.matrix(pred)
  if (!is.numeric(obs) || !is.numeric(pred)) {
    cli_abort("`obs` and `pred` must be numeric (after coercion to matrix).")
  }

  # Ensure obs and pred have the same dimensions
  if (!identical(dim(obs), dim(pred))) {
    cli_abort("`obs` and `pred` must have the same dimensions") # nolint
  }
  return(NULL)
}
