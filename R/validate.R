#' Validate triangle
#' Various checks to make sure that the reporting triangle passed in to
#'   [get_delay_estimate()] is formatted properly.
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @inheritParams get_delay_estimate
#' @returns NULL, invisibly
#' @keywords internal
.validate_triangle <- function(triangle,
                               max_delay = ncol(triangle) - 1,
                               n_history_delay = nrow(triangle)) {
  # Make sure the input triangle only contains integer values
  # and is of the correct class
  assert_class(triangle, "matrix")
  assert_integerish(triangle)
  assert_integerish(max_delay)
  assert_integerish(n_history_delay)
  assert_matrix(triangle, all.missing = FALSE)

  if (nrow(triangle) <= ncol(triangle)) {
    cli_abort(
      message =
        "Number of observations must be greater than the maximum delay"
    )
  }

  if (nrow(triangle) < n_history_delay) {
    cli_abort(
      message = c(
        "Number of observations in input data not sufficient for",
        "user specified number of historical observations to use",
        "for estimaton."
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

  if ((max_delay < 1 || n_history_delay < 1)) {
    cli_abort(
      message = c(
        "Insufficient `max_delay` or `n_history_delay`, must be greater than ",
        "or equal to 1."
      )
    )
  }
}

#' Validate triangle to nowcast and delay pmf together
#' Various checks to make sure that the reporting triangle  and the delay pmf
#'   passed in to [apply_delay()] are formatted properly and compatible
#' @param triangle Matrix of values with rows indicating the time points and
#'   columns indicating the delays
#' @param delay_pmf Vector of length of the number of delays indicating the
#'   probability of a case being reported on a given delay
#' @importFrom checkmate assert_class
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_matrix
#' @importFrom cli cli_abort
#' @returns NULL, invisibly
#' @keywords internal
.validate_delay_and_triangle <- function(triangle,
                                         delay_pmf) {
  # Check that the input triangle only contains integer values
  assert_integerish(triangle)
  # Check that the inputs are the correct type
  assert_class(triangle, "matrix")
  assert_class(delay_pmf, "numeric")
  assert_matrix(triangle, all.missing = FALSE)


  # Make sure the triangle has the same number of colums as the delay
  if ((ncol(triangle) != length(delay_pmf))) {
    cli_abort(
      message = c(
        "Length of the delay pmf is not the same as the number of delays ",
        "in the triangle to be nowcasted. Ensure that these are equivalent ",
        "by generating the delay pmf using the same maximum delay as in the ",
        "data you want to be nowcasted."
      )
    )
  }
}

#' Validate inputs use to generate retrospective data
#' Various checks to make sure that the reporting triangle and specified
#'   integers passed in to [generate_retrospective_data()] are formatted
#'   properly
#' @importFrom checkmate assert_integerish
#' @importFrom checkmate assert_number
#' @importFrom cli cli_abort
#' @inheritParams generate_retrospective_data
#' @returns NULL, invisibly
#' @keywords internal
.validate_retro_inputs <- function(triangle,
                                   n_history_uncertainty,
                                   n_history_delay) {
  assert_integerish(n_history_delay)
  assert_integerish(n_history_uncertainty)
  assert_number(n_history_uncertainty, lower = 0, finite = TRUE)
  assert_number(n_history_delay, lower = 0, finite = TRUE)

  if ((n_history_uncertainty + n_history_delay) > nrow(triangle)) {
    cli_abort(
      message = c(
        "Triangle to nowcast does not contain sufficient rows to ",
        "estimate uncertainty from `n_history_uncertainty` reporting ",
        "triangles and `n_history_delay` rows of the reporting triangle. ",
        "Either pass in a triangle of more rows or lower the ",
        "`n_history_uncertainty`"
      )
    )
  }

  if (n_history_delay < ncol(triangle)) {
    cli_abort(
      message = c(
        "Specified number of observations to use to estimate the delay ",
        "`n_history_delay` must be greater than the number of columns in ",
        " the reporting triangle"
      )
    )
  }
}

.validate_est_uncertainty_inputs <- function(triangle,
                                             retro_rts,
                                             n_history_uncertainty,
                                             n_history_delay) {
  assert_integerish(n_history_delay)
  assert_integerish(n_history_uncertainty)
  assert_number(n_history_uncertainty, lower = 0, finite = TRUE)
  assert_number(n_history_delay, lower = 0, finite = TRUE)

  # Check that specified historical values for estimation are possible
  if (n_history_uncertainty > length(retro_rts)) {
    cli_abort(
      message = c(
        "Specified nuber of reporting triangles used to estimate the ",
        "uncertainty is greater than the number of reporting triangles ",
        "passed in."
      )
    )
  }

  # Check that specified historical values for estimation are possible
  if (n_history_delay > nrows(retro_rts[[1]]) ||
    n_history_delay > nrows(retro_rts[[length(retro_rts)]])) {
    cli_abort(
      message = c(
        "Specified nuber of observations used to estimate the ",
        "delay is greater than the number of rows in the reporting triangles ",
        "passed in."
      )
    )
  }

  # Check that the number of columns in the latest triangle and the retro
  # triangles are the same
  if (ncol(triangle_latest) != ncol(retro_rts[[1]]) ||
    ncol(triangle_latest) != ncol(retro_rts[[length(retro_rts)]])) {
    cli_abort(
      message = c(
        "The number of columns, which is assumed to be one more than the ",
        "maximum delay, is not equivalent in the latest reporting triangle ",
        "and the retrospective reporting triangles"
      )
    )
  }

  if ((n_history_uncertainty + n_history_delay) > nrow(triangle - 1)) {
    cli_abort(
      message = c(
        "Triangle to nowcast does not contain sufficient rows to ",
        "estimate uncertainty from `n_history_dispersion` observations. Either",
        "pass in a triangle of more rows or lower the `n_history_dispersion`"
      )
    )
  }
}
