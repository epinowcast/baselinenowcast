#' Estimate the uncertainty in the nowcasts from a list of retrospective
#'   reporting triangles
#' This function takes in the latest reporting triangle, a list of
#'   retrospective reporting triangles, and the number of triangles to
#'   use for the uncertainty estimate and the number of observations to use for
#'   the delay estimate, and returns an estimate of the negative binomial
#'   dispersion for each delay. It does so by iteratively re-estimating the
#'   delay distribution for each retrospective reporting triangle, using it to
#'   compute a nowcast, and comparing the nowcasted value to those that are
#'   observed. This code was adapted from code written (under an MIT license)
#'   by the Karlsruhe Institute of Technology RESPINOW German Hospitalization
#'   Nowcasting Hub,
#'   Modified from: https://github.com/KITmetricslab/RESPINOW-Hub/blob/39e2b17bc79492b0aee4c0b615a1c8dbf978ef53/code/baseline/functions.R#L142 #nolint
#' @param triangle_latest Matrix of the latest incomplete reporting triangle
#'   to be nowcasted, with rows representing the time points of reference and
#'   columns representing the delays
#' @param list_of_retro_rts List where each item is a reporting triangle with
#'   the same number of columns as `triangle_latest`. The indices of the list
#'   are ordered from most to least recent (e.g. list_of_retro_rts[[1]] is the
#'   retrospective reporting triangle from one delay unit prior).
#' @param n_history_uncertainty Integer indicating the number of retrospective
#'   reporting triangles to be used in the estimate, always starting from the
#'   most recent reference time. This number must be less than or equal to the
#'   number of elements in `list_of_retro_rts`. Default is the number of
#'   elements in the list.
#' @param n_history_delay Integer indicating the number of reference dates to be
#'   used in the estimate of the reporting delay, always starting from the most
#'   recent reporting delay. This number must be less than or equal to the
#'   number of rows in each of the elements in the `list_of_retro_rts`. Default
#'   is the number of rows in each reporting triangle.
#' @importFrom cli cli_abort
#' @returns Vector of dispersion parameters, which will be one less than the
#'   number of columns in `triangle_latest`
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     65, 46, 21, 7,
#'     70, 40, 20, 5,
#'     80, 50, 10, 10,
#'     100, 40, 31, 20,
#'     95, 45, 21, NA,
#'     82, 42, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 7,
#'   byrow = TRUE
#' )

#' retro_rts <- generate_retrospective_data(triangle)
#' disp_params <- estimate_uncertainty(
#'   triangle_for_latest = triangle,
#'   list_of_retro_rts = retro_rts,
#'   n_history_uncertainty = 3,
#'   n_history_delay = 4
#' )
#' print(disp_params)
estimate_uncertainty2 <- function(
    triangle_latest,
    list_of_retro_rts,
    n_history_uncertainty = length(list_of_retro_rts),
    n_history_delay = nrow(list_of_retro_rts[[1]])) {
  # Check the inputs to make sure they are compatible
  .validate_est_uncertainty_inputs(
    triangle = triangle_for_latest,
    retro_rts = list_of_retro_rts,
    n_history_uncertainty = n_history_uncertainty,
    n_history_delay = n_history_delay
  )
  .validate_triangle(triangle_latest)
}
