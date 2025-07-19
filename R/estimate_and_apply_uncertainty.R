#' Estimate and apply uncertainty to a point nowcast matrix
#'
#' @inheritParams estimate_delay
#' @inheritParams sample_nowcasts
#' @param n_retrospective_nowcasts Integer indicating the number of reference
#'    times to use as retrospective nowcast time. Default is 1.5* the maximum
#'    delay
#' @param error_model Character string indicating what error model to use.
#'    Default is `"negative binomial"`
#' @param aggregator Function to aggregate predictions for error estimation.
#'    Default is `sum`.
#' @param ... Additional arguments
#'
#' @returns `nowcast_draws_df` Dataframe containing draws of combined
#'    observations and probabilistic predictions at each reference time.
#' @export
#'
#' @examples
#' triangle <- matrix(
#'   c(
#'     80, 50, 25, 10,
#'     100, 50, 30, 20,
#'     90, 45, 25, NA,
#'     80, 40, NA, NA,
#'     70, NA, NA, NA
#'   ),
#'   nrow = 5,
#'   byrow = TRUE
#' )
#' pt_nowcast_matrix <- estimate_and_apply_delay(
#'   reporting_triangle = triangle,
#'   max_delay = 3,
#'   n = 4
#' )
#' nowcast_draws_df <- estimate_and_apply_uncertainty(
#'   pt_nowcast_matrix,
#'   triangle,
#'   max_delay,
#'   n_retrospective_nowcasts = 1
#' )
#' nowcast_draws_df
estimate_and_apply_uncertainty <- function(point_nowcast_matrix,
                                           reporting_triangle,
                                           max_delay,
                                           n_retrospective_nowcasts =
                                             round(1.5 * max_delay),
                                           draws = 100,
                                           error_model = "negative binomial",
                                           aggregator = sum,
                                           ...) {
  trunc_rep_tris <- truncate_triangles(reporting_triangle,
    n = n_retrospective_nowcasts
  )

  retro_rep_tris <- construct_triangles(trunc_rep_tris)

  retro_pt_nowcasts <- fill_triangles(retro_rep_tris)

  disp <- estimate_uncertainty(
    retro_pt_nowcasts,
    trunc_rep_tris,
    retro_rep_tris
  )
  nowcast_draws <- sample_nowcasts(
    point_nowcast_matrix,
    reporting_triangle,
    disp,
    draws
  )
  return(nowcast_draws)
}
